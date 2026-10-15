-module(cs_customer).

-export([
    create/5,
    exists/1,
    get/2,
    get_state/2,
    get_by_external_id/2,
    get_by_email/2,
    get_by_payment/3,
    get_by_withdrawal/2,
    find_or_create_by_email/2,
    find_or_create_by_external_id/2,
    add_external_id/3,
    normalize_email/1,
    delete/1,
    add_bank_card/2,
    remove_bank_card/2,
    get_bank_cards/3,
    add_payment/4,
    get_payments/4,
    add_withdrawal/3,
    get_withdrawals/4
]).

-export_type([customer_id/0, bank_card_id/0, party_ref/0, contact_info/0, metadata/0]).
-export_type([customer/0, customer_state/0, payment_ref/0, withdrawal_ref/0, email/0, external_id/0]).

%% Reason recorded on affinities released together with the customer
-define(DELETED_REASON, <<"customer_deleted">>).
%% Longest address an SMTP envelope may carry (RFC 5321)
-define(MAX_EMAIL_SIZE, 320).
%% Keeps (party_ref, external_id) well within what a btree index entry can hold
-define(MAX_EXTERNAL_ID_SIZE, 1024).

%% Types
-type customer_id() :: binary().
-type bank_card_id() :: binary().
-type party_ref() :: dmsl_domain_thrift:'PartyConfigRef'().
-type contact_info() :: dmsl_domain_thrift:'ContactInfo'() | undefined.
-type metadata() :: dmsl_domain_thrift:'Metadata'() | undefined.
-type email() :: binary().
-type external_id() :: binary().

%% A customer as seen from one of its parties
-type customer() :: #{
    id := customer_id(),
    %% the party it is seen from, in its stored form
    party_ref := binary(),
    contact_info => contact_info(),
    metadata => metadata(),
    created_at := binary(),
    deleted_at => binary() | undefined,
    %% external id in that party
    external_id => external_id() | undefined,
    email => email() | undefined
}.
-type customer_state() :: #{
    customer := customer(),
    bank_card_refs := [bank_card_id()],
    payment_refs := [payment_ref()],
    withdrawal_refs := [withdrawal_ref()]
}.
-type payment_ref() :: #{
    invoice_id := binary(),
    payment_id := binary(),
    created_at => binary()
}.
-type withdrawal_ref() :: #{
    withdrawal_id := binary(),
    created_at => binary()
}.

%% API

-spec create(party_ref(), contact_info(), metadata(), binary() | undefined, email() | undefined) ->
    {ok, customer_id()} | {error, external_id_conflict | email_conflict | invalid_email | term()}.
create(PartyRef, ContactInfo, Metadata, ExternalID, Email) ->
    case maybe_normalize_email(Email) of
        {ok, NormalizedEmail} ->
            PartyRefJson = party_ref_to_json(PartyRef),
            cs_customer_database:create(
                PartyRefJson, ContactInfo, Metadata, ExternalID, NormalizedEmail
            );
        {error, _} = Error ->
            Error
    end.

-spec exists(customer_id()) -> ok | {error, not_found | term()}.
exists(CustomerID) ->
    cs_customer_database:exists(CustomerID).

-spec get(customer_id(), party_ref()) -> {ok, customer()} | {error, not_found | term()}.
get(CustomerID, PartyRef) ->
    cs_customer_database:get(CustomerID, party_ref_to_json(PartyRef)).

-spec get_state(customer_id(), party_ref()) -> {ok, customer_state()} | {error, not_found | term()}.
get_state(CustomerID, PartyRef) ->
    with_state(?MODULE:get(CustomerID, PartyRef)).

-spec get_by_external_id(binary(), party_ref()) ->
    {ok, customer_state()} | {error, not_found | term()}.
get_by_external_id(ExternalID, PartyRef) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    with_state(cs_customer_database:get_by_external_id(ExternalID, PartyRefJson)).

-spec get_by_email(party_ref(), email()) ->
    {ok, customer_state()} | {error, not_found | invalid_email | term()}.
get_by_email(PartyRef, Email) ->
    case normalize_email(Email) of
        {ok, NormalizedEmail} ->
            PartyRefJson = party_ref_to_json(PartyRef),
            with_state(cs_customer_database:get_by_email(NormalizedEmail, PartyRefJson));
        {error, _} = Error ->
            Error
    end.

-spec find_or_create_by_email(party_ref(), email()) ->
    {ok, customer()} | {error, invalid_email | term()}.
find_or_create_by_email(PartyRef, Email) ->
    case normalize_email(Email) of
        {ok, NormalizedEmail} ->
            PartyRefJson = party_ref_to_json(PartyRef),
            cs_customer_database:find_or_create_by_email(PartyRefJson, NormalizedEmail);
        {error, _} = Error ->
            Error
    end.

-spec find_or_create_by_external_id(party_ref(), external_id()) ->
    {ok, customer()} | {error, invalid_external_id | term()}.
find_or_create_by_external_id(PartyRef, ExternalID) ->
    case validate_external_id(ExternalID) of
        ok ->
            PartyRefJson = party_ref_to_json(PartyRef),
            cs_customer_database:find_or_create_by_external_id(PartyRefJson, ExternalID);
        {error, _} = Error ->
            Error
    end.

-spec add_external_id(customer_id(), party_ref(), external_id()) ->
    ok
    | {error,
        not_found
        | invalid_external_id
        | {external_id_conflict, customer_id(), external_id()}
        | term()}.
add_external_id(CustomerID, PartyRef, ExternalID) ->
    case validate_external_id(ExternalID) of
        ok ->
            PartyRefJson = party_ref_to_json(PartyRef),
            cs_customer_database:add_external_id(CustomerID, PartyRefJson, ExternalID);
        {error, _} = Error ->
            Error
    end.

-spec get_by_payment(binary(), binary(), party_ref()) ->
    {ok, customer_state()} | {error, not_found | invalid_recurrent_parent | term()}.
get_by_payment(InvoiceId, PaymentId, PartyRef) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_customer_database:get_by_payment(InvoiceId, PaymentId, PartyRefJson) of
        {error, not_found} ->
            case cs_customer_database:payment_customer_membership(InvoiceId, PaymentId, PartyRefJson) of
                %% The customer of the payment is not in the party
                {ok, not_member} -> {error, not_found};
                %% No payment, or one made in another party
                {ok, _} -> {error, invalid_recurrent_parent};
                {error, _} = Error -> Error
            end;
        Result ->
            with_state(Result)
    end.

-spec get_by_withdrawal(binary(), party_ref()) -> {ok, customer_state()} | {error, not_found | term()}.
get_by_withdrawal(WithdrawalID, PartyRef) ->
    with_state(cs_customer_database:get_by_withdrawal(WithdrawalID, party_ref_to_json(PartyRef))).

-spec normalize_email(email()) -> {ok, email()} | {error, invalid_email}.
normalize_email(Email) when is_binary(Email) ->
    try unicode:characters_to_binary(string:trim(string:lowercase(Email))) of
        Normalized when is_binary(Normalized) -> validate_email(Normalized);
        _ -> {error, invalid_email}
    catch
        %% string:lowercase/1 fails with badarg on malformed UTF-8
        _:_ -> {error, invalid_email}
    end;
normalize_email(_Email) ->
    {error, invalid_email}.

-spec delete(customer_id()) -> ok | {error, not_found | term()}.
delete(CustomerID) ->
    %% Orphan affinities would outlive the customer and the freed email, so they are
    %% released in the same transaction as the delete itself
    cs_customer_database:delete(CustomerID, ?DELETED_REASON).

-spec add_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
add_bank_card(CustomerID, BankCardId) ->
    %% link_bank_card atomically links bank card and grants it to the customer's parties
    cs_customer_database:link_bank_card(CustomerID, BankCardId).

-spec remove_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
remove_bank_card(CustomerID, BankCardId) ->
    cs_customer_database:unlink_bank_card(CustomerID, BankCardId).

-spec get_bank_cards(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [bank_card_id()], binary() | undefined} | {error, term()}.
get_bank_cards(CustomerID, Limit, Offset) ->
    case cs_customer_database:get_bank_cards(CustomerID, Limit, Offset) of
        {ok, BankCardIds, Total} ->
            Token = make_continuation_token(Offset, length(BankCardIds), Total),
            {ok, BankCardIds, Token};
        Error ->
            Error
    end.

-spec add_payment(customer_id(), party_ref(), binary(), binary()) -> ok | {error, not_found | term()}.
add_payment(CustomerID, PartyRef, InvoiceId, PaymentId) ->
    case cs_customer_database:member(CustomerID, party_ref_to_json(PartyRef)) of
        {ok, PartyID} -> cs_customer_database:add_payment(CustomerID, PartyID, InvoiceId, PaymentId);
        {error, _} = Error -> Error
    end.

-spec get_payments(customer_id(), party_ref(), non_neg_integer(), non_neg_integer()) ->
    {ok, [map()], binary() | undefined} | {error, not_found | term()}.
get_payments(CustomerID, PartyRef, Limit, Offset) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_customer_database:member(CustomerID, PartyRefJson) of
        {ok, _PartyID} -> do_get_payments(CustomerID, PartyRefJson, Limit, Offset);
        {error, _} = Error -> Error
    end.

do_get_payments(CustomerID, PartyRefJson, Limit, Offset) ->
    case cs_customer_database:get_payments(CustomerID, PartyRefJson, Limit, Offset) of
        {ok, Payments, Total} ->
            Token = make_continuation_token(Offset, length(Payments), Total),
            PaymentMaps = [
                #{
                    invoice_id => InvId,
                    payment_id => PayId,
                    created_at => CreatedAt
                }
             || {InvId, PayId, CreatedAt} <- Payments
            ],
            {ok, PaymentMaps, Token};
        Error ->
            Error
    end.

-spec add_withdrawal(customer_id(), party_ref(), binary()) ->
    ok | {error, not_found | withdrawal_of_other_customer | term()}.
add_withdrawal(CustomerID, PartyRef, WithdrawalID) ->
    case cs_customer_database:member(CustomerID, party_ref_to_json(PartyRef)) of
        {ok, PartyID} -> cs_customer_database:add_withdrawal(CustomerID, PartyID, WithdrawalID);
        {error, _} = Error -> Error
    end.

-spec get_withdrawals(customer_id(), party_ref(), non_neg_integer(), non_neg_integer()) ->
    {ok, [withdrawal_ref()], binary() | undefined} | {error, not_found | term()}.
get_withdrawals(CustomerID, PartyRef, Limit, Offset) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_customer_database:member(CustomerID, PartyRefJson) of
        {ok, _PartyID} -> do_get_withdrawals(CustomerID, PartyRefJson, Limit, Offset);
        {error, _} = Error -> Error
    end.

do_get_withdrawals(CustomerID, PartyRefJson, Limit, Offset) ->
    case cs_customer_database:get_withdrawals(CustomerID, PartyRefJson, Limit, Offset) of
        {ok, Withdrawals, Total} ->
            Token = make_continuation_token(Offset, length(Withdrawals), Total),
            WithdrawalMaps = [
                #{withdrawal_id => WithdrawalID, created_at => CreatedAt}
             || {WithdrawalID, CreatedAt} <- Withdrawals
            ],
            {ok, WithdrawalMaps, Token};
        Error ->
            Error
    end.

%% Internal functions

-spec with_state({ok, customer()} | {error, Reason}) -> {ok, customer_state()} | {error, Reason}.
with_state({ok, Customer}) ->
    CustomerID = maps:get(id, Customer),
    %% The party the customer is seen from; payments and withdrawals are those made there
    PartyRef = maps:get(party_ref, Customer),
    {ok, BankCardIds, _} = cs_customer_database:get_bank_cards(CustomerID, 1000, 0),
    {ok, Payments, _} = cs_customer_database:get_payments(CustomerID, PartyRef, 1000, 0),
    {ok, Withdrawals, _} = cs_customer_database:get_withdrawals(CustomerID, PartyRef, 1000, 0),
    PaymentRefs = [
        #{invoice_id => InvId, payment_id => PayId}
     || {InvId, PayId, _} <- Payments
    ],
    WithdrawalRefs = [#{withdrawal_id => WithdrawalID} || {WithdrawalID, _} <- Withdrawals],
    {ok, #{
        customer => Customer,
        bank_card_refs => BankCardIds,
        payment_refs => PaymentRefs,
        withdrawal_refs => WithdrawalRefs
    }};
with_state({error, _} = Error) ->
    Error.

-spec maybe_normalize_email(email() | undefined) -> {ok, email() | undefined} | {error, invalid_email}.
maybe_normalize_email(undefined) -> {ok, undefined};
maybe_normalize_email(Email) -> normalize_email(Email).

-spec validate_email(binary()) -> {ok, email()} | {error, invalid_email}.
validate_email(<<>>) ->
    {error, invalid_email};
%% Beyond the RFC 5321 envelope limit the value is not an address, and it also stops
%% fitting into idx_customer_email_party (btree rejects entries over ~2704 bytes)
validate_email(Email) when byte_size(Email) > ?MAX_EMAIL_SIZE ->
    {error, invalid_email};
validate_email(Email) ->
    case binary:match(Email, <<"@">>) of
        nomatch -> {error, invalid_email};
        _ -> validate_email_charset(Email)
    end.

%% Control characters never belong in an address, and a NUL byte cannot be stored in a
%% text column at all, so PostgreSQL would answer with an error instead of a conflict
-spec validate_email_charset(binary()) -> {ok, email()} | {error, invalid_email}.
validate_email_charset(Email) ->
    ControlChars = [<<Char>> || Char <- lists:seq(0, 31)] ++ [<<127>>],
    case binary:match(Email, ControlChars) of
        nomatch -> {ok, Email};
        _ -> {error, invalid_email}
    end.

%% External ids are opaque to the service, so only what PostgreSQL cannot store or index
%% is rejected: an error there would come instead of a conflict
-spec validate_external_id(external_id()) -> ok | {error, invalid_external_id}.
validate_external_id(<<>>) ->
    {error, invalid_external_id};
validate_external_id(ExternalID) when byte_size(ExternalID) > ?MAX_EXTERNAL_ID_SIZE ->
    {error, invalid_external_id};
validate_external_id(ExternalID) when is_binary(ExternalID) ->
    case unicode:characters_to_binary(ExternalID) of
        ExternalID ->
            case binary:match(ExternalID, <<0>>) of
                nomatch -> ok;
                _ -> {error, invalid_external_id}
            end;
        _ ->
            {error, invalid_external_id}
    end;
validate_external_id(_ExternalID) ->
    {error, invalid_external_id}.

-spec make_continuation_token(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    binary() | undefined.
make_continuation_token(Offset, Count, Total) ->
    NextOffset = Offset + Count,
    case NextOffset < Total of
        true -> cs_pagination:encode(NextOffset);
        false -> undefined
    end.

-spec party_ref_to_json(party_ref()) -> binary().
party_ref_to_json(PartyRef) ->
    Type = {struct, struct, {dmsl_domain_thrift, 'PartyConfigRef'}},
    cs_json:encode(cs_json:term_to_json(PartyRef, Type)).
