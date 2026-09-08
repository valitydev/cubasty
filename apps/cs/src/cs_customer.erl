-module(cs_customer).

-export([
    create/5,
    get/1,
    get_state/1,
    get_by_external_id/2,
    get_by_email/2,
    get_by_payment/2,
    find_or_create_by_email/2,
    normalize_email/1,
    delete/1,
    add_bank_card/2,
    remove_bank_card/2,
    get_bank_cards/3,
    add_payment/3,
    get_payments/3
]).

-export_type([customer_id/0, bank_card_id/0, party_ref/0, contact_info/0, metadata/0]).
-export_type([customer/0, customer_state/0, payment_ref/0, email/0]).

%% Reason recorded on affinities released together with the customer
-define(DELETED_REASON, <<"customer_deleted">>).
%% Longest address an SMTP envelope may carry (RFC 5321)
-define(MAX_EMAIL_SIZE, 320).

%% Types
-type customer_id() :: binary().
-type bank_card_id() :: binary().
-type party_ref() :: dmsl_domain_thrift:'PartyConfigRef'().
-type contact_info() :: dmsl_domain_thrift:'ContactInfo'() | undefined.
-type metadata() :: dmsl_domain_thrift:'Metadata'() | undefined.
-type email() :: binary().

-type customer() :: #{
    id := customer_id(),
    party_ref := binary(),
    contact_info => contact_info(),
    metadata => metadata(),
    created_at := binary(),
    deleted_at => binary() | undefined,
    external_id => binary() | undefined,
    email => email() | undefined
}.
-type customer_state() :: #{
    customer := customer(),
    bank_card_refs := [bank_card_id()],
    payment_refs := [payment_ref()]
}.
-type payment_ref() :: #{
    invoice_id := binary(),
    payment_id := binary(),
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

-spec get(customer_id()) -> {ok, customer()} | {error, not_found | term()}.
get(CustomerID) ->
    case cs_customer_database:get(CustomerID) of
        {ok, Customer} ->
            case maps:get(deleted_at, Customer) of
                undefined -> {ok, Customer};
                _ -> {error, not_found}
            end;
        Error ->
            Error
    end.

-spec get_state(customer_id()) -> {ok, customer_state()} | {error, not_found | term()}.
get_state(CustomerID) ->
    case ?MODULE:get(CustomerID) of
        {ok, Customer} ->
            {ok, BankCardIds, _} = cs_customer_database:get_bank_cards(CustomerID, 1000, 0),
            {ok, Payments, _} = cs_customer_database:get_payments(CustomerID, 1000, 0),
            PaymentRefs = [
                #{invoice_id => InvId, payment_id => PayId}
             || {InvId, PayId, _} <- Payments
            ],
            {ok, #{
                customer => Customer,
                bank_card_refs => BankCardIds,
                payment_refs => PaymentRefs
            }};
        Error ->
            Error
    end.

-spec get_by_external_id(binary(), party_ref()) ->
    {ok, customer_state()} | {error, not_found | term()}.
get_by_external_id(ExternalID, PartyRef) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_customer_database:get_by_external_id(ExternalID, PartyRefJson) of
        {ok, Customer} ->
            CustomerID = maps:get(id, Customer),
            get_state(CustomerID);
        Error ->
            Error
    end.

-spec get_by_email(party_ref(), email()) ->
    {ok, customer_state()} | {error, not_found | invalid_email | term()}.
get_by_email(PartyRef, Email) ->
    case normalize_email(Email) of
        {ok, NormalizedEmail} ->
            PartyRefJson = party_ref_to_json(PartyRef),
            case cs_customer_database:get_by_email(NormalizedEmail, PartyRefJson) of
                {ok, Customer} ->
                    get_state(maps:get(id, Customer));
                Error ->
                    Error
            end;
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

-spec get_by_payment(binary(), binary()) ->
    {ok, customer_state()} | {error, not_found | invalid_recurrent_parent | term()}.
get_by_payment(InvoiceId, PaymentId) ->
    case cs_customer_database:get_by_payment(InvoiceId, PaymentId) of
        {ok, Customer} ->
            CustomerID = maps:get(id, Customer),
            get_state(CustomerID);
        {error, not_found} ->
            {error, invalid_recurrent_parent};
        Error ->
            Error
    end.

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
    case cs_customer_database:delete(CustomerID) of
        ok ->
            %% Orphan affinities would outlive the customer and the freed email
            _ = release_terminal_affinities(CustomerID),
            ok;
        Error ->
            Error
    end.

-spec add_bank_card(customer_id(), bank_card_id()) -> ok | {error, customer_not_found | term()}.
add_bank_card(CustomerID, BankCardId) ->
    %% link_bank_card atomically links bank card and adds party_ref
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

-spec add_payment(customer_id(), binary(), binary()) -> ok | {error, term()}.
add_payment(CustomerID, InvoiceId, PaymentId) ->
    cs_customer_database:add_payment(CustomerID, InvoiceId, PaymentId).

-spec get_payments(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [map()], binary() | undefined} | {error, term()}.
get_payments(CustomerID, Limit, Offset) ->
    case cs_customer_database:get_payments(CustomerID, Limit, Offset) of
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

%% Internal functions

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

-spec release_terminal_affinities(customer_id()) -> ok.
release_terminal_affinities(CustomerID) ->
    case cs_terminal_affinity:release_by_customer(CustomerID, ?DELETED_REASON) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("failed to release terminal affinities of customer ~s: ~p", [
                CustomerID, Reason
            ]),
            ok
    end.

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
