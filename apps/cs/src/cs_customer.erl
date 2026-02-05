-module(cs_customer).

-export([
    create/3,
    get/1,
    get_state/1,
    get_by_payment/2,
    delete/1,
    add_bank_card/2,
    remove_bank_card/2,
    get_bank_cards/3,
    add_payment/3,
    get_payments/3
]).

-export_type([customer_id/0, bank_card_id/0, party_ref/0, contact_info/0, metadata/0]).
-export_type([customer/0, customer_state/0, payment_ref/0]).

%% Types
-type customer_id() :: binary().
-type bank_card_id() :: binary().
-type party_ref() :: dmsl_domain_thrift:'PartyConfigRef'().
-type contact_info() :: dmsl_domain_thrift:'ContactInfo'() | undefined.
-type metadata() :: dmsl_domain_thrift:'Metadata'() | undefined.

-type customer() :: #{
    id := customer_id(),
    party_ref := binary(),
    contact_info => contact_info(),
    metadata => metadata(),
    created_at := binary(),
    deleted_at => binary() | undefined
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

-spec create(party_ref(), contact_info(), metadata()) -> {ok, customer_id()} | {error, term()}.
create(PartyRef, ContactInfo, Metadata) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    cs_customer_database:create(PartyRefJson, ContactInfo, Metadata).

-spec get(customer_id()) -> {ok, customer()} | {error, not_found | term()}.
get(CustomerId) ->
    case cs_customer_database:get(CustomerId) of
        {ok, Customer} ->
            case maps:get(deleted_at, Customer) of
                undefined -> {ok, Customer};
                _ -> {error, not_found}
            end;
        Error ->
            Error
    end.

-spec get_state(customer_id()) -> {ok, customer_state()} | {error, not_found | term()}.
get_state(CustomerId) ->
    case ?MODULE:get(CustomerId) of
        {ok, Customer} ->
            {ok, BankCardIds, _} = cs_customer_database:get_bank_cards(CustomerId, 1000, 0),
            {ok, Payments, _} = cs_customer_database:get_payments(CustomerId, 1000, 0),
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

-spec get_by_payment(binary(), binary()) ->
    {ok, customer_state()} | {error, not_found | invalid_recurrent_parent | term()}.
get_by_payment(InvoiceId, PaymentId) ->
    case cs_customer_database:get_by_payment(InvoiceId, PaymentId) of
        {ok, Customer} ->
            CustomerId = maps:get(id, Customer),
            get_state(CustomerId);
        {error, not_found} ->
            {error, invalid_recurrent_parent};
        Error ->
            Error
    end.

-spec delete(customer_id()) -> ok | {error, not_found | term()}.
delete(CustomerId) ->
    cs_customer_database:delete(CustomerId).

-spec add_bank_card(customer_id(), bank_card_id()) -> ok | {error, customer_not_found | term()}.
add_bank_card(CustomerId, BankCardId) ->
    %% link_bank_card atomically links bank card and adds party_ref
    cs_customer_database:link_bank_card(CustomerId, BankCardId).

-spec remove_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
remove_bank_card(CustomerId, BankCardId) ->
    cs_customer_database:unlink_bank_card(CustomerId, BankCardId).

-spec get_bank_cards(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [bank_card_id()], binary() | undefined} | {error, term()}.
get_bank_cards(CustomerId, Limit, Offset) ->
    case cs_customer_database:get_bank_cards(CustomerId, Limit, Offset) of
        {ok, BankCardIds, Total} ->
            Token = make_continuation_token(Offset, length(BankCardIds), Total),
            {ok, BankCardIds, Token};
        Error ->
            Error
    end.

-spec add_payment(customer_id(), binary(), binary()) -> ok | {error, term()}.
add_payment(CustomerId, InvoiceId, PaymentId) ->
    cs_customer_database:add_payment(CustomerId, InvoiceId, PaymentId).

-spec get_payments(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [map()], binary() | undefined} | {error, term()}.
get_payments(CustomerId, Limit, Offset) ->
    case cs_customer_database:get_payments(CustomerId, Limit, Offset) of
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
