-module(cs_customer_handler).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").

-export([handle_function/4]).

-spec handle_function(atom(), tuple(), woody:context(), map()) ->
    {ok, term()} | no_return().
handle_function(Function, Args, WoodyContext0, Options) ->
    DefaultDeadline = woody_deadline:from_timeout(default_handling_timeout(Options)),
    WoodyContext = ensure_woody_deadline_set(WoodyContext0, DefaultDeadline),
    do_handle_function(Function, Args, WoodyContext, Options).

%% CustomerManagement API

do_handle_function('Create', {Params}, _Context, _Options) ->
    #customer_CustomerParams{
        party_ref = PartyRef,
        contact_info = ContactInfo,
        metadata = Metadata
    } = Params,
    case cs_customer:create(PartyRef, ContactInfo, Metadata) of
        {ok, CustomerId} ->
            {ok, CustomerState} = cs_customer:get_state(CustomerId),
            {ok, cs_mapper:customer_to_thrift(maps:get(customer, CustomerState))};
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('Get', {CustomerId}, _Context, _Options) ->
    case cs_customer:get_state(CustomerId) of
        {ok, State} ->
            {ok, cs_mapper:customer_state_to_thrift(State)};
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('GetByParentPayment', {InvoiceId, PaymentId}, _Context, _Options) ->
    case cs_customer:get_by_payment(InvoiceId, PaymentId) of
        {ok, State} ->
            {ok, cs_mapper:customer_state_to_thrift(State)};
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, invalid_parent} ->
            woody_error:raise(business, #customer_InvalidRecurrentParent{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('Delete', {CustomerId}, _Context, _Options) ->
    case cs_customer:delete(CustomerId) of
        ok ->
            {ok, ok};
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('AddBankCard', {CustomerId, Params}, _Context, _Options) ->
    #customer_BankCardParams{
        bank_card_token = BankCardToken,
        card_mask = CardMask
    } = Params,
    case cs_customer:get(CustomerId) of
        {ok, Customer} ->
            PartyRef = json_to_party_ref(maps:get(party_ref, Customer)),
            case cs_bank_card:find_or_create(PartyRef, BankCardToken, CardMask) of
                {ok, BankCardId} ->
                    case cs_customer:add_bank_card(CustomerId, BankCardId) of
                        ok ->
                            {ok, BankCard} = cs_bank_card:get_with_tokens(BankCardId),
                            {ok, cs_mapper:bank_card_with_tokens_to_thrift(BankCard)};
                        {error, Reason} ->
                            woody_error:raise(system, {internal, Reason})
                    end;
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('RemoveBankCard', {CustomerId, BankCardId}, _Context, _Options) ->
    case cs_customer:get(CustomerId) of
        {ok, _} ->
            case cs_customer:remove_bank_card(CustomerId, BankCardId) of
                ok ->
                    {ok, ok};
                {error, not_found} ->
                    woody_error:raise(business, #customer_BankCardNotFound{});
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('AddPayment', {CustomerId, InvoiceId, PaymentId}, _Context, _Options) ->
    case cs_customer:get(CustomerId) of
        {ok, _} ->
            case cs_customer:add_payment(CustomerId, InvoiceId, PaymentId) of
                ok ->
                    {ok, ok};
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('GetPayments', {CustomerId, Limit, ContinuationToken}, _Context, _Options) ->
    case cs_customer:get(CustomerId) of
        {ok, _} ->
            Offset = cs_pagination:decode(ContinuationToken),
            case cs_customer:get_payments(CustomerId, Limit, Offset) of
                {ok, Payments, NextToken} ->
                    PaymentsThrift = [cs_mapper:payment_to_thrift(P) || P <- Payments],
                    {ok, #customer_CustomerPaymentsResponse{
                        payments = PaymentsThrift,
                        continuation_token = NextToken
                    }};
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('GetBankCards', {CustomerId, Limit, ContinuationToken}, _Context, _Options) ->
    case cs_customer:get(CustomerId) of
        {ok, _Customer} ->
            Offset = cs_pagination:decode(ContinuationToken),
            case cs_customer:get_bank_cards(CustomerId, Limit, Offset) of
                {ok, BankCardIds, NextToken} ->
                    BankCardsInfo = lists:filtermap(fun get_bank_card_info/1, BankCardIds),
                    {ok, #customer_CustomerBankCardsResponse{
                        bank_cards = BankCardsInfo,
                        continuation_token = NextToken
                    }};
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_CustomerNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end.

%% Internal functions

-spec default_handling_timeout(map()) -> non_neg_integer().
default_handling_timeout(#{default_handling_timeout := Timeout}) ->
    Timeout.

get_bank_card_info(BankCardId) ->
    case cs_bank_card:get_info(BankCardId) of
        {ok, BankCardInfo} -> {true, cs_mapper:bank_card_info_to_thrift(BankCardInfo)};
        _ -> false
    end.

-spec ensure_woody_deadline_set(woody:context(), woody:deadline()) -> woody:context().
ensure_woody_deadline_set(WoodyContext, DefaultDeadline) ->
    case woody_context:get_deadline(WoodyContext) of
        undefined ->
            woody_context:set_deadline(DefaultDeadline, WoodyContext);
        _ ->
            WoodyContext
    end.

json_to_party_ref(PartyRefJson) ->
    Type = {struct, struct, {dmsl_domain_thrift, 'PartyConfigRef'}},
    cs_json:json_to_term(cs_json:decode(PartyRefJson), Type).
