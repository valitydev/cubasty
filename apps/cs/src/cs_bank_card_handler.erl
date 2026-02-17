-module(cs_bank_card_handler).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([handle_function/4]).

-spec handle_function(atom(), tuple(), woody:context(), map()) ->
    {ok, term()} | no_return().
handle_function(Function, Args, WoodyContext0, Options) ->
    DefaultDeadline = woody_deadline:from_timeout(default_handling_timeout(Options)),
    WoodyContext = ensure_woody_deadline_set(WoodyContext0, DefaultDeadline),
    do_handle_function(Function, Args, WoodyContext, Options).

%% BankCardStorage API

do_handle_function('Get', {BankCardId}, _Context, _Options) ->
    case cs_bank_card:get_with_tokens(BankCardId) of
        {ok, BankCard} ->
            {ok, cs_mapper:bank_card_with_tokens_to_thrift(BankCard)};
        {error, not_found} ->
            woody_error:raise(business, #customer_BankCardNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('Find', {Params}, _Context, _Options) ->
    #customer_BankCardSearchParams{
        bank_card_token = BankCardToken,
        party_ref = PartyRef
    } = Params,
    case cs_bank_card:find_with_tokens(BankCardToken, PartyRef) of
        {ok, BankCard} ->
            {ok, cs_mapper:bank_card_with_tokens_to_thrift(BankCard)};
        {error, not_found} ->
            woody_error:raise(business, #customer_BankCardNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('Create', {PartyRef, Params}, _Context, _Options) ->
    #customer_BankCardParams{
        bank_card_token = BankCardToken,
        card_mask = CardMask
    } = Params,
    case cs_bank_card:create(PartyRef, BankCardToken, CardMask) of
        {ok, BankCardId} ->
            {ok, BankCard} = cs_bank_card:get_with_tokens(BankCardId),
            {ok, cs_mapper:bank_card_with_tokens_to_thrift(BankCard)};
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('GetRecurrentTokens', {BankCardId}, _Context, _Options) ->
    case cs_bank_card:get(BankCardId) of
        {ok, _} ->
            case cs_bank_card:get_recurrent_tokens(BankCardId) of
                {ok, Tokens} ->
                    TokensThrift = [cs_mapper:recurrent_token_to_thrift(T) || T <- Tokens],
                    {ok, TokensThrift};
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_BankCardNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('AddRecurrentToken', {Params}, _Context, _Options) ->
    #customer_RecurrentTokenParams{
        bank_card_id = BankCardId,
        provider_ref = ProviderRef,
        terminal_ref = TerminalRef,
        token = Token
    } = Params,
    case cs_bank_card:get(BankCardId) of
        {ok, _} ->
            case cs_bank_card:add_recurrent_token(BankCardId, ProviderRef, TerminalRef, Token) of
                {ok, RecurrentToken} ->
                    {ok, cs_mapper:recurrent_token_to_thrift(RecurrentToken)};
                {error, Reason} ->
                    woody_error:raise(system, {internal, Reason})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_BankCardNotFound{});
        {error, Reason} ->
            woody_error:raise(system, {internal, Reason})
    end;
do_handle_function('InvalidateRecurrentToken', {Params}, _Context, _Options) ->
    #customer_InvalidateRecurrentTokenParams{
        bank_card_id = BankCardId,
        key = #customer_ProviderTerminalKey{
            provider_ref = ProviderRef,
            terminal_ref = TerminalRef
        },
        reason = Reason
    } = Params,
    case cs_bank_card:get(BankCardId) of
        {ok, _} ->
            Result = cs_bank_card:invalidate_recurrent_token(
                BankCardId, ProviderRef, TerminalRef, Reason
            ),
            case Result of
                ok ->
                    {ok, ok};
                {error, not_found} ->
                    woody_error:raise(business, #customer_RecurrentTokenNotFound{});
                {error, Reason2} ->
                    woody_error:raise(system, {internal, Reason2})
            end;
        {error, not_found} ->
            woody_error:raise(business, #customer_BankCardNotFound{});
        {error, Reason2} ->
            woody_error:raise(system, {internal, Reason2})
    end.

%% Internal functions

-spec default_handling_timeout(map()) -> non_neg_integer().
default_handling_timeout(#{default_handling_timeout := Timeout}) ->
    Timeout.

-spec ensure_woody_deadline_set(woody:context(), woody:deadline()) -> woody:context().
ensure_woody_deadline_set(WoodyContext, DefaultDeadline) ->
    case woody_context:get_deadline(WoodyContext) of
        undefined ->
            woody_context:set_deadline(DefaultDeadline, WoodyContext);
        _ ->
            WoodyContext
    end.
