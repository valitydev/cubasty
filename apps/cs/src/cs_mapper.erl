-module(cs_mapper).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([
    customer_to_thrift/1,
    customer_state_to_thrift/1,
    bank_card_to_thrift/2,
    bank_card_with_tokens_to_thrift/1,
    bank_card_info_to_thrift/1,
    payment_to_thrift/1,
    recurrent_token_to_thrift/1
]).

-export_type([
    customer/0,
    customer_state/0,
    payment_ref/0,
    bank_card_with_tokens/0,
    bank_card_info/0,
    recurrent_token/0,
    tokens_map/0
]).

%% Types
-type customer() :: cs_customer:customer().
-type customer_state() :: cs_customer:customer_state().
-type payment_ref() :: cs_customer:payment_ref().
-type bank_card_with_tokens() :: cs_bank_card:bank_card_with_tokens().
-type bank_card_info() :: cs_bank_card:bank_card_info().
-type recurrent_token() :: cs_bank_card_database:recurrent_token().
-type tokens_map() :: #{
    dmsl_customer_thrift:'ProviderTerminalKey'() => dmsl_customer_thrift:'RecurrentToken'()
}.

%% API

-spec customer_to_thrift(customer()) -> dmsl_customer_thrift:'Customer'().
customer_to_thrift(Customer) ->
    #customer_Customer{
        id = maps:get(id, Customer),
        party_ref = binary_to_party_ref(maps:get(party_ref, Customer)),
        created_at = format_timestamp_required(maps:get(created_at, Customer)),
        status = customer_status(Customer),
        contact_info = maps:get(contact_info, Customer, undefined),
        metadata = maps:get(metadata, Customer, undefined)
    }.

-spec customer_state_to_thrift(customer_state()) -> dmsl_customer_thrift:'CustomerState'().
customer_state_to_thrift(State) ->
    Customer = maps:get(customer, State),
    BankCardIds = maps:get(bank_card_refs, State, []),
    PaymentRefs = maps:get(payment_refs, State, []),
    #customer_CustomerState{
        customer = customer_to_thrift(Customer),
        bank_card_refs = [#customer_BankCardRef{id = Id} || Id <- BankCardIds],
        payment_refs = [
            #customer_PaymentRef{
                invoice_id = maps:get(invoice_id, P),
                payment_id = maps:get(payment_id, P)
            }
         || P <- PaymentRefs
        ]
    }.

-spec bank_card_to_thrift(bank_card_with_tokens(), tokens_map()) ->
    dmsl_customer_thrift:'BankCard'().
bank_card_to_thrift(BankCard, TokensMap) ->
    PartyRefs = maps:get(party_refs, BankCard, []),
    #customer_BankCard{
        id = maps:get(id, BankCard),
        bank_card_token = maps:get(bank_card_token, BankCard),
        party_refs = [binary_to_party_ref(P) || P <- PartyRefs],
        card_mask = maps:get(card_mask, BankCard, undefined),
        created_at = format_timestamp_required(maps:get(created_at, BankCard)),
        recurrent_tokens = TokensMap
    }.

-spec bank_card_with_tokens_to_thrift(bank_card_with_tokens()) -> dmsl_customer_thrift:'BankCard'().
bank_card_with_tokens_to_thrift(BankCard) ->
    Tokens = maps:get(recurrent_tokens, BankCard, []),
    TokensMap = lists:foldl(
        fun(Token, Acc) ->
            ProviderRefBin = maps:get(provider_ref, Token),
            TerminalRefBin = maps:get(terminal_ref, Token),
            Key = #customer_ProviderTerminalKey{
                provider_ref = binary_to_provider_ref(ProviderRefBin),
                terminal_ref = binary_to_terminal_ref(TerminalRefBin)
            },
            Acc#{Key => recurrent_token_to_thrift(Token)}
        end,
        #{},
        Tokens
    ),
    bank_card_to_thrift(BankCard, TokensMap).

-spec bank_card_info_to_thrift(bank_card_info()) -> dmsl_customer_thrift:'BankCardInfo'().
bank_card_info_to_thrift(BankCard) ->
    ProviderRefs = maps:get(recurrent_providers, BankCard, []),
    #customer_BankCardInfo{
        id = maps:get(id, BankCard),
        card_mask = maps:get(card_mask, BankCard, undefined),
        created_at = format_timestamp_required(maps:get(created_at, BankCard)),
        recurrent_providers = [binary_to_provider_ref(P) || P <- ProviderRefs]
    }.

-spec payment_to_thrift(payment_ref()) -> dmsl_customer_thrift:'CustomerPayment'().
payment_to_thrift(Payment) ->
    #customer_CustomerPayment{
        invoice_id = maps:get(invoice_id, Payment),
        payment_id = maps:get(payment_id, Payment),
        created_at = format_timestamp_required(maps:get(created_at, Payment))
    }.

-spec recurrent_token_to_thrift(recurrent_token()) -> dmsl_customer_thrift:'RecurrentToken'().
recurrent_token_to_thrift(Token) ->
    #customer_RecurrentToken{
        id = maps:get(id, Token),
        provider_ref = binary_to_provider_ref(maps:get(provider_ref, Token)),
        terminal_ref = binary_to_terminal_ref(maps:get(terminal_ref, Token)),
        token = maps:get(token, Token),
        created_at = format_timestamp_required(maps:get(created_at, Token)),
        status = recurrent_token_status(Token)
    }.

%% Internal functions

-spec customer_status(customer()) -> dmsl_customer_thrift:'CustomerStatus'().
customer_status(#{deleted_at := DeletedAt}) when DeletedAt =/= undefined ->
    {deleted, #customer_CustomerDeleted{deleted_at = format_timestamp_required(DeletedAt)}};
customer_status(_) ->
    {active, #customer_CustomerActive{}}.

-spec recurrent_token_status(recurrent_token()) -> dmsl_customer_thrift:'RecurrentTokenStatus'().
recurrent_token_status(#{invalidated_at := InvalidatedAt, invalidated_reason := Reason}) when
    InvalidatedAt =/= undefined
->
    {invalidated, #customer_RecurrentTokenInvalidated{
        invalidated_at = format_timestamp_required(InvalidatedAt),
        reason = Reason
    }};
recurrent_token_status(_) ->
    {active, #customer_RecurrentTokenActive{}}.

-spec format_timestamp_required(binary() | calendar:datetime()) -> binary().
format_timestamp_required(Timestamp) when is_binary(Timestamp) -> Timestamp;
format_timestamp_required({{Y, M, D}, {H, Mi, S}}) when is_float(S) ->
    Sec = trunc(S),
    Micro = round((S - Sec) * 1000000),
    format_iso8601(Y, M, D, H, Mi, Sec, Micro);
format_timestamp_required({{Y, M, D}, {H, Mi, S}}) ->
    format_iso8601(Y, M, D, H, Mi, S, undefined).

format_iso8601(Y, M, D, H, Mi, S, MaybeMicro) ->
    DateTimePart = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:", [Y, M, D, H, Mi]),
    SecPart = format_seconds(S, MaybeMicro),
    iolist_to_binary([DateTimePart, SecPart]).

format_seconds(S, undefined) ->
    io_lib:format("~2..0BZ", [S]);
format_seconds(S, Micro) ->
    io_lib:format("~2..0B.~6..0BZ", [S, Micro]).

-spec binary_to_party_ref(binary()) -> dmsl_domain_thrift:'PartyConfigRef'().
binary_to_party_ref(PartyRefBin) when is_binary(PartyRefBin) ->
    try
        json_to_ref(PartyRefBin, {struct, struct, {dmsl_domain_thrift, 'PartyConfigRef'}})
    catch
        _:_ ->
            %% Legacy format: plain ID
            #domain_PartyConfigRef{id = PartyRefBin}
    end.

-spec binary_to_provider_ref(binary() | dmsl_domain_thrift:'ProviderRef'()) ->
    dmsl_domain_thrift:'ProviderRef'().
binary_to_provider_ref(#domain_ProviderRef{} = Ref) ->
    Ref;
binary_to_provider_ref(ProviderRefBin) when is_binary(ProviderRefBin) ->
    try
        json_to_ref(ProviderRefBin, {struct, struct, {dmsl_domain_thrift, 'ProviderRef'}})
    catch
        _:_ ->
            %% Legacy format: plain integer string
            #domain_ProviderRef{id = binary_to_integer(ProviderRefBin)}
    end.

-spec binary_to_terminal_ref(binary() | dmsl_domain_thrift:'TerminalRef'()) ->
    dmsl_domain_thrift:'TerminalRef'().
binary_to_terminal_ref(#domain_TerminalRef{} = Ref) ->
    Ref;
binary_to_terminal_ref(TerminalRefBin) when is_binary(TerminalRefBin) ->
    try
        json_to_ref(TerminalRefBin, {struct, struct, {dmsl_domain_thrift, 'TerminalRef'}})
    catch
        _:_ ->
            %% Legacy format: plain integer string
            #domain_TerminalRef{id = binary_to_integer(TerminalRefBin)}
    end.

json_to_ref(Json, Type) ->
    cs_json:json_to_term(cs_json:decode(Json), Type).
