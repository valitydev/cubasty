-module(cs_client).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% CustomerManagement API
-export([
    create_customer/2,
    get_customer/2,
    get_customer_by_parent_payment/3,
    delete_customer/2,
    add_bank_card/3,
    remove_bank_card/3,
    add_payment/4,
    get_payments/4,
    get_bank_cards/4
]).

%% BankCardStorage API
-export([
    get_bank_card/2,
    find_bank_card/2,
    create_bank_card/3,
    get_recurrent_tokens/2,
    add_recurrent_token/2,
    invalidate_recurrent_token/2
]).

%% CustomerManagement

-spec create_customer(dmsl_customer_thrift:'CustomerParams'(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'Customer'()} | {exception, term()} | {error, term()}.
create_customer(Params, Client) ->
    cs_client_api:call(customer_management, 'Create', [Params], Client).

-spec get_customer(binary(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'CustomerState'()} | {exception, term()} | {error, term()}.
get_customer(CustomerId, Client) ->
    cs_client_api:call(customer_management, 'Get', [CustomerId], Client).

-spec get_customer_by_parent_payment(binary(), binary(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'CustomerState'()} | {exception, term()} | {error, term()}.
get_customer_by_parent_payment(InvoiceId, PaymentId, Client) ->
    cs_client_api:call(customer_management, 'GetByParentPayment', [InvoiceId, PaymentId], Client).

-spec delete_customer(binary(), cs_client_api:t()) ->
    {ok, ok} | {exception, term()} | {error, term()}.
delete_customer(CustomerId, Client) ->
    cs_client_api:call(customer_management, 'Delete', [CustomerId], Client).

-spec add_bank_card(binary(), dmsl_customer_thrift:'BankCardParams'(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'BankCard'()} | {exception, term()} | {error, term()}.
add_bank_card(CustomerId, Params, Client) ->
    cs_client_api:call(customer_management, 'AddBankCard', [CustomerId, Params], Client).

-spec remove_bank_card(binary(), binary(), cs_client_api:t()) ->
    {ok, ok} | {exception, term()} | {error, term()}.
remove_bank_card(CustomerId, BankCardId, Client) ->
    cs_client_api:call(customer_management, 'RemoveBankCard', [CustomerId, BankCardId], Client).

-spec add_payment(binary(), binary(), binary(), cs_client_api:t()) ->
    {ok, ok} | {exception, term()} | {error, term()}.
add_payment(CustomerId, InvoiceId, PaymentId, Client) ->
    cs_client_api:call(customer_management, 'AddPayment', [CustomerId, InvoiceId, PaymentId], Client).

-spec get_payments(binary(), integer(), binary() | undefined, cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'CustomerPaymentsResponse'()} | {exception, term()} | {error, term()}.
get_payments(CustomerId, Limit, ContinuationToken, Client) ->
    cs_client_api:call(customer_management, 'GetPayments', [CustomerId, Limit, ContinuationToken], Client).

-spec get_bank_cards(binary(), integer(), binary() | undefined, cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'CustomerBankCardsResponse'()} | {exception, term()} | {error, term()}.
get_bank_cards(CustomerId, Limit, ContinuationToken, Client) ->
    cs_client_api:call(customer_management, 'GetBankCards', [CustomerId, Limit, ContinuationToken], Client).

%% BankCardStorage

-spec get_bank_card(binary(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'BankCard'()} | {exception, term()} | {error, term()}.
get_bank_card(BankCardId, Client) ->
    cs_client_api:call(bank_card_storage, 'Get', [BankCardId], Client).

-spec find_bank_card(dmsl_customer_thrift:'BankCardSearchParams'(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'BankCard'()} | {exception, term()} | {error, term()}.
find_bank_card(Params, Client) ->
    cs_client_api:call(bank_card_storage, 'Find', [Params], Client).

-spec create_bank_card(
    dmsl_domain_thrift:'PartyConfigRef'(), dmsl_customer_thrift:'BankCardParams'(), cs_client_api:t()
) ->
    {ok, dmsl_customer_thrift:'BankCard'()} | {exception, term()} | {error, term()}.
create_bank_card(PartyRef, Params, Client) ->
    cs_client_api:call(bank_card_storage, 'Create', [PartyRef, Params], Client).

-spec get_recurrent_tokens(binary(), cs_client_api:t()) ->
    {ok, [dmsl_customer_thrift:'RecurrentToken'()]} | {exception, term()} | {error, term()}.
get_recurrent_tokens(BankCardId, Client) ->
    cs_client_api:call(bank_card_storage, 'GetRecurrentTokens', [BankCardId], Client).

-spec add_recurrent_token(dmsl_customer_thrift:'RecurrentTokenParams'(), cs_client_api:t()) ->
    {ok, dmsl_customer_thrift:'RecurrentToken'()} | {exception, term()} | {error, term()}.
add_recurrent_token(Params, Client) ->
    cs_client_api:call(bank_card_storage, 'AddRecurrentToken', [Params], Client).

-spec invalidate_recurrent_token(dmsl_customer_thrift:'InvalidateRecurrentTokenParams'(), cs_client_api:t()) ->
    {ok, ok} | {exception, term()} | {error, term()}.
invalidate_recurrent_token(Params, Client) ->
    cs_client_api:call(bank_card_storage, 'InvalidateRecurrentToken', [Params], Client).
