-module(cs_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    create_customer_test/1,
    get_customer_test/1,
    delete_customer_test/1,
    customer_not_found_test/1,
    add_bank_card_test/1,
    remove_bank_card_test/1,
    add_payment_test/1,
    get_payments_test/1,
    get_payments_pagination_test/1,
    get_bank_cards_test/1,
    get_bank_cards_pagination_test/1,
    get_by_parent_payment_test/1,
    create_bank_card_test/1,
    find_bank_card_test/1,
    add_recurrent_token_test/1,
    invalidate_recurrent_token_test/1
]).

all() ->
    [
        {group, customer_management},
        {group, bank_card_storage}
    ].

groups() ->
    [
        {customer_management, [parallel], [
            create_customer_test,
            get_customer_test,
            add_bank_card_test,
            add_payment_test,
            get_payments_test,
            get_payments_pagination_test,
            get_bank_cards_test,
            get_bank_cards_pagination_test,
            get_by_parent_payment_test,
            remove_bank_card_test,
            delete_customer_test,
            customer_not_found_test
        ]},
        {bank_card_storage, [parallel], [
            create_bank_card_test,
            find_bank_card_test,
            add_recurrent_token_test,
            invalidate_recurrent_token_test
        ]}
    ].

init_per_suite(Config) ->
    {Apps, _} = cs_ct_helper:start_apps([scoper, epg_connector, woody, cs]),
    Client = cs_ct_helper:create_client(),
    [{apps, Apps}, {client, Client} | Config].

end_per_suite(Config) ->
    cs_ct_helper:cleanup_db(),
    Apps = ?config(apps, Config),
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Customer Management Tests

create_customer_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-1">>},
    Metadata = {obj, #{<<"key">> => {str, <<"value">>}, <<"nested">> => {obj, #{<<"inner">> => {i, 42}}}}},
    ContactInfo = #domain_ContactInfo{phone_number = <<"+1234567890">>, email = <<"test@example.com">>},
    Params = #customer_CustomerParams{
        party_ref = PartyRef,
        contact_info = ContactInfo,
        metadata = Metadata
    },
    {ok, Customer} = cs_client:create_customer(Params, Client),
    ?assert(is_binary(Customer#customer_Customer.id)),
    ?assertEqual(PartyRef, Customer#customer_Customer.party_ref),
    %% Validate metadata was saved correctly
    ?assertEqual(Metadata, Customer#customer_Customer.metadata),
    %% Validate contact_info was saved correctly
    ?assertEqual(ContactInfo, Customer#customer_Customer.contact_info),
    %% Fetch and verify persisted data
    {ok, State} = cs_client:get_customer(Customer#customer_Customer.id, Client),
    StoredCustomer = State#customer_CustomerState.customer,
    ?assertEqual(Metadata, StoredCustomer#customer_Customer.metadata),
    ?assertEqual(ContactInfo, StoredCustomer#customer_Customer.contact_info),
    ok.

get_customer_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-2">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, State} = cs_client:get_customer(CustomerId, Client),
    ?assertEqual(CustomerId, State#customer_CustomerState.customer#customer_Customer.id),
    ok.

delete_customer_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-3">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, ok} = cs_client:delete_customer(CustomerId, Client),
    {exception, #customer_CustomerNotFound{}} = cs_client:get_customer(CustomerId, Client),
    ok.

customer_not_found_test(Config) ->
    Client = ?config(client, Config),
    FakeId = <<"00000000-0000-0000-0000-000000000000">>,
    {exception, #customer_CustomerNotFound{}} = cs_client:get_customer(FakeId, Client),
    ok.

add_bank_card_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-4">>}
        },
        Client
    ),
    {ok, BankCard} = cs_client:add_bank_card(
        Customer#customer_Customer.id,
        #customer_BankCardParams{
            bank_card_token = <<"token-1">>,
            card_mask = <<"424242******4242">>
        },
        Client
    ),
    ?assert(is_binary(BankCard#customer_BankCard.id)),
    ?assertEqual(<<"token-1">>, BankCard#customer_BankCard.bank_card_token),
    ok.

remove_bank_card_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-5">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, BankCard} = cs_client:add_bank_card(
        CustomerId,
        #customer_BankCardParams{
            bank_card_token = <<"token-2">>
        },
        Client
    ),
    {ok, ok} = cs_client:remove_bank_card(CustomerId, BankCard#customer_BankCard.id, Client),
    {ok, State} = cs_client:get_customer(CustomerId, Client),
    ?assertEqual([], State#customer_CustomerState.bank_card_refs),
    ok.

add_payment_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-6">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerId, <<"invoice-1">>, <<"payment-1">>, Client),
    {ok, State} = cs_client:get_customer(CustomerId, Client),
    ?assertEqual(1, length(State#customer_CustomerState.payment_refs)),
    ok.

get_payments_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-7">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerId, <<"inv-1">>, <<"pay-1">>, Client),
    {ok, ok} = cs_client:add_payment(CustomerId, <<"inv-2">>, <<"pay-2">>, Client),
    {ok, Response} = cs_client:get_payments(CustomerId, 10, undefined, Client),
    ?assertEqual(2, length(Response#customer_CustomerPaymentsResponse.payments)),
    ok.

get_payments_pagination_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-pagination-payments">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    %% Add 5 payments
    lists:foreach(
        fun(N) ->
            InvId = <<"inv-pag-", (integer_to_binary(N))/binary>>,
            PayId = <<"pay-pag-", (integer_to_binary(N))/binary>>,
            {ok, ok} = cs_client:add_payment(CustomerId, InvId, PayId, Client)
        end,
        lists:seq(1, 5)
    ),
    %% Get first page (limit 2)
    {ok, Response1} = cs_client:get_payments(CustomerId, 2, undefined, Client),
    Payments1 = Response1#customer_CustomerPaymentsResponse.payments,
    Token1 = Response1#customer_CustomerPaymentsResponse.continuation_token,
    ?assertEqual(2, length(Payments1)),
    ?assertNotEqual(undefined, Token1),
    %% Get second page
    {ok, Response2} = cs_client:get_payments(CustomerId, 2, Token1, Client),
    Payments2 = Response2#customer_CustomerPaymentsResponse.payments,
    Token2 = Response2#customer_CustomerPaymentsResponse.continuation_token,
    ?assertEqual(2, length(Payments2)),
    ?assertNotEqual(undefined, Token2),
    %% Get third page (only 1 remaining)
    {ok, Response3} = cs_client:get_payments(CustomerId, 2, Token2, Client),
    Payments3 = Response3#customer_CustomerPaymentsResponse.payments,
    Token3 = Response3#customer_CustomerPaymentsResponse.continuation_token,
    ?assertEqual(1, length(Payments3)),
    ?assertEqual(undefined, Token3),
    ok.

get_bank_cards_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-8">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, _} = cs_client:add_bank_card(CustomerId, #customer_BankCardParams{bank_card_token = <<"token-a">>}, Client),
    {ok, _} = cs_client:add_bank_card(CustomerId, #customer_BankCardParams{bank_card_token = <<"token-b">>}, Client),
    {ok, Response} = cs_client:get_bank_cards(CustomerId, 10, undefined, Client),
    ?assertEqual(2, length(Response#customer_CustomerBankCardsResponse.bank_cards)),
    ok.

get_bank_cards_pagination_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-pagination-cards">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    %% Add 5 bank cards
    lists:foreach(
        fun(N) ->
            Token = <<"token-pag-", (integer_to_binary(N))/binary>>,
            {ok, _} = cs_client:add_bank_card(CustomerId, #customer_BankCardParams{bank_card_token = Token}, Client)
        end,
        lists:seq(1, 5)
    ),
    %% Get first page (limit 2)
    {ok, Response1} = cs_client:get_bank_cards(CustomerId, 2, undefined, Client),
    Cards1 = Response1#customer_CustomerBankCardsResponse.bank_cards,
    Token1 = Response1#customer_CustomerBankCardsResponse.continuation_token,
    ?assertEqual(2, length(Cards1)),
    ?assertNotEqual(undefined, Token1),
    %% Get second page
    {ok, Response2} = cs_client:get_bank_cards(CustomerId, 2, Token1, Client),
    Cards2 = Response2#customer_CustomerBankCardsResponse.bank_cards,
    Token2 = Response2#customer_CustomerBankCardsResponse.continuation_token,
    ?assertEqual(2, length(Cards2)),
    ?assertNotEqual(undefined, Token2),
    %% Get third page (only 1 remaining)
    {ok, Response3} = cs_client:get_bank_cards(CustomerId, 2, Token2, Client),
    Cards3 = Response3#customer_CustomerBankCardsResponse.bank_cards,
    Token3 = Response3#customer_CustomerBankCardsResponse.continuation_token,
    ?assertEqual(1, length(Cards3)),
    ?assertEqual(undefined, Token3),
    ok.

get_by_parent_payment_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-9">>}
        },
        Client
    ),
    CustomerId = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerId, <<"invoice-parent">>, <<"payment-parent">>, Client),
    {ok, State} = cs_client:get_customer_by_parent_payment(<<"invoice-parent">>, <<"payment-parent">>, Client),
    ?assertEqual(CustomerId, State#customer_CustomerState.customer#customer_Customer.id),
    ok.

%% Bank Card Storage Tests

create_bank_card_test(Config) ->
    Client = ?config(client, Config),
    {ok, BankCard} = cs_client:create_bank_card(
        #domain_PartyConfigRef{id = <<"party-bc-1">>},
        #customer_BankCardParams{bank_card_token = <<"token-bc-1">>, card_mask = <<"555555******4444">>},
        Client
    ),
    ?assert(is_binary(BankCard#customer_BankCard.id)),
    ok.

find_bank_card_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-bc-2">>},
    {ok, Created} = cs_client:create_bank_card(
        PartyRef,
        #customer_BankCardParams{
            bank_card_token = <<"token-bc-2">>
        },
        Client
    ),
    {ok, Found} = cs_client:find_bank_card(
        #customer_BankCardSearchParams{
            bank_card_token = <<"token-bc-2">>,
            party_ref = PartyRef
        },
        Client
    ),
    ?assertEqual(Created#customer_BankCard.id, Found#customer_BankCard.id),
    ok.

add_recurrent_token_test(Config) ->
    Client = ?config(client, Config),
    {ok, BankCard} = cs_client:create_bank_card(
        #domain_PartyConfigRef{id = <<"party-bc-3">>},
        #customer_BankCardParams{bank_card_token = <<"token-bc-3">>},
        Client
    ),
    BankCardId = BankCard#customer_BankCard.id,
    {ok, Token} = cs_client:add_recurrent_token(
        #customer_RecurrentTokenParams{
            bank_card_id = BankCardId,
            provider_ref = #domain_ProviderRef{id = 1},
            terminal_ref = #domain_TerminalRef{id = 1},
            token = <<"recurrent-token-value">>
        },
        Client
    ),
    ?assert(is_binary(Token#customer_RecurrentToken.id)),
    {ok, Tokens} = cs_client:get_recurrent_tokens(BankCardId, Client),
    ?assertEqual(1, length(Tokens)),
    ok.

invalidate_recurrent_token_test(Config) ->
    Client = ?config(client, Config),
    {ok, BankCard} = cs_client:create_bank_card(
        #domain_PartyConfigRef{id = <<"party-bc-4">>},
        #customer_BankCardParams{bank_card_token = <<"token-bc-4">>},
        Client
    ),
    BankCardId = BankCard#customer_BankCard.id,
    ProviderRef = #domain_ProviderRef{id = 2},
    TerminalRef = #domain_TerminalRef{id = 2},
    {ok, _} = cs_client:add_recurrent_token(
        #customer_RecurrentTokenParams{
            bank_card_id = BankCardId,
            provider_ref = ProviderRef,
            terminal_ref = TerminalRef,
            token = <<"recurrent-token-to-invalidate">>
        },
        Client
    ),
    {ok, ok} = cs_client:invalidate_recurrent_token(
        #customer_InvalidateRecurrentTokenParams{
            bank_card_id = BankCardId,
            key = #customer_ProviderTerminalKey{provider_ref = ProviderRef, terminal_ref = TerminalRef},
            reason = <<"test reason">>
        },
        Client
    ),
    {ok, Tokens} = cs_client:get_recurrent_tokens(BankCardId, Client),
    ?assertEqual(0, length(Tokens)),
    ok.
