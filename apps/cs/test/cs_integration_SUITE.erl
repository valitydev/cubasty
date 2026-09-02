-module(cs_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_customer_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% Age, in seconds, an affinity is backdated to before a one day TTL is applied to it
-define(TEN_DAYS, 10 * 24 * 60 * 60).

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
    add_bank_card_idempotent_test/1,
    add_bank_card_idempotent_different_customers_test/1,
    remove_bank_card_test/1,
    add_payment_test/1,
    get_payments_test/1,
    get_payments_pagination_test/1,
    get_bank_cards_test/1,
    get_bank_cards_pagination_test/1,
    get_by_parent_payment_test/1,
    get_by_external_id_test/1,
    get_by_external_id_not_found_test/1,
    create_customer_external_id_conflict_test/1,
    find_or_create_by_email_test/1,
    find_or_create_by_email_concurrent_test/1,
    find_or_create_by_email_normalization_test/1,
    find_or_create_by_email_invalid_test/1,
    create_customer_email_conflict_test/1,
    create_customer_email_other_party_test/1,
    get_by_email_test/1,
    get_by_email_not_found_test/1,
    create_bank_card_test/1,
    find_bank_card_test/1,
    add_recurrent_token_test/1,
    invalidate_recurrent_token_test/1
]).

-export([
    terminal_affinities_empty_test/1,
    terminal_affinities_customer_not_found_test/1,
    bind_terminal_affinity_test/1,
    bind_terminal_affinity_idempotent_test/1,
    bind_terminal_affinity_ttl_since_bound_test/1,
    bind_terminal_affinity_ttl_since_last_use_test/1,
    bind_terminal_affinity_ttl_since_last_use_expires_test/1,
    bind_terminal_affinity_ttl_since_last_use_deadline_test/1,
    bind_terminal_affinity_ttl_deadline_test/1,
    bind_terminal_affinity_invalid_ttl_test/1,
    release_terminal_affinity_test/1,
    release_terminal_affinities_by_terminal_test/1,
    delete_customer_releases_affinities_test/1
]).

all() ->
    [
        {group, customer_management},
        {group, terminal_affinity},
        {group, bank_card_storage}
    ].

groups() ->
    [
        {customer_management, [parallel], [
            create_customer_test,
            get_customer_test,
            add_bank_card_test,
            add_bank_card_idempotent_test,
            add_bank_card_idempotent_different_customers_test,
            add_payment_test,
            get_payments_test,
            get_payments_pagination_test,
            get_bank_cards_test,
            get_bank_cards_pagination_test,
            get_by_parent_payment_test,
            get_by_external_id_test,
            get_by_external_id_not_found_test,
            create_customer_external_id_conflict_test,
            find_or_create_by_email_test,
            find_or_create_by_email_concurrent_test,
            find_or_create_by_email_normalization_test,
            find_or_create_by_email_invalid_test,
            create_customer_email_conflict_test,
            create_customer_email_other_party_test,
            get_by_email_test,
            get_by_email_not_found_test,
            remove_bank_card_test,
            delete_customer_test,
            customer_not_found_test
        ]},
        {terminal_affinity, [parallel], [
            terminal_affinities_empty_test,
            terminal_affinities_customer_not_found_test,
            bind_terminal_affinity_test,
            bind_terminal_affinity_idempotent_test,
            bind_terminal_affinity_ttl_since_bound_test,
            bind_terminal_affinity_ttl_since_last_use_test,
            bind_terminal_affinity_ttl_since_last_use_expires_test,
            bind_terminal_affinity_ttl_since_last_use_deadline_test,
            bind_terminal_affinity_ttl_deadline_test,
            bind_terminal_affinity_invalid_ttl_test,
            release_terminal_affinity_test,
            release_terminal_affinities_by_terminal_test,
            delete_customer_releases_affinities_test
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
    ExternalID = <<"ext-customer-1">>,
    Params = #customer_CustomerParams{
        party_ref = PartyRef,
        contact_info = ContactInfo,
        metadata = Metadata,
        external_id = ExternalID
    },
    {ok, Customer} = cs_client:create_customer(Params, Client),
    ?assert(is_binary(Customer#customer_Customer.id)),
    ?assertEqual(PartyRef, Customer#customer_Customer.party_ref),
    %% Validate metadata was saved correctly
    ?assertEqual(Metadata, Customer#customer_Customer.metadata),
    %% Validate contact_info was saved correctly
    ?assertEqual(ContactInfo, Customer#customer_Customer.contact_info),
    %% Validate external_id was saved correctly
    ?assertEqual(ExternalID, Customer#customer_Customer.external_id),
    %% Fetch and verify persisted data
    {ok, State} = cs_client:get_customer(Customer#customer_Customer.id, Client),
    StoredCustomer = State#customer_CustomerState.customer,
    ?assertEqual(Metadata, StoredCustomer#customer_Customer.metadata),
    ?assertEqual(ContactInfo, StoredCustomer#customer_Customer.contact_info),
    ?assertEqual(ExternalID, StoredCustomer#customer_Customer.external_id),
    ok.

get_customer_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-2">>}
        },
        Client
    ),
    CustomerID = Customer#customer_Customer.id,
    {ok, State} = cs_client:get_customer(CustomerID, Client),
    ?assertEqual(CustomerID, State#customer_CustomerState.customer#customer_Customer.id),
    ok.

delete_customer_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-3">>}
        },
        Client
    ),
    CustomerID = Customer#customer_Customer.id,
    {ok, ok} = cs_client:delete_customer(CustomerID, Client),
    {exception, #customer_CustomerNotFound{}} = cs_client:get_customer(CustomerID, Client),
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

%% Adding the same bank card token to the same customer twice should succeed (idempotent)
add_bank_card_idempotent_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-idempotent">>}
        },
        Client
    ),
    CustomerID = Customer#customer_Customer.id,
    Params = #customer_BankCardParams{bank_card_token = <<"token-idempotent">>},
    {ok, BankCard1} = cs_client:add_bank_card(CustomerID, Params, Client),
    {ok, BankCard2} = cs_client:add_bank_card(CustomerID, Params, Client),
    ?assertEqual(BankCard1#customer_BankCard.id, BankCard2#customer_BankCard.id),
    ok.

%% Same bank card token added to two different customers of the same party should reuse the card
add_bank_card_idempotent_different_customers_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-shared-card">>},
    {ok, Customer1} = cs_client:create_customer(#customer_CustomerParams{party_ref = PartyRef}, Client),
    {ok, Customer2} = cs_client:create_customer(#customer_CustomerParams{party_ref = PartyRef}, Client),
    Params = #customer_BankCardParams{bank_card_token = <<"token-shared">>},
    {ok, BankCard1} = cs_client:add_bank_card(Customer1#customer_Customer.id, Params, Client),
    {ok, BankCard2} = cs_client:add_bank_card(Customer2#customer_Customer.id, Params, Client),
    ?assertEqual(BankCard1#customer_BankCard.id, BankCard2#customer_BankCard.id),
    ok.

remove_bank_card_test(Config) ->
    Client = ?config(client, Config),
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = #domain_PartyConfigRef{id = <<"party-5">>}
        },
        Client
    ),
    CustomerID = Customer#customer_Customer.id,
    {ok, BankCard} = cs_client:add_bank_card(
        CustomerID,
        #customer_BankCardParams{
            bank_card_token = <<"token-2">>
        },
        Client
    ),
    {ok, ok} = cs_client:remove_bank_card(CustomerID, BankCard#customer_BankCard.id, Client),
    {ok, State} = cs_client:get_customer(CustomerID, Client),
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
    CustomerID = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerID, <<"invoice-1">>, <<"payment-1">>, Client),
    {ok, State} = cs_client:get_customer(CustomerID, Client),
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
    CustomerID = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerID, <<"inv-1">>, <<"pay-1">>, Client),
    {ok, ok} = cs_client:add_payment(CustomerID, <<"inv-2">>, <<"pay-2">>, Client),
    {ok, Response} = cs_client:get_payments(CustomerID, 10, undefined, Client),
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
    CustomerID = Customer#customer_Customer.id,
    %% Add 5 payments
    lists:foreach(
        fun(N) ->
            InvId = <<"inv-pag-", (integer_to_binary(N))/binary>>,
            PayId = <<"pay-pag-", (integer_to_binary(N))/binary>>,
            {ok, ok} = cs_client:add_payment(CustomerID, InvId, PayId, Client)
        end,
        lists:seq(1, 5)
    ),
    %% Get first page (limit 2)
    {ok, Response1} = cs_client:get_payments(CustomerID, 2, undefined, Client),
    Payments1 = Response1#customer_CustomerPaymentsResponse.payments,
    Token1 = Response1#customer_CustomerPaymentsResponse.continuation_token,
    ?assertEqual(2, length(Payments1)),
    ?assertNotEqual(undefined, Token1),
    %% Get second page
    {ok, Response2} = cs_client:get_payments(CustomerID, 2, Token1, Client),
    Payments2 = Response2#customer_CustomerPaymentsResponse.payments,
    Token2 = Response2#customer_CustomerPaymentsResponse.continuation_token,
    ?assertEqual(2, length(Payments2)),
    ?assertNotEqual(undefined, Token2),
    %% Get third page (only 1 remaining)
    {ok, Response3} = cs_client:get_payments(CustomerID, 2, Token2, Client),
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
    CustomerID = Customer#customer_Customer.id,
    {ok, _} = cs_client:add_bank_card(CustomerID, #customer_BankCardParams{bank_card_token = <<"token-a">>}, Client),
    {ok, _} = cs_client:add_bank_card(CustomerID, #customer_BankCardParams{bank_card_token = <<"token-b">>}, Client),
    {ok, Response} = cs_client:get_bank_cards(CustomerID, 10, undefined, Client),
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
    CustomerID = Customer#customer_Customer.id,
    %% Add 5 bank cards
    lists:foreach(
        fun(N) ->
            Token = <<"token-pag-", (integer_to_binary(N))/binary>>,
            {ok, _} = cs_client:add_bank_card(CustomerID, #customer_BankCardParams{bank_card_token = Token}, Client)
        end,
        lists:seq(1, 5)
    ),
    %% Get first page (limit 2)
    {ok, Response1} = cs_client:get_bank_cards(CustomerID, 2, undefined, Client),
    Cards1 = Response1#customer_CustomerBankCardsResponse.bank_cards,
    Token1 = Response1#customer_CustomerBankCardsResponse.continuation_token,
    ?assertEqual(2, length(Cards1)),
    ?assertNotEqual(undefined, Token1),
    %% Get second page
    {ok, Response2} = cs_client:get_bank_cards(CustomerID, 2, Token1, Client),
    Cards2 = Response2#customer_CustomerBankCardsResponse.bank_cards,
    Token2 = Response2#customer_CustomerBankCardsResponse.continuation_token,
    ?assertEqual(2, length(Cards2)),
    ?assertNotEqual(undefined, Token2),
    %% Get third page (only 1 remaining)
    {ok, Response3} = cs_client:get_bank_cards(CustomerID, 2, Token2, Client),
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
    CustomerID = Customer#customer_Customer.id,
    {ok, ok} = cs_client:add_payment(CustomerID, <<"invoice-parent">>, <<"payment-parent">>, Client),
    {ok, State} = cs_client:get_customer_by_parent_payment(<<"invoice-parent">>, <<"payment-parent">>, Client),
    ?assertEqual(CustomerID, State#customer_CustomerState.customer#customer_Customer.id),
    ok.

get_by_external_id_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-ext-id">>},
    ExternalID = <<"ext-lookup-1">>,
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{
            party_ref = PartyRef,
            external_id = ExternalID
        },
        Client
    ),
    CustomerID = Customer#customer_Customer.id,
    {ok, State} = cs_client:get_customer_by_external_id(ExternalID, PartyRef, Client),
    ?assertEqual(CustomerID, State#customer_CustomerState.customer#customer_Customer.id),
    ?assertEqual(ExternalID, State#customer_CustomerState.customer#customer_Customer.external_id),
    ok.

get_by_external_id_not_found_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-ext-id-missing">>},
    {exception, #customer_CustomerNotFound{}} =
        cs_client:get_customer_by_external_id(<<"nonexistent">>, PartyRef, Client),
    ok.

create_customer_external_id_conflict_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-ext-conflict">>},
    ExternalID = <<"ext-conflict-1">>,
    Params = #customer_CustomerParams{
        party_ref = PartyRef,
        external_id = ExternalID
    },
    {ok, Customer} = cs_client:create_customer(Params, Client),
    CustomerID = Customer#customer_Customer.id,
    %% Creating another customer with the same external_id and party should conflict
    {exception, #customer_CustomerAlreadyExists{id = ConflictID}} =
        cs_client:create_customer(Params, Client),
    ?assertEqual(CustomerID, ConflictID),
    ok.

%% Repeated find-or-create by the same email resolves to a single customer
find_or_create_by_email_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-find-or-create">>},
    Email = <<"find-or-create@affinity.test">>,
    {ok, Customer1} = cs_client:find_or_create_customer_by_email(PartyRef, Email, Client),
    {ok, Customer2} = cs_client:find_or_create_customer_by_email(PartyRef, Email, Client),
    ?assertEqual(Customer1#customer_Customer.id, Customer2#customer_Customer.id),
    ?assertEqual(Email, Customer1#customer_Customer.email),
    ?assertEqual(PartyRef, Customer1#customer_Customer.party_ref),
    ?assertEqual(1, count_customers_by_email(Email)),
    ok.

%% Concurrent find-or-create is arbitrated by the unique index, not by the caller
find_or_create_by_email_concurrent_test(_Config) ->
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-concurrent">>},
    Email = <<"concurrent@affinity.test">>,
    Self = self(),
    Pids = [
        erlang:spawn_link(fun() ->
            Client = cs_ct_helper:create_client(),
            Self ! {self(), cs_client:find_or_create_customer_by_email(PartyRef, Email, Client)}
        end)
     || _ <- lists:seq(1, 5)
    ],
    Ids = [
        receive
            {Pid, {ok, Customer}} -> Customer#customer_Customer.id
        after 30000 -> error({timeout, Pid})
        end
     || Pid <- Pids
    ],
    ?assertEqual(1, length(lists:usort(Ids))),
    ?assertEqual(1, count_customers_by_email(Email)),
    ok.

%% Email is normalized (trimmed and lowercased) both on write and on lookup
find_or_create_by_email_normalization_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-normalization">>},
    Normalized = <<"user@norm.test">>,
    {ok, Customer1} = cs_client:find_or_create_customer_by_email(PartyRef, <<" User@Norm.Test ">>, Client),
    {ok, Customer2} = cs_client:find_or_create_customer_by_email(PartyRef, Normalized, Client),
    ?assertEqual(Customer1#customer_Customer.id, Customer2#customer_Customer.id),
    ?assertEqual(Normalized, Customer1#customer_Customer.email),
    ?assertEqual(1, count_customers_by_email(Normalized)),
    ok.

find_or_create_by_email_invalid_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-invalid">>},
    lists:foreach(
        fun(Email) ->
            {exception, #base_InvalidRequest{}} =
                cs_client:find_or_create_customer_by_email(PartyRef, Email, Client)
        end,
        [
            <<"">>,
            <<"   ">>,
            <<"no-at-sign">>,
            %% A NUL byte cannot be stored in a text column at all
            <<"nul@byte.test", 0>>,
            %% Past the envelope limit the value no longer fits into the unique index
            <<(binary:copy(<<"x">>, 400))/binary, "@long.test">>
        ]
    ),
    ok.

create_customer_email_conflict_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-conflict">>},
    Email = <<"conflict@affinity.test">>,
    Params = #customer_CustomerParams{party_ref = PartyRef, email = Email},
    {ok, Customer} = cs_client:create_customer(Params, Client),
    {exception, #customer_CustomerEmailConflict{id = ConflictID}} = cs_client:create_customer(Params, Client),
    ?assertEqual(Customer#customer_Customer.id, ConflictID),
    ok.

%% Email uniqueness is scoped to the party
create_customer_email_other_party_test(Config) ->
    Client = ?config(client, Config),
    Email = <<"shared@affinity.test">>,
    {ok, Customer1} = cs_client:create_customer(
        #customer_CustomerParams{party_ref = #domain_PartyConfigRef{id = <<"party-email-shared-1">>}, email = Email},
        Client
    ),
    {ok, Customer2} = cs_client:create_customer(
        #customer_CustomerParams{party_ref = #domain_PartyConfigRef{id = <<"party-email-shared-2">>}, email = Email},
        Client
    ),
    ?assertNotEqual(Customer1#customer_Customer.id, Customer2#customer_Customer.id),
    ?assertEqual(2, count_customers_by_email(Email)),
    ok.

get_by_email_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-get">>},
    Email = <<"get@affinity.test">>,
    {ok, Customer} = cs_client:find_or_create_customer_by_email(PartyRef, Email, Client),
    {ok, State} = cs_client:get_customer_by_email(PartyRef, Email, Client),
    StoredCustomer = State#customer_CustomerState.customer,
    ?assertEqual(Customer#customer_Customer.id, StoredCustomer#customer_Customer.id),
    ?assertEqual(Email, StoredCustomer#customer_Customer.email),
    ok.

get_by_email_not_found_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-email-get-missing">>},
    {exception, #customer_CustomerNotFound{}} =
        cs_client:get_customer_by_email(PartyRef, <<"missing@affinity.test">>, Client),
    ok.

%% Terminal Affinity Tests

terminal_affinities_empty_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-empty">>, Client),
    ?assertEqual({ok, []}, cs_client:get_terminal_affinities(CustomerID, Client)),
    ok.

terminal_affinities_customer_not_found_test(Config) ->
    Client = ?config(client, Config),
    FakeId = <<"00000000-0000-0000-0000-000000000000">>,
    {exception, #customer_CustomerNotFound{}} = cs_client:get_terminal_affinities(FakeId, Client),
    ok.

%% Affinities are listed in bind order, earliest first
bind_terminal_affinity_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-bind">>, Client),
    {ok, First} = bind_affinity(CustomerID, 1, 10, undefined, Client),
    {ok, Second} = bind_affinity(CustomerID, 1, 11, undefined, Client),
    ?assert(First#customer_TerminalAffinity.bind_seq < Second#customer_TerminalAffinity.bind_seq),
    {ok, Affinities} = cs_client:get_terminal_affinities(CustomerID, Client),
    ?assertEqual([{1, 10}, {1, 11}], [affinity_key(A) || A <- Affinities]),
    ok.

%% Rebinding the same terminal keeps bind_seq and only moves last_used_at forward
bind_terminal_affinity_idempotent_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-idempotent">>, Client),
    {ok, First} = bind_affinity(CustomerID, 1, 12, undefined, Client),
    ok = timer:sleep(50),
    {ok, Second} = bind_affinity(CustomerID, 1, 12, undefined, Client),
    ?assertEqual(First#customer_TerminalAffinity.bind_seq, Second#customer_TerminalAffinity.bind_seq),
    %% Two binds are two transactions, so NOW() strictly advances between them
    ?assert(last_used_at(Second) > last_used_at(First)),
    ?assertEqual(First#customer_TerminalAffinity.bound_at, Second#customer_TerminalAffinity.bound_at),
    {ok, Affinities} = cs_client:get_terminal_affinities(CustomerID, Client),
    ?assertEqual(1, length(Affinities)),
    ?assertEqual([], released_affinities(CustomerID)),
    ok.

%% Hard TTL: an affinity bound long ago expires and is rebound at the tail
bind_terminal_affinity_ttl_since_bound_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-ttl-bound">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 20, undefined, Client),
    1 = backdate_affinities(CustomerID, ?TEN_DAYS, ?TEN_DAYS),
    {ok, Second} = bind_affinity(CustomerID, 2, 20, {since_bound, {timeout, 86400}}, Client),
    ?assert(Second#customer_TerminalAffinity.bind_seq > First#customer_TerminalAffinity.bind_seq),
    ?assertEqual([{First#customer_TerminalAffinity.bind_seq, <<"expired">>}], released_affinities(CustomerID)),
    {ok, [Live]} = cs_client:get_terminal_affinities(CustomerID, Client),
    ?assertEqual(Second#customer_TerminalAffinity.bind_seq, Live#customer_TerminalAffinity.bind_seq),
    ok.

%% Sliding TTL looks at last_used_at, hard TTL at bound_at — same row, different verdicts
bind_terminal_affinity_ttl_since_last_use_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-ttl-last-use">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 21, undefined, Client),
    1 = backdate_affinities(CustomerID, ?TEN_DAYS, 0),
    {ok, Second} = bind_affinity(CustomerID, 2, 21, {since_last_use, {timeout, 86400}}, Client),
    ?assertEqual(First#customer_TerminalAffinity.bind_seq, Second#customer_TerminalAffinity.bind_seq),
    ?assertEqual([], released_affinities(CustomerID)),
    %% bound_at is still stale, so the hard TTL does expire the very same row
    {ok, Third} = bind_affinity(CustomerID, 2, 21, {since_bound, {timeout, 86400}}, Client),
    ?assert(Third#customer_TerminalAffinity.bind_seq > First#customer_TerminalAffinity.bind_seq),
    ?assertEqual([{First#customer_TerminalAffinity.bind_seq, <<"expired">>}], released_affinities(CustomerID)),
    ok.

%% The sliding TTL in its expiring position: idle for longer than the term, rebound at the tail
bind_terminal_affinity_ttl_since_last_use_expires_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-ttl-last-use-exp">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 23, undefined, Client),
    %% bound_at stays fresh, so only the sliding base is stale
    1 = backdate_affinities(CustomerID, 0, ?TEN_DAYS),
    {ok, Second} = bind_affinity(CustomerID, 2, 23, {since_last_use, {timeout, 86400}}, Client),
    ?assert(Second#customer_TerminalAffinity.bind_seq > First#customer_TerminalAffinity.bind_seq),
    ?assertEqual([{First#customer_TerminalAffinity.bind_seq, <<"expired">>}], released_affinities(CustomerID)),
    ok.

%% The remaining kind x form combination: sliding base against an absolute cutoff
bind_terminal_affinity_ttl_since_last_use_deadline_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-ttl-last-use-deadline">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 24, undefined, Client),
    Past = {since_last_use, {deadline, <<"2000-01-01T00:00:00Z">>}},
    {ok, Second} = bind_affinity(CustomerID, 2, 24, Past, Client),
    ?assertEqual(First#customer_TerminalAffinity.bind_seq, Second#customer_TerminalAffinity.bind_seq),
    ?assertEqual([], released_affinities(CustomerID)),
    Future = {since_last_use, {deadline, <<"2100-01-01T00:00:00Z">>}},
    {ok, Third} = bind_affinity(CustomerID, 2, 24, Future, Client),
    ?assert(Third#customer_TerminalAffinity.bind_seq > First#customer_TerminalAffinity.bind_seq),
    ?assertEqual([{First#customer_TerminalAffinity.bind_seq, <<"expired">>}], released_affinities(CustomerID)),
    ok.

%% A deadline is parsed before it reaches the database: neither garbage nor the special
%% values PostgreSQL would accept ('now', 'infinity', ...) may reach the cutoff
bind_terminal_affinity_invalid_ttl_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-invalid-ttl">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 25, undefined, Client),
    lists:foreach(
        fun(Deadline) ->
            {exception, #base_InvalidRequest{}} =
                bind_affinity(CustomerID, 2, 25, {since_bound, {deadline, Deadline}}, Client)
        end,
        %% The last one is a timestamp without an offset: its meaning would depend on
        %% the session TimeZone rather than on what the caller sent
        [<<"not-a-timestamp">>, <<"now">>, <<"infinity">>, <<"yesterday">>, <<"2026-01-01T00:00:00">>]
    ),
    {exception, #base_InvalidRequest{}} = bind_affinity(CustomerID, 2, 25, {since_bound, {timeout, -5}}, Client),
    %% Nothing was released and nothing was rebound
    ?assertEqual([], released_affinities(CustomerID)),
    {ok, [Live]} = cs_client:get_terminal_affinities(CustomerID, Client),
    ?assertEqual(First#customer_TerminalAffinity.bind_seq, Live#customer_TerminalAffinity.bind_seq),
    ok.

%% Deadline form: the base is compared against an absolute cutoff
bind_terminal_affinity_ttl_deadline_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-ttl-deadline">>, Client),
    {ok, First} = bind_affinity(CustomerID, 2, 22, undefined, Client),
    Past = {since_bound, {deadline, <<"2000-01-01T00:00:00Z">>}},
    {ok, Second} = bind_affinity(CustomerID, 2, 22, Past, Client),
    ?assertEqual(First#customer_TerminalAffinity.bind_seq, Second#customer_TerminalAffinity.bind_seq),
    ?assertEqual([], released_affinities(CustomerID)),
    Future = {since_bound, {deadline, <<"2100-01-01T00:00:00Z">>}},
    {ok, Third} = bind_affinity(CustomerID, 2, 22, Future, Client),
    ?assert(Third#customer_TerminalAffinity.bind_seq > First#customer_TerminalAffinity.bind_seq),
    ?assertEqual([{First#customer_TerminalAffinity.bind_seq, <<"expired">>}], released_affinities(CustomerID)),
    ok.

release_terminal_affinity_test(Config) ->
    Client = ?config(client, Config),
    CustomerID = create_affinity_customer(<<"party-affinity-release">>, Client),
    {ok, _} = bind_affinity(CustomerID, 3, 30, undefined, Client),
    {ok, _} = bind_affinity(CustomerID, 3, 31, undefined, Client),
    Params = #customer_ReleaseTerminalAffinityParams{
        customer_id = CustomerID,
        key = terminal_key(3, 30),
        reason = <<"manual">>
    },
    {ok, ok} = cs_client:release_terminal_affinity(Params, Client),
    {ok, Affinities} = cs_client:get_terminal_affinities(CustomerID, Client),
    ?assertEqual([{3, 31}], [affinity_key(A) || A <- Affinities]),
    %% Releasing an affinity that is already gone is a no-op, not an error
    {ok, ok} = cs_client:release_terminal_affinity(Params, Client),
    %% A missing customer is still reported as such
    {exception, #customer_CustomerNotFound{}} = cs_client:release_terminal_affinity(
        Params#customer_ReleaseTerminalAffinityParams{customer_id = <<"00000000-0000-0000-0000-000000000000">>},
        Client
    ),
    ok.

%% Bulk release covers every customer bound to the terminal and nothing else
release_terminal_affinities_by_terminal_test(Config) ->
    Client = ?config(client, Config),
    CustomerID1 = create_affinity_customer(<<"party-affinity-release-terminal-1">>, Client),
    CustomerID2 = create_affinity_customer(<<"party-affinity-release-terminal-2">>, Client),
    lists:foreach(
        fun(CustomerID) ->
            {ok, _} = bind_affinity(CustomerID, 4, 40, undefined, Client),
            {ok, _} = bind_affinity(CustomerID, 4, 41, undefined, Client)
        end,
        [CustomerID1, CustomerID2]
    ),
    {ok, ok} = cs_client:release_terminal_affinities_by_terminal(
        terminal_key(4, 40), <<"terminal_disabled">>, Client
    ),
    lists:foreach(
        fun(CustomerID) ->
            {ok, Affinities} = cs_client:get_terminal_affinities(CustomerID, Client),
            ?assertEqual([{4, 41}], [affinity_key(A) || A <- Affinities])
        end,
        [CustomerID1, CustomerID2]
    ),
    ok.

%% Soft-deleting a customer releases its affinities and frees the email
delete_customer_releases_affinities_test(Config) ->
    Client = ?config(client, Config),
    PartyRef = #domain_PartyConfigRef{id = <<"party-affinity-delete">>},
    Email = <<"delete@affinity.test">>,
    {ok, Customer} = cs_client:find_or_create_customer_by_email(PartyRef, Email, Client),
    CustomerID = Customer#customer_Customer.id,
    {ok, Affinity} = bind_affinity(CustomerID, 5, 50, undefined, Client),
    {ok, ok} = cs_client:delete_customer(CustomerID, Client),
    ?assertEqual(
        [{Affinity#customer_TerminalAffinity.bind_seq, <<"customer_deleted">>}],
        released_affinities(CustomerID)
    ),
    {ok, Recreated} = cs_client:find_or_create_customer_by_email(PartyRef, Email, Client),
    RecreatedID = Recreated#customer_Customer.id,
    ?assertNotEqual(CustomerID, RecreatedID),
    ?assertEqual({ok, []}, cs_client:get_terminal_affinities(RecreatedID, Client)),
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

%% Internal functions

create_affinity_customer(PartyID, Client) ->
    {ok, Customer} = cs_client:create_customer(
        #customer_CustomerParams{party_ref = #domain_PartyConfigRef{id = PartyID}},
        Client
    ),
    Customer#customer_Customer.id.

bind_affinity(CustomerID, ProviderID, TerminalID, Ttl, Client) ->
    cs_client:bind_terminal_affinity(
        #customer_TerminalAffinityParams{
            customer_id = CustomerID,
            provider_ref = #domain_ProviderRef{id = ProviderID},
            terminal_ref = #domain_TerminalRef{id = TerminalID},
            ttl = Ttl
        },
        Client
    ).

terminal_key(ProviderID, TerminalID) ->
    #customer_ProviderTerminalKey{
        provider_ref = #domain_ProviderRef{id = ProviderID},
        terminal_ref = #domain_TerminalRef{id = TerminalID}
    }.

affinity_key(#customer_TerminalAffinity{provider_ref = ProviderRef, terminal_ref = TerminalRef}) ->
    {ProviderRef#domain_ProviderRef.id, TerminalRef#domain_TerminalRef.id}.

last_used_at(#customer_TerminalAffinity{last_used_at = Timestamp}) ->
    calendar:rfc3339_to_system_time(binary_to_list(Timestamp), [{unit, microsecond}]).

%% Direct database access, to observe what the API deliberately does not expose

count_customers_by_email(Email) ->
    Query =
        """
        SELECT count(*)
        FROM customer
        WHERE email = $1
          AND deleted_at IS NULL
        """,
    [{Count}] = select(Query, [Email]),
    Count.

released_affinities(CustomerID) ->
    Query =
        """
        SELECT bind_seq, released_reason
        FROM terminal_affinity
        WHERE customer_id = $1::uuid
          AND released_at IS NOT NULL
        ORDER BY bind_seq
        """,
    select(Query, [CustomerID]).

backdate_affinities(CustomerID, BoundAge, LastUsedAge) ->
    Query =
        """
        UPDATE terminal_affinity
        SET bound_at = NOW() - ($2::int * interval '1 second'),
            last_used_at = NOW() - ($3::int * interval '1 second')
        WHERE customer_id = $1::uuid
          AND released_at IS NULL
        """,
    {ok, Count} = epg_pool:query(default_pool, Query, [CustomerID, BoundAge, LastUsedAge]),
    Count.

select(Query, Params) ->
    case epg_pool:query(default_pool, Query, Params) of
        {ok, _Columns, Rows} -> Rows;
        {ok, _Count, _Columns, Rows} -> Rows
    end.
