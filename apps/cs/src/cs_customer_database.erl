-module(cs_customer_database).

-export([
    create/3,
    get/1,
    get_by_payment/2,
    delete/1,
    link_bank_card/2,
    unlink_bank_card/2,
    get_bank_cards/3,
    add_payment/3,
    get_payments/3
]).

-define(POOL, default_pool).

%% Types
-type customer_id() :: binary().
-type bank_card_id() :: binary().
-type invoice_id() :: binary().
-type payment_id() :: binary().
-type customer() :: cs_customer:customer().

-export_type([customer_id/0, customer/0, bank_card_id/0, invoice_id/0, payment_id/0]).

%% API

-spec create(binary(), term(), term()) ->
    {ok, customer_id()} | {error, term()}.
create(PartyRef, ContactInfo, Metadata) ->
    Query = """
    INSERT INTO customer (party_ref, contact_info, metadata)
    VALUES ($1, $2, $3)
    RETURNING id
    """,
    Params = [PartyRef, encode_contact_info(ContactInfo), encode_metadata(Metadata)],
    case epg_pool:query(?POOL, Query, Params) of
        {ok, _, _, [{Id}]} -> {ok, Id};
        {ok, _, [{Id}]} -> {ok, Id};
        {error, Reason} -> {error, Reason};
        Other -> {error, {unexpected_result, Other}}
    end.

-spec get(customer_id()) -> {ok, customer()} | {error, not_found | term()}.
get(CustomerId) ->
    Query = """
    SELECT id, party_ref, contact_info, metadata, created_at, deleted_at
    FROM customer
    WHERE id = $1::uuid
    """,
    case epg_pool:query(?POOL, Query, [CustomerId]) of
        {ok, _, _, [Row]} -> {ok, row_to_customer(Row)};
        {ok, _, _, []} -> {error, not_found};
        {ok, _, [Row]} -> {ok, row_to_customer(Row)};
        {ok, _, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec get_by_payment(invoice_id(), payment_id()) -> {ok, customer()} | {error, not_found | term()}.
get_by_payment(InvoiceId, PaymentId) ->
    Query = """
    SELECT c.id, c.party_ref, c.contact_info, c.metadata, c.created_at, c.deleted_at
    FROM customer c
    JOIN payment_ref pr ON c.id = pr.customer_id
    WHERE pr.invoice_id = $1
      AND pr.payment_id = $2
      AND c.deleted_at IS NULL
    LIMIT 1
    """,
    case epg_pool:query(?POOL, Query, [InvoiceId, PaymentId]) of
        {ok, _, _, [Row]} -> {ok, row_to_customer(Row)};
        {ok, _, _, []} -> {error, not_found};
        {ok, _, [Row]} -> {ok, row_to_customer(Row)};
        {ok, _, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec delete(customer_id()) -> ok | {error, not_found | term()}.
delete(CustomerId) ->
    Query = """
    UPDATE customer
    SET deleted_at = NOW()
    WHERE id = $1 AND deleted_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [CustomerId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec link_bank_card(customer_id(), bank_card_id()) -> ok | {error, term()}.
link_bank_card(CustomerId, BankCardId) ->
    %% Atomic: link bank card to customer AND add customer's party_ref to bank_card_party
    Query = """
    WITH customer_link AS (
        INSERT INTO customer_bank_card (customer_id, bank_card_id)
        VALUES ($1, $2)
        ON CONFLICT (customer_id, bank_card_id)
        DO UPDATE SET deleted_at = NULL, created_at = NOW()
        RETURNING customer_id
    )
    INSERT INTO bank_card_party (bank_card_id, party_ref)
    SELECT $2, c.party_ref
    FROM customer c
    WHERE c.id = $1
    ON CONFLICT (bank_card_id, party_ref)
    DO UPDATE SET deleted_at = NULL, created_at = NOW()
    """,
    case epg_pool:query(?POOL, Query, [CustomerId, BankCardId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec unlink_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
unlink_bank_card(CustomerId, BankCardId) ->
    Query = """
    UPDATE customer_bank_card
    SET deleted_at = NOW()
    WHERE customer_id = $1
      AND bank_card_id = $2
      AND deleted_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [CustomerId, BankCardId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec get_bank_cards(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [bank_card_id()], non_neg_integer()} | {error, term()}.
get_bank_cards(CustomerId, Limit, Offset) ->
    Query = """
    SELECT bc.id, COUNT(*) OVER() AS total
    FROM bank_card bc
    JOIN customer_bank_card cbc ON bc.id = cbc.bank_card_id
    WHERE cbc.customer_id = $1
      AND cbc.deleted_at IS NULL
      AND bc.deleted_at IS NULL
    ORDER BY cbc.created_at DESC
    LIMIT $2 OFFSET $3
    """,
    RowMapper = fun({Id, _Total}) -> Id end,
    query_with_total(?POOL, Query, [CustomerId, Limit, Offset], RowMapper).

-spec add_payment(customer_id(), invoice_id(), payment_id()) -> ok | {error, term()}.
add_payment(CustomerId, InvoiceId, PaymentId) ->
    Query = """
    INSERT INTO payment_ref (customer_id, invoice_id, payment_id)
    VALUES ($1, $2, $3)
    ON CONFLICT (invoice_id, payment_id) DO NOTHING
    """,
    case epg_pool:query(?POOL, Query, [CustomerId, InvoiceId, PaymentId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec get_payments(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [{invoice_id(), payment_id(), binary()}], non_neg_integer()} | {error, term()}.
get_payments(CustomerId, Limit, Offset) ->
    Query = """
    SELECT invoice_id, payment_id, created_at, COUNT(*) OVER() AS total
    FROM payment_ref
    WHERE customer_id = $1
    ORDER BY created_at DESC
    LIMIT $2 OFFSET $3
    """,
    RowMapper = fun({InvId, PayId, CreatedAt, _Total}) -> {InvId, PayId, CreatedAt} end,
    query_with_total(?POOL, Query, [CustomerId, Limit, Offset], RowMapper).

%% Internal functions

row_to_customer({Id, PartyRef, ContactInfo, Metadata, CreatedAt, DeletedAt}) ->
    #{
        id => Id,
        party_ref => PartyRef,
        contact_info => decode_contact_info(ContactInfo),
        metadata => decode_metadata(Metadata),
        created_at => CreatedAt,
        deleted_at => null_to_default(DeletedAt, undefined)
    }.

null_to_default(null, Default) -> Default;
null_to_default(V, _Default) -> V.

%% JSON encoding/decoding helpers for Thrift types

encode_contact_info(undefined) ->
    null;
encode_contact_info(ContactInfo) ->
    Type = {struct, struct, {dmsl_domain_thrift, 'ContactInfo'}},
    cs_json:encode(cs_json:term_to_json(ContactInfo, Type)).

decode_contact_info(null) ->
    undefined;
decode_contact_info(ContactInfoJson) ->
    Type = {struct, struct, {dmsl_domain_thrift, 'ContactInfo'}},
    cs_json:json_to_term(cs_json:decode(ContactInfoJson), Type).

encode_metadata(undefined) ->
    null;
encode_metadata(Metadata) ->
    Type = dmsl_domain_thrift:typedef_info('Metadata'),
    cs_json:encode(cs_json:term_to_json(Metadata, Type)).

decode_metadata(null) ->
    undefined;
decode_metadata(MetadataJson) ->
    Type = dmsl_domain_thrift:typedef_info('Metadata'),
    cs_json:json_to_term(cs_json:decode(MetadataJson), Type).

query_rows(Pool, Query, Params) ->
    case epg_pool:query(Pool, Query, Params) of
        {ok, _, _, Rows} -> {ok, Rows};
        {ok, _, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

query_with_total(Pool, Query, Params, RowMapper) ->
    case query_rows(Pool, Query, Params) of
        {ok, []} ->
            {ok, [], 0};
        {ok, Rows} ->
            Total = element(tuple_size(hd(Rows)), hd(Rows)),
            {ok, [RowMapper(Row) || Row <- Rows], Total};
        {error, Reason} ->
            {error, Reason}
    end.
