-module(cs_customer_database).

-export([
    create/5,
    exists/1,
    member/2,
    member/3,
    get/2,
    get_by_external_id/2,
    get_by_email/2,
    find_or_create_by_email/2,
    find_or_create_by_external_id/2,
    add_external_id/3,
    get_by_payment/3,
    payment_customer_membership/3,
    get_by_withdrawal/2,
    delete/2,
    link_bank_card/2,
    unlink_bank_card/2,
    get_bank_cards/3,
    add_payment/4,
    get_payments/4,
    add_withdrawal/3,
    get_withdrawals/4
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(POOL, default_pool).

%% Columns of a customer as seen from one of its parties: `p` is that party, `cp` the
%% Customer's link to it. Spaces on both ends, so that it splices between the
%% surrounding parts of a query.
-define(CUSTOMER_COLUMNS,
    " c.id, p.party_ref, c.contact_info, c.metadata, c.created_at, c.deleted_at, c.email,"
    " cp.external_id "
).
-define(CUSTOMER_IN_PARTY,
    " FROM customer c"
    " JOIN customer_party cp ON cp.customer_id = c.id AND cp.deleted_at IS NULL"
    " JOIN party p ON p.id = cp.party_id "
).

%% Types
-type customer_id() :: binary().
-type bank_card_id() :: binary().
-type invoice_id() :: binary().
-type payment_id() :: binary().
-type withdrawal_id() :: binary().
-type party_id() :: integer().
-type customer() :: cs_customer:customer().

-export_type([customer_id/0, customer/0, bank_card_id/0, invoice_id/0, payment_id/0, withdrawal_id/0, party_id/0]).

%% API

-spec create(binary(), term(), term(), binary() | undefined, binary() | undefined) ->
    {ok, customer_id()} | {error, external_id_conflict | email_conflict | term()}.
create(PartyRef, ContactInfo, Metadata, ExternalID, Email) ->
    %% The customer and its link to the party commit together, so a taken external id
    %% leaves no customer behind
    Fun = fun(Conn) ->
        PartyID = ensure_party(Conn, PartyRef),
        Query = """
        INSERT INTO customer (party_ref, contact_info, metadata, email)
        VALUES ($1, $2, $3, $4)
        RETURNING id
        """,
        Params = [PartyRef, encode_contact_info(ContactInfo), encode_metadata(Metadata), Email],
        case query_rows(Conn, Query, Params) of
            {ok, [{Id}]} ->
                ok = insert_party_link(Conn, Id, PartyID, ExternalID),
                {ok, Id};
            {error, #error{codename = unique_violation, extra = Extra}} ->
                rollback(unique_violation(Extra));
            {error, Reason} ->
                rollback(Reason)
        end
    end,
    transaction(Fun).

%% For the operations that do not depend on a party
-spec exists(customer_id()) -> ok | {error, not_found | term()}.
exists(CustomerID) ->
    Query = """
    SELECT 1
    FROM customer
    WHERE id = $1::uuid
      AND deleted_at IS NULL
    """,
    case query_rows(?POOL, Query, [CustomerID]) of
        {ok, [_]} -> ok;
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% The party, if the customer is in it
-spec member(customer_id(), binary()) -> {ok, party_id()} | {error, not_found | term()}.
member(CustomerID, PartyRef) ->
    member(?POOL, CustomerID, PartyRef).

-spec member(epgsql:connection() | atom(), customer_id(), binary()) ->
    {ok, party_id()} | {error, not_found | term()}.
member(PoolOrConn, CustomerID, PartyRef) ->
    Query = """
    SELECT cp.party_id
    FROM customer c
    JOIN customer_party cp ON cp.customer_id = c.id AND cp.deleted_at IS NULL
    JOIN party p ON p.id = cp.party_id
    WHERE c.id = $1::uuid
      AND p.party_ref = $2
      AND c.deleted_at IS NULL
    """,
    case query_rows(PoolOrConn, Query, [CustomerID, PartyRef]) of
        {ok, [{PartyID}]} -> {ok, PartyID};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% A customer outside the party is not found in it
-spec get(customer_id(), binary()) -> {ok, customer()} | {error, not_found | term()}.
get(CustomerID, PartyRef) ->
    Query = "SELECT" ?CUSTOMER_COLUMNS ?CUSTOMER_IN_PARTY """
    WHERE c.id = $1::uuid
      AND p.party_ref = $2
      AND c.deleted_at IS NULL
    """,
    query_customer(Query, [CustomerID, PartyRef]).

-spec get_by_external_id(binary(), binary()) -> {ok, customer()} | {error, not_found | term()}.
get_by_external_id(ExternalID, PartyRef) ->
    %% The customer may have been created in another party: it is the external id that
    %% belongs to this one
    Query = "SELECT" ?CUSTOMER_COLUMNS ?CUSTOMER_IN_PARTY """
    WHERE cp.external_id = $1
      AND p.party_ref = $2
      AND c.deleted_at IS NULL
    """,
    query_customer(Query, [ExternalID, PartyRef]).

%% Email is expected to be already normalized by the business layer.
-spec get_by_email(binary(), binary()) -> {ok, customer()} | {error, not_found | term()}.
get_by_email(Email, PartyRef) ->
    %% Email is unique within the party the customer was created in, which is what
    %% customer.party_ref still holds
    Query = "SELECT" ?CUSTOMER_COLUMNS ?CUSTOMER_IN_PARTY """
    WHERE c.email = $1
      AND c.party_ref = $2
      AND p.party_ref = $2
      AND c.deleted_at IS NULL
    LIMIT 1
    """,
    query_customer(Query, [Email, PartyRef]).

-spec find_or_create_by_email(binary(), binary()) -> {ok, customer()} | {error, term()}.
find_or_create_by_email(PartyRef, Email) ->
    find_or_create_by_email(PartyRef, Email, 2).

find_or_create_by_email(_PartyRef, _Email, 0) ->
    {error, failed_to_create};
find_or_create_by_email(PartyRef, Email, Rounds) ->
    %% Read first: a payer who already exists is found without a write, so concurrent
    %% payments of one payer neither serialize on the row nor churn the table
    case get_by_email(Email, PartyRef) of
        {error, not_found} ->
            case insert_by_email(PartyRef, Email) of
                %% A concurrent call created the same payer first; its row is visible now,
                %% unless it was deleted in between, in which case the next round creates one
                conflict -> find_or_create_by_email(PartyRef, Email, Rounds - 1);
                Result -> Result
            end;
        Result ->
            Result
    end.

-spec find_or_create_by_external_id(binary(), binary()) -> {ok, customer()} | {error, term()}.
find_or_create_by_external_id(PartyRef, ExternalID) ->
    find_or_create_by_external_id(PartyRef, ExternalID, 2).

find_or_create_by_external_id(_PartyRef, _ExternalID, 0) ->
    {error, failed_to_create};
find_or_create_by_external_id(PartyRef, ExternalID, Rounds) ->
    %% Same read-first scheme as find_or_create_by_email
    case get_by_external_id(ExternalID, PartyRef) of
        {error, not_found} ->
            case insert_by_external_id(PartyRef, ExternalID) of
                {ok, CustomerID} -> ?MODULE:get(CustomerID, PartyRef);
                conflict -> find_or_create_by_external_id(PartyRef, ExternalID, Rounds - 1);
                {error, _} = Error -> Error
            end;
        Result ->
            Result
    end.

-spec add_external_id(customer_id(), binary(), binary()) ->
    ok | {error, not_found | {external_id_conflict, customer_id(), binary()} | term()}.
add_external_id(CustomerID, PartyRef, ExternalID) ->
    add_external_id(CustomerID, PartyRef, ExternalID, 2).

add_external_id(_CustomerID, _PartyRef, _ExternalID, 0) ->
    {error, failed_to_add};
add_external_id(CustomerID, PartyRef, ExternalID, Rounds) ->
    Fun = fun(Conn) ->
        %% Held until commit: a concurrent Delete either sees the link and deletes it as
        %% well, or has already deleted the customer; a concurrent link_bank_card either
        %% sees the link and grants its card to the party, or has committed the card
        %% for grant_bank_cards to find
        ok = lock_customer(Conn, CustomerID, "FOR NO KEY UPDATE"),
        PartyID = ensure_party(Conn, PartyRef),
        %% A customer already in the party gets the id unless it has one there. An id held
        %% by another customer fails on idx_customer_party_external_id instead.
        %% ON CONFLICT predicate is literally the predicate of idx_customer_party_customer,
        %% otherwise PostgreSQL fails to infer the arbiter index (42P10)
        Query = """
        INSERT INTO customer_party (customer_id, party_id, external_id)
        VALUES ($1::uuid, $2, $3)
        ON CONFLICT (customer_id, party_id) WHERE deleted_at IS NULL
        DO UPDATE SET external_id = COALESCE(customer_party.external_id, EXCLUDED.external_id)
        RETURNING external_id, xmax = 0
        """,
        case query_rows(Conn, Query, [CustomerID, PartyID, ExternalID]) of
            {ok, [{ExternalID, true}]} ->
                grant_bank_cards(Conn, CustomerID, PartyRef);
            {ok, [{ExternalID, false}]} ->
                ok;
            {ok, [{HeldExternalID, false}]} ->
                rollback({external_id_conflict, CustomerID, HeldExternalID});
            {error, #error{codename = unique_violation}} ->
                rollback(external_id_taken);
            {error, Reason} ->
                rollback(Reason)
        end
    end,
    case transaction(Fun) of
        {error, external_id_taken} ->
            case get_by_external_id(ExternalID, PartyRef) of
                {ok, #{id := HolderID}} ->
                    {error, {external_id_conflict, HolderID, ExternalID}};
                %% The holder was deleted in between, the next round adds the id
                {error, not_found} ->
                    add_external_id(CustomerID, PartyRef, ExternalID, Rounds - 1);
                {error, _} = Error ->
                    Error
            end;
        Result ->
            Result
    end.

-spec get_by_payment(invoice_id(), payment_id(), binary()) ->
    {ok, customer()} | {error, not_found | term()}.
get_by_payment(InvoiceId, PaymentId, PartyRef) ->
    Query = "SELECT" ?CUSTOMER_COLUMNS ?CUSTOMER_IN_PARTY """
    JOIN payment_ref pr ON c.id = pr.customer_id AND pr.party_id = p.id
    WHERE pr.invoice_id = $1
      AND pr.payment_id = $2
      AND p.party_ref = $3
      AND c.deleted_at IS NULL
    LIMIT 1
    """,
    query_customer(Query, [InvoiceId, PaymentId, PartyRef]).

%% Why get_by_payment found nothing: no payment, or whether its customer is in the party
-spec payment_customer_membership(invoice_id(), payment_id(), binary()) ->
    {ok, no_payment | member | not_member} | {error, term()}.
payment_customer_membership(InvoiceId, PaymentId, PartyRef) ->
    Query = """
    SELECT EXISTS (
        SELECT 1
        FROM customer_party cp
        JOIN party p ON p.id = cp.party_id
        WHERE cp.customer_id = pr.customer_id
          AND cp.deleted_at IS NULL
          AND p.party_ref = $3
    )
    FROM payment_ref pr
    JOIN customer c ON c.id = pr.customer_id
    WHERE pr.invoice_id = $1
      AND pr.payment_id = $2
      AND c.deleted_at IS NULL
    """,
    case query_rows(?POOL, Query, [InvoiceId, PaymentId, PartyRef]) of
        {ok, []} -> {ok, no_payment};
        {ok, [{true}]} -> {ok, member};
        {ok, [{false}]} -> {ok, not_member};
        {error, Reason} -> {error, Reason}
    end.

-spec get_by_withdrawal(withdrawal_id(), binary()) -> {ok, customer()} | {error, not_found | term()}.
get_by_withdrawal(WithdrawalID, PartyRef) ->
    Query = "SELECT" ?CUSTOMER_COLUMNS ?CUSTOMER_IN_PARTY """
    JOIN withdrawal_ref w ON c.id = w.customer_id AND w.party_id = p.id
    WHERE w.withdrawal_id = $1
      AND p.party_ref = $2
      AND c.deleted_at IS NULL
    """,
    query_customer(Query, [WithdrawalID, PartyRef]).

-spec delete(customer_id(), binary()) -> ok | {error, not_found | term()}.
delete(CustomerID, ReleaseReason) ->
    %% The soft delete and the release of the Customer's bindings commit together: released
    %% afterwards, a failed release would leave bindings live on a deleted Customer, and a
    %% repeated Delete would find nothing left to delete. Its links to parties are deleted
    %% the same way, freeing its external ids there
    Fun = fun(Conn) ->
        Query = """
        UPDATE customer
        SET deleted_at = NOW()
        WHERE id = $1 AND deleted_at IS NULL
        """,
        case epg_pool:query(Conn, Query, [CustomerID]) of
            {ok, 1} ->
                ok = delete_party_links(Conn, CustomerID),
                case cs_terminal_affinity_database:release_by_customer(Conn, CustomerID, ReleaseReason) of
                    {ok, _Count} -> ok;
                    {error, Reason} -> erlang:error({?MODULE, Reason})
                end;
            {ok, 0} ->
                erlang:error({?MODULE, not_found});
            {error, Reason} ->
                erlang:error({?MODULE, Reason})
        end
    end,
    case epg_pool:transaction(?POOL, Fun) of
        ok -> ok;
        {rollback, {?MODULE, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

-spec link_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
link_bank_card(CustomerID, BankCardId) ->
    %% Atomic: link bank card to customer AND grant it to every party of the customer.
    %% The lock makes a concurrent add_external_id either see this card or link the
    %% party before this reads the parties
    Fun = fun(Conn) ->
        ok = lock_customer(Conn, CustomerID, "FOR SHARE"),
        Query = """
        WITH customer_link AS (
            INSERT INTO customer_bank_card (customer_id, bank_card_id)
            VALUES ($1, $2)
            ON CONFLICT (customer_id, bank_card_id)
            DO UPDATE SET deleted_at = NULL, created_at = NOW()
            RETURNING customer_id
        )
        INSERT INTO bank_card_party (bank_card_id, party_ref)
        SELECT $2, p.party_ref
        FROM customer_party cp
        JOIN party p ON p.id = cp.party_id
        WHERE cp.customer_id = $1
          AND cp.deleted_at IS NULL
        ON CONFLICT (bank_card_id, party_ref)
        DO UPDATE SET deleted_at = NULL, created_at = NOW()
        """,
        case epg_pool:query(Conn, Query, [CustomerID, BankCardId]) of
            {ok, _} -> ok;
            {error, Reason} -> rollback(Reason)
        end
    end,
    transaction(Fun).

-spec unlink_bank_card(customer_id(), bank_card_id()) -> ok | {error, not_found | term()}.
unlink_bank_card(CustomerID, BankCardId) ->
    Query = """
    UPDATE customer_bank_card
    SET deleted_at = NOW()
    WHERE customer_id = $1
      AND bank_card_id = $2
      AND deleted_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [CustomerID, BankCardId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec get_bank_cards(customer_id(), non_neg_integer(), non_neg_integer()) ->
    {ok, [bank_card_id()], non_neg_integer()} | {error, term()}.
get_bank_cards(CustomerID, Limit, Offset) ->
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
    query_with_total(?POOL, Query, [CustomerID, Limit, Offset], RowMapper).

%% The party of a payment is the one it is first added in
-spec add_payment(customer_id(), party_id(), invoice_id(), payment_id()) -> ok | {error, term()}.
add_payment(CustomerID, PartyID, InvoiceId, PaymentId) ->
    Query = """
    INSERT INTO payment_ref (customer_id, party_id, invoice_id, payment_id)
    VALUES ($1, $2, $3, $4)
    ON CONFLICT (invoice_id, payment_id) DO NOTHING
    """,
    case epg_pool:query(?POOL, Query, [CustomerID, PartyID, InvoiceId, PaymentId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Payments made in the party
-spec get_payments(customer_id(), binary(), non_neg_integer(), non_neg_integer()) ->
    {ok, [{invoice_id(), payment_id(), binary()}], non_neg_integer()} | {error, term()}.
get_payments(CustomerID, PartyRef, Limit, Offset) ->
    Query = """
    SELECT pr.invoice_id, pr.payment_id, pr.created_at, COUNT(*) OVER() AS total
    FROM payment_ref pr
    JOIN party p ON p.id = pr.party_id
    WHERE pr.customer_id = $1
      AND p.party_ref = $2
    ORDER BY pr.created_at DESC
    LIMIT $3 OFFSET $4
    """,
    RowMapper = fun({InvId, PayId, CreatedAt, _Total}) -> {InvId, PayId, CreatedAt} end,
    query_with_total(?POOL, Query, [CustomerID, PartyRef, Limit, Offset], RowMapper).

%% The party of a withdrawal is the one it is first added in
-spec add_withdrawal(customer_id(), party_id(), withdrawal_id()) ->
    ok | {error, withdrawal_of_other_customer | term()}.
add_withdrawal(CustomerID, PartyID, WithdrawalID) ->
    Insert = """
    INSERT INTO withdrawal_ref (customer_id, party_id, withdrawal_id)
    VALUES ($1::uuid, $3, $2)
    ON CONFLICT (withdrawal_id) DO NOTHING
    RETURNING customer_id
    """,
    %% A statement of its own, so it sees the row the conflict waited for
    Select = """
    SELECT customer_id = $1::uuid
    FROM withdrawal_ref
    WHERE withdrawal_id = $2
    """,
    case query_rows(?POOL, Insert, [CustomerID, WithdrawalID, PartyID]) of
        {ok, [_]} ->
            ok;
        {ok, []} ->
            case query_rows(?POOL, Select, [CustomerID, WithdrawalID]) of
                %% A repeat
                {ok, [{true}]} -> ok;
                {ok, [{false}]} -> {error, withdrawal_of_other_customer};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Withdrawals made in the party
-spec get_withdrawals(customer_id(), binary(), non_neg_integer(), non_neg_integer()) ->
    {ok, [{withdrawal_id(), binary()}], non_neg_integer()} | {error, term()}.
get_withdrawals(CustomerID, PartyRef, Limit, Offset) ->
    Query = """
    SELECT w.withdrawal_id, w.created_at, COUNT(*) OVER() AS total
    FROM withdrawal_ref w
    JOIN party p ON p.id = w.party_id
    WHERE w.customer_id = $1
      AND p.party_ref = $2
    ORDER BY w.created_at DESC
    LIMIT $3 OFFSET $4
    """,
    RowMapper = fun({WithdrawalID, CreatedAt, _Total}) -> {WithdrawalID, CreatedAt} end,
    query_with_total(?POOL, Query, [CustomerID, PartyRef, Limit, Offset], RowMapper).

%% Internal functions

insert_by_email(PartyRef, Email) ->
    %% Spans two tables, hence a transaction: the customer is never seen outside its party
    Fun = fun(Conn) ->
        PartyID = ensure_party(Conn, PartyRef),
        %% ON CONFLICT predicate is literally the predicate of idx_customer_email_party,
        %% otherwise PostgreSQL fails to infer the arbiter index (42P10)
        Query = """
        INSERT INTO customer (party_ref, email)
        VALUES ($1, $2)
        ON CONFLICT (email, party_ref) WHERE deleted_at IS NULL AND email IS NOT NULL
        DO NOTHING
        RETURNING id, party_ref, contact_info, metadata, created_at, deleted_at, email, NULL::text
        """,
        case query_rows(Conn, Query, [PartyRef, Email]) of
            {ok, [{CustomerID, _, _, _, _, _, _, _} = Row]} ->
                ok = insert_party_link(Conn, CustomerID, PartyID, undefined),
                {ok, row_to_customer(Row)};
            %% The conflict waited for the concurrent insert to commit
            {ok, []} ->
                rollback(conflict);
            {error, Reason} ->
                rollback(Reason)
        end
    end,
    case transaction(Fun) of
        {error, conflict} -> conflict;
        Result -> Result
    end.

insert_by_external_id(PartyRef, ExternalID) ->
    %% Spans two tables, hence a transaction: a conflict on the external id rolls the
    %% fresh customer back rather than leaving it orphaned
    Fun = fun(Conn) ->
        PartyID = ensure_party(Conn, PartyRef),
        CustomerQuery = """
        INSERT INTO customer (party_ref)
        VALUES ($1)
        RETURNING id
        """,
        %% ON CONFLICT predicate is literally the predicate of idx_customer_party_external_id,
        %% otherwise PostgreSQL fails to infer the arbiter index (42P10)
        LinkQuery = """
        INSERT INTO customer_party (customer_id, party_id, external_id)
        VALUES ($1, $2, $3)
        ON CONFLICT (party_id, external_id) WHERE deleted_at IS NULL AND external_id IS NOT NULL
        DO NOTHING
        RETURNING customer_id
        """,
        case query_rows(Conn, CustomerQuery, [PartyRef]) of
            {ok, [{CustomerID}]} ->
                case query_rows(Conn, LinkQuery, [CustomerID, PartyID, ExternalID]) of
                    {ok, [_]} -> {ok, CustomerID};
                    %% The conflict waited for the concurrent insert to commit
                    {ok, []} -> rollback(conflict);
                    {error, Reason} -> rollback(Reason)
                end;
            {error, Reason} ->
                rollback(Reason)
        end
    end,
    case transaction(Fun) of
        {error, conflict} -> conflict;
        Result -> Result
    end.

insert_party_link(Conn, CustomerID, PartyID, ExternalID) ->
    Query = """
    INSERT INTO customer_party (customer_id, party_id, external_id)
    VALUES ($1, $2, $3)
    """,
    case epg_pool:query(Conn, Query, [CustomerID, PartyID, ExternalID]) of
        {ok, 1} -> ok;
        {error, #error{codename = unique_violation, extra = Extra}} -> rollback(unique_violation(Extra));
        {error, Reason} -> rollback(Reason)
    end.

%% Registers the party on first sight. Separate statements rather than an upsert: the
%% second SELECT sees a row a concurrent call committed while the INSERT waited on it,
%% and no lock is held on the party row, which every customer of the party would contend
ensure_party(Conn, PartyRef) ->
    Select = "SELECT id FROM party WHERE party_ref = $1",
    Insert = """
    INSERT INTO party (party_ref)
    VALUES ($1)
    ON CONFLICT (party_ref) DO NOTHING
    RETURNING id
    """,
    case query_rows(Conn, Select, [PartyRef]) of
        {ok, [{PartyID}]} ->
            PartyID;
        {ok, []} ->
            case query_rows(Conn, Insert, [PartyRef]) of
                {ok, [{PartyID}]} ->
                    PartyID;
                {ok, []} ->
                    case query_rows(Conn, Select, [PartyRef]) of
                        {ok, [{PartyID}]} -> PartyID;
                        {ok, []} -> rollback(party_not_registered);
                        {error, Reason} -> rollback(Reason)
                    end;
                {error, Reason} ->
                    rollback(Reason)
            end;
        {error, Reason} ->
            rollback(Reason)
    end.

%% Cards of a customer that has just joined a party become available to it as well
grant_bank_cards(Conn, CustomerID, PartyRef) ->
    Query = """
    INSERT INTO bank_card_party (bank_card_id, party_ref)
    SELECT cbc.bank_card_id, $2
    FROM customer_bank_card cbc
    JOIN bank_card bc ON bc.id = cbc.bank_card_id
    WHERE cbc.customer_id = $1::uuid
      AND cbc.deleted_at IS NULL
      AND bc.deleted_at IS NULL
    ON CONFLICT (bank_card_id, party_ref)
    DO UPDATE SET deleted_at = NULL, created_at = NOW()
    WHERE bank_card_party.deleted_at IS NOT NULL
    """,
    case epg_pool:query(Conn, Query, [CustomerID, PartyRef]) of
        {ok, _} -> ok;
        {error, Reason} -> rollback(Reason)
    end.

delete_party_links(Conn, CustomerID) ->
    Query = """
    UPDATE customer_party
    SET deleted_at = NOW()
    WHERE customer_id = $1::uuid
      AND deleted_at IS NULL
    """,
    case epg_pool:query(Conn, Query, [CustomerID]) of
        {ok, _} -> ok;
        {error, Reason} -> rollback(Reason)
    end.

lock_customer(Conn, CustomerID, Lock) ->
    Query = """
    SELECT 1
    FROM customer
    WHERE id = $1::uuid
      AND deleted_at IS NULL
    """ ++ " " ++ Lock,
    case query_rows(Conn, Query, [CustomerID]) of
        {ok, [_]} -> ok;
        {ok, []} -> rollback(not_found);
        {error, Reason} -> rollback(Reason)
    end.

-spec rollback(term()) -> no_return().
rollback(Reason) ->
    erlang:error({?MODULE, Reason}).

transaction(Fun) ->
    case epg_pool:transaction(?POOL, Fun) of
        {rollback, {?MODULE, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason};
        Result -> Result
    end.

query_customer(Query, Params) ->
    case query_rows(?POOL, Query, Params) of
        {ok, [Row]} -> {ok, row_to_customer(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

row_to_customer({Id, PartyRef, ContactInfo, Metadata, CreatedAt, DeletedAt, Email, ExternalID}) ->
    #{
        id => Id,
        party_ref => PartyRef,
        contact_info => decode_contact_info(ContactInfo),
        metadata => decode_metadata(Metadata),
        created_at => CreatedAt,
        deleted_at => null_to_default(DeletedAt, undefined),
        external_id => null_to_default(ExternalID, undefined),
        email => null_to_default(Email, undefined)
    }.

%% Unique indexes on customer and customer_party are both reachable from create/5,
%% so the conflicting one is told apart by constraint name.
unique_violation(Extra) ->
    case proplists:get_value(constraint_name, Extra) of
        <<"idx_customer_party_external_id">> -> external_id_conflict;
        <<"idx_customer_email_party">> -> email_conflict;
        Name -> {unique_violation, Name}
    end.

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
