-module(cs_terminal_affinity_database).

-export([
    list/1,
    bind/5,
    release/4,
    release_by_customer/2,
    release_by_terminal/3
]).

-define(POOL, default_pool).

%% Types
-type customer_id() :: cs_customer_database:customer_id().
-type affinity() :: #{
    provider_ref := binary(),
    terminal_ref := binary(),
    bind_seq := integer(),
    bound_at := term(),
    last_used_at := term()
}.
%% What bind/5 releases before writing: nothing, the active affinity whatever its
%% bases (an absolute deadline has passed), or the one whose base column precedes
%% the cutoff timestamp.
-type cutoff() :: undefined | expired | {since_bound | since_last_use, binary()}.
%% The payment a bind is made on behalf of: the idempotency key of bind/5 and, at the
%% same time, the payment remembered for the Customer.
-type payment_ref() :: #{
    invoice_id := binary(),
    payment_id := binary()
}.

-export_type([customer_id/0, affinity/0, cutoff/0, payment_ref/0]).

%% API

-spec list(customer_id()) -> {ok, [affinity()]} | {error, term()}.
list(CustomerID) ->
    Query = """
    SELECT provider_ref, terminal_ref, bind_seq, bound_at, last_used_at
    FROM terminal_affinity
    WHERE customer_id = $1::uuid
      AND released_at IS NULL
    ORDER BY bind_seq ASC
    """,
    case query_rows(?POOL, Query, [CustomerID]) of
        {ok, Rows} -> {ok, [row_to_affinity(Row) || Row <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

-spec bind(customer_id(), binary(), binary(), cutoff(), payment_ref()) ->
    {ok, affinity()} | {error, term()}.
bind(CustomerID, ProviderRef, TerminalRef, Cutoff, Payment) ->
    %% Several statements in a single transaction rather than a data-modifying CTE: CTE
    %% branches share one snapshot and would not see each other's effects.
    Fun = fun(Conn) ->
        ok = lock_customer(Conn, CustomerID),
        %% Idempotency by payment: the payment ledger remembers which binding each
        %% payment made, and a repeat call by the same payment returns it untouched.
        %% Otherwise a retried machine step would read as another successful payment:
        %% a binding expired in between would be released and rebound at the tail.
        case remember_payment(Conn, CustomerID, Payment) of
            {bound, AffinityID} ->
                get_affinity(Conn, AffinityID);
            {fresh, PaymentRefID} ->
                ok = release_expired(Conn, CustomerID, ProviderRef, TerminalRef, Cutoff),
                {Affinity, AffinityID} = upsert(Conn, CustomerID, ProviderRef, TerminalRef),
                ok = link_payment(Conn, PaymentRefID, AffinityID),
                {ok, Affinity}
        end
    end,
    case epg_pool:transaction(?POOL, Fun) of
        {ok, _Affinity} = Result -> Result;
        {error, Reason} -> {error, Reason};
        {rollback, {?MODULE, Reason}} -> {error, Reason};
        {rollback, Reason} -> {error, Reason}
    end.

-spec release(customer_id(), binary(), binary(), binary() | undefined) ->
    ok | {error, not_found | term()}.
release(CustomerID, ProviderRef, TerminalRef, Reason) ->
    Query = """
    UPDATE terminal_affinity
    SET released_at = NOW(), released_reason = $4
    WHERE customer_id = $1::uuid
      AND provider_ref = $2
      AND terminal_ref = $3
      AND released_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [CustomerID, ProviderRef, TerminalRef, Reason]) of
        {ok, N} when N > 0 -> ok;
        {ok, 0} -> {error, not_found};
        {error, Error} -> {error, Error}
    end.

-spec release_by_customer(customer_id(), binary() | undefined) ->
    {ok, non_neg_integer()} | {error, term()}.
release_by_customer(CustomerID, Reason) ->
    Query = """
    UPDATE terminal_affinity
    SET released_at = NOW(), released_reason = $2
    WHERE customer_id = $1::uuid
      AND released_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [CustomerID, Reason]) of
        {ok, N} -> {ok, N};
        {error, Error} -> {error, Error}
    end.

-spec release_by_terminal(binary(), binary(), binary() | undefined) ->
    {ok, non_neg_integer()} | {error, term()}.
release_by_terminal(ProviderRef, TerminalRef, Reason) ->
    Query = """
    UPDATE terminal_affinity
    SET released_at = NOW(), released_reason = $3
    WHERE provider_ref = $1
      AND terminal_ref = $2
      AND released_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [ProviderRef, TerminalRef, Reason]) of
        {ok, N} -> {ok, N};
        {error, Error} -> {error, Error}
    end.

%% Internal functions

%% The deletion check lives inside the transaction and takes a shared row lock: done
%% outside it, before the transaction, a concurrent Delete fits between the check and
%% the write, and the binding would end up on a deleted Customer.
lock_customer(Conn, CustomerID) ->
    Query = """
    SELECT 1
    FROM customer
    WHERE id = $1::uuid
      AND deleted_at IS NULL
    FOR SHARE
    """,
    case query_rows(Conn, Query, [CustomerID]) of
        {ok, [_]} -> ok;
        {ok, []} -> rollback(not_found);
        {error, Reason} -> rollback(Reason)
    end.

%% Puts the payment in the ledger and answers whether it has already made a binding.
%% Writing to the ledger is exactly what AddPayment does, so hellgate no longer calls
%% AddPayment separately.
remember_payment(Conn, CustomerID, #{invoice_id := InvoiceID, payment_id := PaymentID}) ->
    Query = """
    INSERT INTO payment_ref (customer_id, invoice_id, payment_id)
    VALUES ($1::uuid, $2, $3)
    ON CONFLICT (invoice_id, payment_id) DO NOTHING
    RETURNING id
    """,
    case query_rows(Conn, Query, [CustomerID, InvoiceID, PaymentID]) of
        {ok, [{PaymentRefID}]} -> {fresh, PaymentRefID};
        {ok, []} -> lookup_payment(Conn, CustomerID, InvoiceID, PaymentID);
        {error, Reason} -> rollback(Reason)
    end.

lookup_payment(Conn, CustomerID, InvoiceID, PaymentID) ->
    Query = """
    SELECT id, customer_id, terminal_affinity_id
    FROM payment_ref
    WHERE invoice_id = $1
      AND payment_id = $2
    """,
    case query_rows(Conn, Query, [InvoiceID, PaymentID]) of
        %% The ledger is unique by payment database-wide, so the row found may belong to
        %% someone else. It does not become ours: linking it to our binding would credit
        %% one payer's payment to another payer's binding.
        {ok, [{_PaymentRefID, Owner, _AffinityID}]} when Owner =/= CustomerID ->
            rollback(payment_of_other_customer);
        %% The payment is in the ledger but no binding is credited to it: either AddPayment
        %% wrote it, or it predates this migration. It has made no binding yet, so we do.
        {ok, [{PaymentRefID, _Owner, null}]} ->
            {fresh, PaymentRefID};
        {ok, [{_PaymentRefID, _Owner, AffinityID}]} ->
            {bound, AffinityID};
        %% The insert lost to a conflict, yet the row is nowhere to be seen: a neighbouring
        %% transaction is writing it and has not committed. It will finish its own binding,
        %% so there is nothing to redo — we fail, and the caller retries the step later.
        {ok, []} ->
            rollback(concurrent_bind);
        {error, Reason} ->
            rollback(Reason)
    end.

%% The binding this payment made last time. Released ones are not filtered out: the
%% payment has already done its work, and what became of the binding afterwards — an
%% operator released it, or it expired — is no reason for a retry alone to make a second.
get_affinity(Conn, AffinityID) ->
    Query = """
    SELECT provider_ref, terminal_ref, bind_seq, bound_at, last_used_at
    FROM terminal_affinity
    WHERE id = $1::uuid
    """,
    case query_rows(Conn, Query, [AffinityID]) of
        {ok, [Row]} -> {ok, row_to_affinity(Row)};
        {ok, []} -> rollback(affinity_not_found);
        {error, Reason} -> rollback(Reason)
    end.

link_payment(Conn, PaymentRefID, AffinityID) ->
    Query = """
    UPDATE payment_ref
    SET terminal_affinity_id = $2::uuid
    WHERE id = $1::uuid
    """,
    case epg_pool:query(Conn, Query, [PaymentRefID, AffinityID]) of
        {ok, _} -> ok;
        {error, Reason} -> rollback(Reason)
    end.

release_expired(_Conn, _CustomerID, _ProviderRef, _TerminalRef, undefined) ->
    ok;
release_expired(Conn, CustomerID, ProviderRef, TerminalRef, expired) ->
    Query = """
    UPDATE terminal_affinity
    SET released_at = NOW(), released_reason = 'expired'
    WHERE customer_id = $1::uuid
      AND provider_ref = $2
      AND terminal_ref = $3
      AND released_at IS NULL
    """,
    case epg_pool:query(Conn, Query, [CustomerID, ProviderRef, TerminalRef]) of
        {ok, _} -> ok;
        {error, Reason} -> rollback(Reason)
    end;
release_expired(Conn, CustomerID, ProviderRef, TerminalRef, {Base, CutoffAt}) ->
    Query = """
    UPDATE terminal_affinity
    SET released_at = NOW(), released_reason = 'expired'
    WHERE customer_id = $1::uuid
      AND provider_ref = $2
      AND terminal_ref = $3
      AND released_at IS NULL
      AND (CASE $5::text WHEN 'since_bound' THEN bound_at ELSE last_used_at END)
          <= $4::text::timestamptz
    """,
    Params = [CustomerID, ProviderRef, TerminalRef, CutoffAt, atom_to_binary(Base, utf8)],
    case epg_pool:query(Conn, Query, Params) of
        {ok, _} -> ok;
        {error, Reason} -> rollback(Reason)
    end.

upsert(Conn, CustomerID, ProviderRef, TerminalRef) ->
    %% ON CONFLICT predicate is literally the predicate of idx_terminal_affinity_unique
    Query = """
    INSERT INTO terminal_affinity (customer_id, provider_ref, terminal_ref)
    VALUES ($1::uuid, $2, $3)
    ON CONFLICT (customer_id, provider_ref, terminal_ref) WHERE released_at IS NULL
    DO UPDATE SET last_used_at = NOW()
    RETURNING id, provider_ref, terminal_ref, bind_seq, bound_at, last_used_at
    """,
    case query_rows(Conn, Query, [CustomerID, ProviderRef, TerminalRef]) of
        {ok, [{AffinityID, ProviderRefOut, TerminalRefOut, BindSeq, BoundAt, LastUsedAt}]} ->
            Row = {ProviderRefOut, TerminalRefOut, BindSeq, BoundAt, LastUsedAt},
            {row_to_affinity(Row), AffinityID};
        {ok, []} ->
            rollback(failed_to_bind);
        {error, Reason} ->
            rollback(Reason)
    end.

%% Only a raised exception rolls the transaction back; a plain {error, _} return
%% would be committed by epgsql:with_transaction/3.
-spec rollback(term()) -> no_return().
rollback(Reason) ->
    erlang:error({?MODULE, Reason}).

row_to_affinity({ProviderRef, TerminalRef, BindSeq, BoundAt, LastUsedAt}) ->
    #{
        provider_ref => ProviderRef,
        terminal_ref => TerminalRef,
        bind_seq => BindSeq,
        bound_at => BoundAt,
        last_used_at => LastUsedAt
    }.

query_rows(PoolOrConn, Query, Params) ->
    case epg_pool:query(PoolOrConn, Query, Params) of
        {ok, _, _, Rows} -> {ok, Rows};
        {ok, _, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
