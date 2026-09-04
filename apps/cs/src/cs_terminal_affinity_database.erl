-module(cs_terminal_affinity_database).

-export([
    list/1,
    bind/4,
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
%% What bind/4 releases before writing: nothing, the active affinity whatever its
%% bases (an absolute deadline has passed), or the one whose base column precedes
%% the cutoff timestamp.
-type cutoff() :: undefined | expired | {since_bound | since_last_use, binary()}.

-export_type([customer_id/0, affinity/0, cutoff/0]).

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

-spec bind(customer_id(), binary(), binary(), cutoff()) -> {ok, affinity()} | {error, term()}.
bind(CustomerID, ProviderRef, TerminalRef, Cutoff) ->
    %% Two statements in a single transaction rather than a data-modifying CTE: CTE
    %% branches share one snapshot and would not see each other's effects.
    Fun = fun(Conn) ->
        ok = release_expired(Conn, CustomerID, ProviderRef, TerminalRef, Cutoff),
        upsert(Conn, CustomerID, ProviderRef, TerminalRef)
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
          < $4::text::timestamptz
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
    RETURNING provider_ref, terminal_ref, bind_seq, bound_at, last_used_at
    """,
    case query_rows(Conn, Query, [CustomerID, ProviderRef, TerminalRef]) of
        {ok, [Row]} -> {ok, row_to_affinity(Row)};
        {ok, []} -> rollback(failed_to_bind);
        {error, Reason} -> rollback(Reason)
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
