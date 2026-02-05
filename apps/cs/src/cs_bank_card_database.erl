-module(cs_bank_card_database).

-export([
    create/2,
    get/1,
    get_with_tokens/1,
    get_with_providers/1,
    find/2,
    find_with_tokens/2,
    find_or_create/3,
    delete/1,
    add_party_ref/2,
    remove_party_ref/2,
    get_party_refs/1,
    get_recurrent_tokens/1,
    add_recurrent_token/4,
    invalidate_recurrent_token/4
]).

-define(POOL, default_pool).

%% Types
-type bank_card_id() :: binary().
-type bank_card() :: #{
    id := bank_card_id(),
    bank_card_token := binary(),
    card_mask => binary() | undefined,
    created_at := binary(),
    deleted_at => binary() | undefined
}.
-type recurrent_token() :: #{
    id := binary(),
    bank_card_id := bank_card_id(),
    provider_ref := binary(),
    terminal_ref := binary(),
    token := binary(),
    created_at := binary(),
    invalidated_at => binary() | undefined,
    invalidated_reason => binary() | undefined
}.

-export_type([bank_card_id/0, bank_card/0, recurrent_token/0]).

%% API

-spec create(binary(), binary() | undefined) -> {ok, bank_card_id()} | {error, term()}.
create(BankCardToken, CardMask) ->
    Query = """
    INSERT INTO bank_card (bank_card_token, card_mask)
    VALUES ($1, $2)
    RETURNING id
    """,
    case epg_pool:query(?POOL, Query, [BankCardToken, CardMask]) of
        {ok, _, _, [{Id}]} -> {ok, Id};
        {ok, _, [{Id}]} -> {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

-spec get(bank_card_id()) -> {ok, bank_card()} | {error, not_found | term()}.
get(BankCardId) ->
    Query = """
    SELECT id, bank_card_token, card_mask, created_at, deleted_at
    FROM bank_card
    WHERE id = $1 AND deleted_at IS NULL
    """,
    case query_rows(?POOL, Query, [BankCardId]) of
        {ok, [Row]} -> {ok, row_to_bank_card(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec get_with_tokens(bank_card_id()) ->
    {ok, bank_card(), [recurrent_token()], [binary()]} | {error, not_found | term()}.
get_with_tokens(BankCardId) ->
    Query = """
    SELECT
        bc.id, bc.bank_card_token, bc.card_mask, bc.created_at, bc.deleted_at,
        COALESCE(
            (SELECT json_agg(json_build_object(
                'id', rt.id, 'bank_card_id', rt.bank_card_id,
                'provider_ref', rt.provider_ref, 'terminal_ref', rt.terminal_ref,
                'token', rt.token, 'created_at', rt.created_at,
                'invalidated_at', rt.invalidated_at, 'invalidated_reason', rt.invalidated_reason
            ) ORDER BY rt.created_at DESC)
            FROM recurrent_token rt
            WHERE rt.bank_card_id = bc.id AND rt.invalidated_at IS NULL),
            '[]'::json
        ) AS tokens,
        COALESCE(
            (SELECT json_agg(bcp.party_ref ORDER BY bcp.created_at)
            FROM bank_card_party bcp
            WHERE bcp.bank_card_id = bc.id AND bcp.deleted_at IS NULL),
            '[]'::json
        ) AS party_refs
    FROM bank_card bc
    WHERE bc.id = $1 AND bc.deleted_at IS NULL
    """,
    case query_rows(?POOL, Query, [BankCardId]) of
        {ok, [{Id, Token, Mask, CreatedAt, DeletedAt, TokensJson, PartyRefsJson}]} ->
            BankCard = row_to_bank_card({Id, Token, Mask, CreatedAt, DeletedAt}),
            Tokens = json_to_recurrent_tokens(TokensJson),
            PartyRefs = json_to_party_refs(PartyRefsJson),
            {ok, BankCard, Tokens, PartyRefs};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_with_providers(bank_card_id()) ->
    {ok, bank_card(), [binary()]} | {error, not_found | term()}.
get_with_providers(BankCardId) ->
    Query = """
    SELECT
        bc.id, bc.bank_card_token, bc.card_mask, bc.created_at, bc.deleted_at,
        COALESCE(
            (SELECT json_agg(DISTINCT rt.provider_ref)
            FROM recurrent_token rt
            WHERE rt.bank_card_id = bc.id AND rt.invalidated_at IS NULL),
            '[]'::json
        ) AS provider_refs
    FROM bank_card bc
    WHERE bc.id = $1 AND bc.deleted_at IS NULL
    """,
    case query_rows(?POOL, Query, [BankCardId]) of
        {ok, [{Id, Token, Mask, CreatedAt, DeletedAt, ProviderRefsJson}]} ->
            BankCard = row_to_bank_card({Id, Token, Mask, CreatedAt, DeletedAt}),
            ProviderRefs = json_to_party_refs(ProviderRefsJson),
            {ok, BankCard, ProviderRefs};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec find(binary(), binary()) -> {ok, bank_card()} | {error, not_found | term()}.
find(BankCardToken, PartyRef) ->
    Query = """
    SELECT bc.id, bc.bank_card_token, bc.card_mask, bc.created_at, bc.deleted_at
    FROM bank_card bc
    JOIN bank_card_party bcp ON bc.id = bcp.bank_card_id
    WHERE bc.bank_card_token = $1
      AND bcp.party_ref = $2
      AND bc.deleted_at IS NULL
      AND bcp.deleted_at IS NULL
    LIMIT 1
    """,
    case query_rows(?POOL, Query, [BankCardToken, PartyRef]) of
        {ok, [Row]} -> {ok, row_to_bank_card(Row)};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find_with_tokens(binary(), binary()) ->
    {ok, bank_card(), [recurrent_token()], [binary()]} | {error, not_found | term()}.
find_with_tokens(BankCardToken, PartyRef) ->
    Query = """
    SELECT
        bc.id, bc.bank_card_token, bc.card_mask, bc.created_at, bc.deleted_at,
        COALESCE(
            (SELECT json_agg(json_build_object(
                'id', rt.id, 'bank_card_id', rt.bank_card_id,
                'provider_ref', rt.provider_ref, 'terminal_ref', rt.terminal_ref,
                'token', rt.token, 'created_at', rt.created_at,
                'invalidated_at', rt.invalidated_at, 'invalidated_reason', rt.invalidated_reason
            ) ORDER BY rt.created_at DESC)
            FROM recurrent_token rt
            WHERE rt.bank_card_id = bc.id AND rt.invalidated_at IS NULL),
            '[]'::json
        ) AS tokens,
        COALESCE(
            (SELECT json_agg(bcp.party_ref ORDER BY bcp.created_at)
            FROM bank_card_party bcp
            WHERE bcp.bank_card_id = bc.id AND bcp.deleted_at IS NULL),
            '[]'::json
        ) AS party_refs
    FROM bank_card bc
    JOIN bank_card_party bcp_filter ON bc.id = bcp_filter.bank_card_id
    WHERE bc.bank_card_token = $1
      AND bcp_filter.party_ref = $2
      AND bc.deleted_at IS NULL
      AND bcp_filter.deleted_at IS NULL
    LIMIT 1
    """,
    case query_rows(?POOL, Query, [BankCardToken, PartyRef]) of
        {ok, [{Id, Token, Mask, CreatedAt, DeletedAt, TokensJson, PartyRefsJson}]} ->
            BankCard = row_to_bank_card({Id, Token, Mask, CreatedAt, DeletedAt}),
            Tokens = json_to_recurrent_tokens(TokensJson),
            PartyRefs = json_to_party_refs(PartyRefsJson),
            {ok, BankCard, Tokens, PartyRefs};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_or_create(binary(), binary(), binary() | undefined) ->
    {ok, bank_card_id()} | {error, term()}.
find_or_create(BankCardToken, PartyRef, CardMask) ->
    %% PostgreSQL 17 MERGE with RETURNING for clean get-or-create
    Query = """
    WITH card AS (
        MERGE INTO bank_card AS target
        USING (SELECT $1::text AS bank_card_token, $3::text AS card_mask) AS source
        ON target.bank_card_token = source.bank_card_token AND target.deleted_at IS NULL
        WHEN NOT MATCHED THEN
            INSERT (bank_card_token, card_mask) VALUES (source.bank_card_token, source.card_mask)
        RETURNING id
    )
    MERGE INTO bank_card_party AS target
    USING (SELECT id AS bank_card_id, $2::text AS party_ref FROM card) AS source
    ON target.bank_card_id = source.bank_card_id AND target.party_ref = source.party_ref
    WHEN MATCHED AND target.deleted_at IS NOT NULL THEN
        UPDATE SET deleted_at = NULL, created_at = NOW()
    WHEN NOT MATCHED THEN
        INSERT (bank_card_id, party_ref) VALUES (source.bank_card_id, source.party_ref)
    RETURNING (SELECT id FROM card)
    """,
    case query_rows(?POOL, Query, [BankCardToken, PartyRef, CardMask]) of
        {ok, [{BankCardId}]} -> {ok, BankCardId};
        {ok, []} -> {error, failed_to_create};
        {error, Reason} -> {error, Reason}
    end.

-spec delete(bank_card_id()) -> ok | {error, not_found | term()}.
delete(BankCardId) ->
    Query = """
    UPDATE bank_card
    SET deleted_at = NOW()
    WHERE id = $1 AND deleted_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [BankCardId]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec add_party_ref(bank_card_id(), binary()) -> ok | {error, term()}.
add_party_ref(BankCardId, PartyRef) ->
    Query = """
    INSERT INTO bank_card_party (bank_card_id, party_ref)
    VALUES ($1, $2)
    ON CONFLICT (bank_card_id, party_ref)
    DO UPDATE SET deleted_at = NULL, created_at = NOW()
    """,
    case epg_pool:query(?POOL, Query, [BankCardId, PartyRef]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec remove_party_ref(bank_card_id(), binary()) -> ok | {error, not_found | term()}.
remove_party_ref(BankCardId, PartyRef) ->
    Query = """
    UPDATE bank_card_party
    SET deleted_at = NOW()
    WHERE bank_card_id = $1
      AND party_ref = $2
      AND deleted_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [BankCardId, PartyRef]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec get_party_refs(bank_card_id()) -> {ok, [binary()]} | {error, term()}.
get_party_refs(BankCardId) ->
    Query = """
    SELECT party_ref
    FROM bank_card_party
    WHERE bank_card_id = $1 AND deleted_at IS NULL
    ORDER BY created_at
    """,
    case query_rows(?POOL, Query, [BankCardId]) of
        {ok, Rows} -> {ok, [Ref || {Ref} <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

-spec get_recurrent_tokens(bank_card_id()) -> {ok, [recurrent_token()]} | {error, term()}.
get_recurrent_tokens(BankCardId) ->
    Query = """
    SELECT id, bank_card_id, provider_ref, terminal_ref, token,
           created_at, invalidated_at, invalidated_reason
    FROM recurrent_token
    WHERE bank_card_id = $1 AND invalidated_at IS NULL
    ORDER BY created_at DESC
    """,
    case query_rows(?POOL, Query, [BankCardId]) of
        {ok, Rows} -> {ok, [row_to_recurrent_token(Row) || Row <- Rows]};
        {error, Reason} -> {error, Reason}
    end.

-spec add_recurrent_token(bank_card_id(), binary(), binary(), binary()) ->
    {ok, recurrent_token()} | {error, term()}.
add_recurrent_token(BankCardId, ProviderRef, TerminalRef, Token) ->
    %% Atomic: invalidate existing token and insert new one in single query
    Query = """
    WITH invalidated AS (
        UPDATE recurrent_token
        SET invalidated_at = NOW(), invalidated_reason = 'replaced'
        WHERE bank_card_id = $1
          AND provider_ref = $2
          AND terminal_ref = $3
          AND invalidated_at IS NULL
    )
    INSERT INTO recurrent_token (bank_card_id, provider_ref, terminal_ref, token)
    VALUES ($1, $2, $3, $4)
    RETURNING id, bank_card_id, provider_ref, terminal_ref, token,
              created_at, invalidated_at, invalidated_reason
    """,
    Params = [BankCardId, ProviderRef, TerminalRef, Token],
    case query_rows(?POOL, Query, Params) of
        {ok, [Row]} -> {ok, row_to_recurrent_token(Row)};
        {ok, []} -> {error, failed_to_create};
        {error, Reason} -> {error, Reason}
    end.

-spec invalidate_recurrent_token(bank_card_id(), binary(), binary(), binary() | undefined) ->
    ok | {error, not_found | term()}.
invalidate_recurrent_token(BankCardId, ProviderRef, TerminalRef, Reason) ->
    Query = """
    UPDATE recurrent_token
    SET invalidated_at = NOW(), invalidated_reason = $4
    WHERE bank_card_id = $1
      AND provider_ref = $2
      AND terminal_ref = $3
      AND invalidated_at IS NULL
    """,
    case epg_pool:query(?POOL, Query, [BankCardId, ProviderRef, TerminalRef, Reason]) of
        {ok, N} when N > 0 -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason2} -> {error, Reason2}
    end.

%% Internal functions

row_to_bank_card({Id, BankCardToken, CardMask, CreatedAt, DeletedAt}) ->
    #{
        id => Id,
        bank_card_token => BankCardToken,
        card_mask => null_to_default(CardMask, <<>>),
        created_at => CreatedAt,
        deleted_at => null_to_default(DeletedAt, undefined)
    }.

row_to_recurrent_token(Row) ->
    {Id, BankCardId, ProviderRef, TerminalRef, Token, CreatedAt, InvalidatedAt, InvalidatedReason} =
        Row,
    #{
        id => Id,
        bank_card_id => BankCardId,
        provider_ref => ProviderRef,
        terminal_ref => TerminalRef,
        token => Token,
        created_at => CreatedAt,
        invalidated_at => null_to_default(InvalidatedAt, undefined),
        invalidated_reason => null_to_default(InvalidatedReason, undefined)
    }.

null_to_default(null, Default) -> Default;
null_to_default(V, _Default) -> V.

query_rows(Pool, Query, Params) ->
    case epg_pool:query(Pool, Query, Params) of
        {ok, _, _, Rows} -> {ok, Rows};
        {ok, _, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

json_to_recurrent_tokens(Json) when is_binary(Json) ->
    json_to_recurrent_tokens(jsx:decode(Json, [{return_maps, true}]));
json_to_recurrent_tokens(List) when is_list(List) ->
    [json_to_recurrent_token(T) || T <- List].

json_to_recurrent_token(
    #{
        <<"id">> := Id,
        <<"bank_card_id">> := BankCardId,
        <<"provider_ref">> := ProviderRef,
        <<"terminal_ref">> := TerminalRef,
        <<"token">> := Token,
        <<"created_at">> := CreatedAt
    } = Map
) ->
    #{
        id => Id,
        bank_card_id => BankCardId,
        provider_ref => ProviderRef,
        terminal_ref => TerminalRef,
        token => Token,
        created_at => CreatedAt,
        invalidated_at => null_to_default(
            maps:get(<<"invalidated_at">>, Map, null), undefined
        ),
        invalidated_reason => null_to_default(
            maps:get(<<"invalidated_reason">>, Map, null), undefined
        )
    }.

json_to_party_refs(Json) when is_binary(Json) ->
    json_to_party_refs(jsx:decode(Json, [{return_maps, true}]));
json_to_party_refs(List) when is_list(List) ->
    List.
