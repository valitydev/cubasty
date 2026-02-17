-module(cs_bank_card).

-compile({no_auto_import, [get/1]}).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([
    create/3,
    get/1,
    get_with_tokens/1,
    get_info/1,
    find/2,
    find_with_tokens/2,
    find_or_create/3,
    get_recurrent_tokens/1,
    add_recurrent_token/4,
    invalidate_recurrent_token/4
]).

-export_type([
    bank_card_id/0,
    bank_card/0,
    bank_card_with_tokens/0,
    bank_card_info/0,
    party_ref/0,
    provider_ref/0,
    terminal_ref/0
]).

%% Types
-type bank_card_id() :: binary().
-type bank_card() :: cs_bank_card_database:bank_card().
-type bank_card_with_tokens() :: #{
    id := bank_card_id(),
    bank_card_token := binary(),
    card_mask => binary() | undefined,
    created_at := binary(),
    deleted_at => binary() | undefined,
    recurrent_tokens := [cs_bank_card_database:recurrent_token()],
    party_refs := [binary()]
}.
-type bank_card_info() :: #{
    id := bank_card_id(),
    bank_card_token := binary(),
    card_mask => binary() | undefined,
    created_at := binary(),
    deleted_at => binary() | undefined,
    recurrent_providers := [binary()]
}.
-type party_ref() :: dmsl_domain_thrift:'PartyConfigRef'().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().

%% API

-spec create(party_ref(), binary(), binary() | undefined) -> {ok, bank_card_id()} | {error, term()}.
create(PartyRef, BankCardToken, CardMask) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_bank_card_database:create(BankCardToken, CardMask) of
        {ok, BankCardId} ->
            case cs_bank_card_database:add_party_ref(BankCardId, PartyRefJson) of
                ok -> {ok, BankCardId};
                Error -> Error
            end;
        Error ->
            Error
    end.

-spec get(bank_card_id()) -> {ok, bank_card()} | {error, not_found | term()}.
get(BankCardId) ->
    cs_bank_card_database:get(BankCardId).

-spec get_with_tokens(bank_card_id()) ->
    {ok, bank_card_with_tokens()} | {error, not_found | term()}.
get_with_tokens(BankCardId) ->
    case cs_bank_card_database:get_with_tokens(BankCardId) of
        {ok, BankCard, Tokens, PartyRefs} ->
            {ok, BankCard#{
                recurrent_tokens => Tokens,
                party_refs => PartyRefs
            }};
        Error ->
            Error
    end.

-spec get_info(bank_card_id()) -> {ok, bank_card_info()} | {error, not_found | term()}.
get_info(BankCardId) ->
    case cs_bank_card_database:get_with_providers(BankCardId) of
        {ok, BankCard, ProviderRefs} ->
            {ok, BankCard#{recurrent_providers => ProviderRefs}};
        Error ->
            Error
    end.

-spec find(binary(), party_ref()) -> {ok, bank_card()} | {error, not_found | term()}.
find(BankCardToken, PartyRef) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    cs_bank_card_database:find(BankCardToken, PartyRefJson).

-spec find_with_tokens(binary(), party_ref()) ->
    {ok, bank_card_with_tokens()} | {error, not_found | term()}.
find_with_tokens(BankCardToken, PartyRef) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    case cs_bank_card_database:find_with_tokens(BankCardToken, PartyRefJson) of
        {ok, BankCard, Tokens, PartyRefs} ->
            {ok, BankCard#{
                recurrent_tokens => Tokens,
                party_refs => PartyRefs
            }};
        Error ->
            Error
    end.

-spec find_or_create(party_ref(), binary(), binary() | undefined) ->
    {ok, bank_card_id()} | {error, term()}.
find_or_create(PartyRef, BankCardToken, CardMask) ->
    PartyRefJson = party_ref_to_json(PartyRef),
    cs_bank_card_database:find_or_create(BankCardToken, PartyRefJson, CardMask).

-spec get_recurrent_tokens(bank_card_id()) -> {ok, [map()]} | {error, term()}.
get_recurrent_tokens(BankCardId) ->
    cs_bank_card_database:get_recurrent_tokens(BankCardId).

-spec add_recurrent_token(bank_card_id(), provider_ref(), terminal_ref(), binary()) ->
    {ok, map()} | {error, term()}.
add_recurrent_token(BankCardId, ProviderRef, TerminalRef, Token) ->
    {ProviderRefJson, TerminalRefJson} = refs_to_json(ProviderRef, TerminalRef),
    cs_bank_card_database:add_recurrent_token(
        BankCardId, ProviderRefJson, TerminalRefJson, Token
    ).

-spec invalidate_recurrent_token(
    bank_card_id(), provider_ref(), terminal_ref(), binary() | undefined
) -> ok | {error, not_found | term()}.
invalidate_recurrent_token(BankCardId, ProviderRef, TerminalRef, Reason) ->
    {ProviderRefJson, TerminalRefJson} = refs_to_json(ProviderRef, TerminalRef),
    cs_bank_card_database:invalidate_recurrent_token(
        BankCardId, ProviderRefJson, TerminalRefJson, Reason
    ).

%% Internal functions

-spec party_ref_to_json(party_ref()) -> binary().
party_ref_to_json(PartyRef) ->
    ref_to_json(PartyRef, {struct, struct, {dmsl_domain_thrift, 'PartyConfigRef'}}).

-spec refs_to_json(provider_ref(), terminal_ref()) -> {binary(), binary()}.
refs_to_json(ProviderRef, TerminalRef) ->
    ProviderType = {struct, struct, {dmsl_domain_thrift, 'ProviderRef'}},
    TerminalType = {struct, struct, {dmsl_domain_thrift, 'TerminalRef'}},
    {ref_to_json(ProviderRef, ProviderType), ref_to_json(TerminalRef, TerminalType)}.

-spec ref_to_json(tuple(), cs_json:thrift_type()) -> binary().
ref_to_json(Ref, Type) ->
    cs_json:encode(cs_json:term_to_json(Ref, Type)).
