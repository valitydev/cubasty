-module(cs_terminal_affinity).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").

-export([
    list/1,
    bind/4,
    release/3,
    release_by_customer/2,
    release_by_terminal/2
]).

-export_type([
    affinity/0,
    cutoff/0,
    customer_id/0,
    provider_ref/0,
    terminal_ref/0,
    provider_terminal_key/0,
    reason/0,
    ttl/0
]).

%% Types
-type customer_id() :: cs_customer:customer_id().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type provider_terminal_key() :: dmsl_customer_thrift:'ProviderTerminalKey'().
-type ttl() :: dmsl_domain_thrift:'RoutingAffinityTtl'() | undefined.
-type reason() :: binary() | undefined.

-type affinity() :: cs_terminal_affinity_database:affinity().
%% Expiration base and the timestamp an affinity is expired when its base precedes
-type cutoff() :: cs_terminal_affinity_database:cutoff().

%% API

-spec list(customer_id()) -> {ok, [affinity()]} | {error, not_found | term()}.
list(CustomerID) ->
    case cs_customer:get(CustomerID) of
        {ok, _Customer} ->
            cs_terminal_affinity_database:list(CustomerID);
        Error ->
            Error
    end.

-spec bind(customer_id(), provider_ref(), terminal_ref(), ttl()) ->
    {ok, affinity()} | {error, not_found | invalid_request | term()}.
bind(CustomerID, ProviderRef, TerminalRef, Ttl) ->
    case ttl_to_cutoff(Ttl) of
        {ok, Cutoff} ->
            case cs_customer:get(CustomerID) of
                {ok, _Customer} ->
                    {ProviderRefJson, TerminalRefJson} = refs_to_json(ProviderRef, TerminalRef),
                    cs_terminal_affinity_database:bind(
                        CustomerID, ProviderRefJson, TerminalRefJson, Cutoff
                    );
                Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec release(customer_id(), provider_terminal_key(), reason()) -> ok | {error, not_found | term()}.
release(CustomerID, #customer_ProviderTerminalKey{} = Key, Reason) ->
    case cs_customer:get(CustomerID) of
        {ok, _Customer} ->
            {ProviderRefJson, TerminalRefJson} = key_to_json(Key),
            Result = cs_terminal_affinity_database:release(
                CustomerID, ProviderRefJson, TerminalRefJson, Reason
            ),
            %% The customer exists, so an absent active affinity means the release
            %% already happened: the operation is idempotent, not an error. Only a
            %% missing customer is reported as not_found (mapped to CustomerNotFound).
            case Result of
                {error, not_found} -> ok;
                Other -> Other
            end;
        Error ->
            Error
    end.

-spec release_by_customer(customer_id(), reason()) -> ok | {error, term()}.
release_by_customer(CustomerID, Reason) ->
    case cs_terminal_affinity_database:release_by_customer(CustomerID, Reason) of
        {ok, _Count} -> ok;
        Error -> Error
    end.

-spec release_by_terminal(provider_terminal_key(), reason()) -> ok | {error, term()}.
release_by_terminal(#customer_ProviderTerminalKey{} = Key, Reason) ->
    {ProviderRefJson, TerminalRefJson} = key_to_json(Key),
    case cs_terminal_affinity_database:release_by_terminal(ProviderRefJson, TerminalRefJson, Reason) of
        {ok, _Count} -> ok;
        Error -> Error
    end.

%% Internal functions

-spec ttl_to_cutoff(ttl()) -> {ok, cutoff()} | {error, invalid_request}.
ttl_to_cutoff(undefined) ->
    {ok, undefined};
ttl_to_cutoff({Base, Timer}) when Base =:= since_bound; Base =:= since_last_use ->
    case timer_to_timestamp(Timer) of
        {ok, Timestamp} -> {ok, {Base, Timestamp}};
        {error, _} = Error -> Error
    end;
ttl_to_cutoff(_Ttl) ->
    {error, invalid_request}.

-spec timer_to_timestamp(dmsl_base_thrift:'Timer'()) -> {ok, binary()} | {error, invalid_request}.
timer_to_timestamp({timeout, Timeout}) when is_integer(Timeout), Timeout >= 0 ->
    Cutoff = erlang:system_time(second) - Timeout,
    {ok, list_to_binary(calendar:system_time_to_rfc3339(Cutoff, [{offset, "Z"}]))};
timer_to_timestamp({deadline, Deadline}) when is_binary(Deadline) ->
    %% Parsed here rather than left to the PostgreSQL timestamptz parser: the latter
    %% turns a malformed deadline into a transaction error (a system error to the
    %% caller instead of the declared InvalidRequest) and silently accepts the special
    %% values 'now' / 'infinity' / 'yesterday', which would expire every affinity.
    %% Reformatting with an explicit offset also keeps the cutoff independent of the
    %% session TimeZone, which an offsetless timestamp would otherwise depend on.
    try calendar:rfc3339_to_system_time(binary_to_list(Deadline), [{unit, microsecond}]) of
        Micro ->
            Formatted = calendar:system_time_to_rfc3339(Micro, [{unit, microsecond}, {offset, "Z"}]),
            {ok, list_to_binary(Formatted)}
    catch
        _:_ -> {error, invalid_request}
    end;
timer_to_timestamp(_Timer) ->
    {error, invalid_request}.

-spec key_to_json(provider_terminal_key()) -> {binary(), binary()}.
key_to_json(#customer_ProviderTerminalKey{provider_ref = ProviderRef, terminal_ref = TerminalRef}) ->
    refs_to_json(ProviderRef, TerminalRef).

-spec refs_to_json(provider_ref(), terminal_ref()) -> {binary(), binary()}.
refs_to_json(ProviderRef, TerminalRef) ->
    ProviderType = {struct, struct, {dmsl_domain_thrift, 'ProviderRef'}},
    TerminalType = {struct, struct, {dmsl_domain_thrift, 'TerminalRef'}},
    {ref_to_json(ProviderRef, ProviderType), ref_to_json(TerminalRef, TerminalType)}.

-spec ref_to_json(tuple(), cs_json:thrift_type()) -> binary().
ref_to_json(Ref, Type) ->
    cs_json:encode(cs_json:term_to_json(Ref, Type)).
