-module(cs_terminal_affinity).

-include_lib("damsel/include/dmsl_customer_thrift.hrl").

-export([
    list/1,
    bind/5,
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
    ttl/0,
    payment_ref/0
]).

%% Types
-type customer_id() :: cs_customer:customer_id().
-type provider_ref() :: dmsl_domain_thrift:'ProviderRef'().
-type terminal_ref() :: dmsl_domain_thrift:'TerminalRef'().
-type provider_terminal_key() :: dmsl_customer_thrift:'ProviderTerminalKey'().
-type ttl() :: dmsl_domain_thrift:'RoutingAffinityTtl'() | undefined.
-type reason() :: binary() | undefined.
-type payment_ref() :: dmsl_customer_thrift:'PaymentRef'().

-type affinity() :: cs_terminal_affinity_database:affinity().
%% What the database layer compares with its own clock to release an expired affinity
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

%% The payment is both the idempotency key of the bind and the payment remembered for
%% the Customer; the database layer does the two in one transaction.
-spec bind(customer_id(), provider_ref(), terminal_ref(), ttl(), payment_ref()) ->
    {ok, affinity()} | {error, not_found | invalid_request | invalid_payment | term()}.
bind(CustomerID, ProviderRef, TerminalRef, Ttl, Payment) ->
    case payment_to_map(Payment) of
        {ok, PaymentMap} ->
            do_bind(CustomerID, ProviderRef, TerminalRef, Ttl, PaymentMap);
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

-spec do_bind(
    customer_id(), provider_ref(), terminal_ref(), ttl(), cs_terminal_affinity_database:payment_ref()
) ->
    {ok, affinity()} | {error, not_found | invalid_request | term()}.
do_bind(CustomerID, ProviderRef, TerminalRef, Ttl, PaymentMap) ->
    case ttl_to_cutoff(Ttl) of
        {ok, Cutoff} ->
            %% The bind transaction checks that the Customer exists itself: a check from
            %% here, outside it, drifts apart from the write under a concurrent delete
            {ProviderRefJson, TerminalRefJson} = refs_to_json(ProviderRef, TerminalRef),
            cs_terminal_affinity_database:bind(
                CustomerID, ProviderRefJson, TerminalRefJson, Cutoff, PaymentMap
            );
        {error, _} = Error ->
            Error
    end.

%% Both halves are required by the schema, but a caller that skips strict validation can
%% still send an empty or absent one; NULL invoice_id would silently defeat the
%% idempotency check (NULL = NULL is never true), so it is rejected here instead.
-spec payment_to_map(payment_ref()) ->
    {ok, cs_terminal_affinity_database:payment_ref()} | {error, invalid_payment}.
payment_to_map(#customer_PaymentRef{invoice_id = InvoiceID, payment_id = PaymentID}) when
    is_binary(InvoiceID), InvoiceID =/= <<>>, is_binary(PaymentID), PaymentID =/= <<>>
->
    {ok, #{invoice_id => InvoiceID, payment_id => PaymentID}};
payment_to_map(_Payment) ->
    {error, invalid_payment}.

%% Every ttl variant names the moment an affinity expires; here it is turned into what the
%% database layer compares with its own clock, the one that stamps bound_at and last_used_at:
%% a timeout for a base column, or an absolute deadline.
-spec ttl_to_cutoff(ttl()) -> {ok, cutoff()} | {error, invalid_request}.
ttl_to_cutoff(undefined) ->
    {ok, undefined};
ttl_to_cutoff({Base, Timeout}) when
    (Base =:= since_bound orelse Base =:= since_last_use), is_integer(Timeout), Timeout >= 0
->
    {ok, {Base, Timeout}};
ttl_to_cutoff({deadline, Deadline}) when is_binary(Deadline) ->
    %% Validated here rather than left to the PostgreSQL timestamptz parser: the latter
    %% turns a malformed deadline into a transaction error (a system error to the caller
    %% instead of the declared InvalidRequest) and silently accepts the special values
    %% 'now' / 'infinity' / 'yesterday', which would expire every affinity.
    try calendar:rfc3339_to_system_time(binary_to_list(Deadline), [{unit, microsecond}]) of
        _ -> {ok, {deadline, Deadline}}
    catch
        _:_ -> {error, invalid_request}
    end;
ttl_to_cutoff(_Ttl) ->
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
