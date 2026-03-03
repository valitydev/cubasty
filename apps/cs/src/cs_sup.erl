-module(cs_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([get_service/1]).

-define(APP, cs).
-define(DEFAULT_DB, cs).
-define(DEFAULT_MIGRATION_OPTS, []).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?APP}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_) ->
    ok = dbinit(),
    {ok, IP} = inet:parse_address(application_get_env(?APP, ip, "::")),
    HealthCheck = enable_health_logging(application_get_env(?APP, health_check, #{})),
    EventHandlers = application_get_env(?APP, woody_event_handlers, [scoper_woody_event_handler]),
    API = woody_server:child_spec(
        ?MODULE,
        #{
            ip => IP,
            port => application_get_env(?APP, port, 8023),
            transport_opts => application_get_env(?APP, transport_opts, #{}),
            protocol_opts => application_get_env(?APP, protocol_opts, #{}),
            event_handler => EventHandlers,
            handlers => get_handlers(),
            additional_routes => [
                get_prometheus_route(),
                erl_health_handle:get_route(HealthCheck)
            ]
        }
    ),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [API],
    {ok, {SupFlags, ChildSpecs}}.

%% Internal functions

dbinit() ->
    case code:priv_dir(?APP) of
        {error, _} ->
            error({migration_error, cant_find_priv_dir});
        Path ->
            MigrationsDir = filename:join([Path, "migrations"]),
            DbRef = application_get_env(?APP, epg_db_name, ?DEFAULT_DB),
            {ok, Databases} = application:get_env(epg_connector, databases),
            DbOpts = maps:get(DbRef, Databases),
            MigrationOpts = application_get_env(?APP, migration_opts, ?DEFAULT_MIGRATION_OPTS),
            ok = check_db_connection(DbOpts),
            logger:info("migrations for cs start"),
            {ok, _} = epg_migrator:perform("cs", DbOpts, MigrationOpts, MigrationsDir),
            logger:info("migrations for cs success"),
            ok
    end.

check_db_connection(
    #{host := Host, port := Port, database := Database, username := Username, password := Password} = DbOpts
) ->
    case epgsql:connect(Host, Username, Password, [{database, Database}, {port, Port}, {timeout, 5000}]) of
        {ok, Conn} ->
            epgsql:close(Conn),
            ok;
        {error, Reason} ->
            logger:error("DB connection check failed, reason: ~p, db_opts: ~p", [Reason, DbOpts]),
            error({db_connection_failed, Reason, DbOpts})
    end.

get_handlers() ->
    DefaultTimeout = application_get_env(?APP, default_woody_handling_timeout, timer:seconds(30)),
    [
        get_handler(customer_management, #{
            default_handling_timeout => DefaultTimeout
        }),
        get_handler(bank_card_storage, #{
            default_handling_timeout => DefaultTimeout
        })
    ].

-spec get_handler(customer_management | bank_card_storage, woody:options()) ->
    woody:http_handler(woody:th_handler()).
get_handler(customer_management, Options) ->
    {"/v1/customer/management", {
        get_service(customer_management),
        {cs_customer_handler, Options}
    }};
get_handler(bank_card_storage, Options) ->
    {"/v1/customer/bank_card", {
        get_service(bank_card_storage),
        {cs_bank_card_handler, Options}
    }}.

-spec get_service(customer_management | bank_card_storage) -> woody:service().
get_service(customer_management) ->
    {dmsl_customer_thrift, 'CustomerManagement'};
get_service(bank_card_storage) ->
    {dmsl_customer_thrift, 'BankCardStorage'}.

-spec enable_health_logging(map()) -> map().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, {_, _, _} = V) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.

application_get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.
