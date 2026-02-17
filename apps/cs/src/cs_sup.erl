-module(cs_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([get_service/1]).

-define(APP, cs).
-define(DEFAULT_DB, cs).

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
    WorkDir = get_env_var("WORK_DIR"),
    _ = set_database_url(),
    MigrationsPath = WorkDir ++ "/migrations",
    Cmd = "run",
    case cs_db_migration:process(["-d", MigrationsPath, Cmd]) of
        ok ->
            ok;
        {error, Reason} ->
            error({migrations_error, Reason})
    end.

set_database_url() ->
    EpgDbName = application_get_env(?APP, epg_db_name, ?DEFAULT_DB),
    #{
        EpgDbName := #{
            host := PgHost,
            port := PgPort,
            username := PgUser,
            password := PgPassword,
            database := DbName
        }
    } = application_get_env(epg_connector, databases),
    Value = lists:concat([
        "postgresql://", PgUser, ":", PgPassword(), "@", PgHost, ":", PgPort, "/", DbName
    ]),
    true = os:putenv("DATABASE_URL", Value).

get_env_var(Name) ->
    case os:getenv(Name) of
        false -> error({os_env_required, Name});
        V -> V
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

application_get_env(App, Key) ->
    application_get_env(App, Key, undefined).

application_get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.
