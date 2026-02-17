-module(cs_ct_helper).

-export([
    start_app/1,
    start_app/2,
    start_apps/1,
    cfg/2,
    create_client/0,
    create_client/1,
    cleanup_db/0
]).

-export_type([config/0]).
-export_type([test_case_name/0]).
-export_type([group_name/0]).

-type app_name() :: atom().
-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().
-type group_name() :: atom().

-spec start_app(app_name()) -> {[app_name()], map()}.
start_app(scoper = AppName) ->
    {
        start_app(AppName, [
            {storage, scoper_storage_logger}
        ]),
        #{}
    };
start_app(woody = AppName) ->
    {
        start_app(AppName, [
            {acceptors_pool_size, 4}
        ]),
        #{}
    };
start_app(cs = AppName) ->
    {
        start_app(AppName, [
            {ip, "::"},
            {port, 8023},
            {default_woody_handling_timeout, 30000},
            {epg_db_name, cs},
            {scoper_event_handler_options, #{
                event_handler_opts => #{
                    formatter_opts => #{
                        max_length => 1000
                    }
                }
            }},
            {services, #{
                customer_management => #{
                    url => <<"http://localhost:8023/v1/customer/management">>
                },
                bank_card_storage => #{
                    url => <<"http://localhost:8023/v1/customer/bank_card">>
                }
            }}
        ]),
        #{}
    };
start_app(epg_connector = AppName) ->
    {
        start_app(AppName, [
            {databases, #{
                cs => #{
                    host => "cs_db",
                    port => 5432,
                    username => "postgres",
                    password => "postgres",
                    database => "customer_storage"
                }
            }},
            {pools, #{
                default_pool => #{
                    database => cs,
                    size => 10
                }
            }}
        ]),
        #{}
    };
start_app(AppName) ->
    {genlib_app:start_application(AppName), #{}}.

-spec start_app(app_name(), list()) -> [app_name()].
start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec start_apps([app_name() | {app_name(), list()}]) -> {[app_name()], map()}.
start_apps(Apps) ->
    lists:foldl(
        fun
            ({AppName, Env}, {AppsAcc, RetAcc}) ->
                {lists:reverse(start_app(AppName, Env), AppsAcc), RetAcc};
            (AppName, {AppsAcc, RetAcc}) ->
                {Apps0, Ret0} = start_app(AppName),
                {lists:reverse(Apps0, AppsAcc), maps:merge(Ret0, RetAcc)}
        end,
        {[], #{}},
        Apps
    ).

-spec cfg(atom(), config()) -> term().
cfg(Key, Config) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        _ -> undefined
    end.

-spec create_client() -> cs_client_api:t().
create_client() ->
    create_client_w_context(woody_context:new()).

-spec create_client(woody:trace_id()) -> cs_client_api:t().
create_client(TraceID) ->
    create_client_w_context(woody_context:new(TraceID)).

create_client_w_context(WoodyCtx) ->
    cs_client_api:new(WoodyCtx).

-spec cleanup_db() -> ok.
cleanup_db() ->
    Query =
        "\n"
        "    DO $$\n"
        "    DECLARE\n"
        "        r RECORD;\n"
        "    BEGIN\n"
        "        FOR r IN (\n"
        "            SELECT table_name\n"
        "            FROM information_schema.tables\n"
        "            WHERE table_schema='public'\n"
        "            AND NOT table_name = '__migrations'\n"
        "        ) LOOP\n"
        "            EXECUTE 'TRUNCATE TABLE ' || quote_ident(r.table_name) || ' RESTART IDENTITY CASCADE';\n"
        "        END LOOP;\n"
        "    END $$;\n"
        "    ",
    {ok, _, _} = epg_pool:query(default_pool, Query),
    ok.
