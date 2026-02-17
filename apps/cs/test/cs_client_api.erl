-module(cs_client_api).

-export([new/1]).
-export([call/4]).

-export_type([t/0]).

-type t() :: woody_context:ctx().

-spec new(woody_context:ctx()) -> t().
new(Context) ->
    Context.

-type service_name() :: customer_management | bank_card_storage.

-spec call(service_name(), woody:func(), [any()], t()) -> {ok, _Response} | {exception, _} | {error, _}.
call(ServiceName, Function, Args, Context) ->
    Service = cs_sup:get_service(ServiceName),
    Request = {Service, Function, list_to_tuple(Args)},
    Opts = get_opts(ServiceName),
    try
        woody_client:call(Request, Opts, Context)
    catch
        error:Error:ST ->
            {error, {Error, ST}}
    end.

get_opts(ServiceName) ->
    EventHandlerOpts = genlib_app:env(cs, scoper_event_handler_options, #{}),
    Opts0 = #{
        event_handler => {scoper_woody_event_handler, EventHandlerOpts}
    },
    Services =
        case genlib_app:env(cs, services) of
            S when is_map(S) -> S;
            NotServices -> error({service_not_configured, ServiceName, NotServices})
        end,
    case maps:get(ServiceName, Services, undefined) of
        #{} = Opts ->
            maps:merge(Opts, Opts0);
        _ ->
            Opts0
    end.
