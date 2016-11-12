-module(es_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MiddlewareConfig = [{mods,
                         [{elli_prometheus, []}, {es_callback, []}]}],
    ElliOpts = [{callback, elli_middleware},
                {callback_args, MiddlewareConfig}, {port, 3000}],
    ElliSpec = {es_http, {elli, start_link, [ElliOpts]},
                permanent, 5000, worker, [elli]},
    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.
