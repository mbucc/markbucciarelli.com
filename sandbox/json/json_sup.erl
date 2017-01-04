-module(json_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

test_events() ->
    E1 = [{id, 1}, {name, <<"test1">>}],
    E2 = [{id, 2}, {name, <<"test2">>}],
    [E1, E2].

init(_Args) ->
    Port = 3000,
    MiddlewareConfig = [{mods,
                         [{json_handler, [{events, test_events()}]}]}],
    ElliOpts = [{callback, elli_middleware},
                {callback_args, MiddlewareConfig}, {port, Port}],
    ElliSpec = {json, {elli, start_link, [ElliOpts]},
                permanent, 5000, worker, [elli]},
    io:format("starting server on port ~p~n", [Port]),
    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.
