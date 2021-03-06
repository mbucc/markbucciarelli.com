-module(es_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, es_sup}, es_sup, []).

init([]) ->
    P = 3000,
    ElliOpts = [{callback, es_callback}, {port, P}],
    io:format("starting erlsrv on port ~p~n", [P]),
    ElliSpec = {es_http, {elli, start_link, [ElliOpts]},
                permanent, 5000, worker, [elli]},
    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.
