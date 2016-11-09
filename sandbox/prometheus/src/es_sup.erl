-module(es_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:put_chars("MKB: es_sup:init/0\n"),
    prometheus_counter:new([{name, http_requests_total}, {help, "Http request count"}]),
    io:put_chars("MKB: prometheus_counter:new/1 done\n"),
    ElliOpts = [{callback, es_callback}, {port, 3000}],
    ElliSpec = {
        es_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.
