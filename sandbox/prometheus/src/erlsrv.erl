-module(erlsrv).

-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    prometheus:start(), es_sup:start_link().

stop(_State) -> ok.
