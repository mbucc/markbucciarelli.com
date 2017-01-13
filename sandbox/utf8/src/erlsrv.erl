-module(erlsrv).

-behavior(application).

-export([start/0, start/2, stop/1]).

start() -> application:start(erlsrv).

start(_Type, _Args) -> es_sup:start_link().

stop(_State) -> ok.
