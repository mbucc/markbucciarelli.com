-module(json).

-behavior(application).

-export([start/0, start/2, stop/1]).

start(_Type, _Args) -> json_sup:start_link().

start() -> application:start(json).

stop(_State) -> ok.
