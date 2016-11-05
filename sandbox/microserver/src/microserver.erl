-module(microserver).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> ms_sup:start_link().

stop(_State) -> ok.
