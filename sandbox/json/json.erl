-module(json).

-behavior(application).

-export([start/0, start/2, stop/1]).

-define(APP, ?MODULE).

start(_Type, _Args) -> json_sup:start_link().

start() -> application:start(?APP).

stop(_State) -> ok.
