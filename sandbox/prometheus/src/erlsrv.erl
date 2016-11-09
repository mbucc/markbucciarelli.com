-module(erlsrv).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> 
    io:put_chars("erlsrv:start/2 enter\n"),
    es_sup:start_link().

stop(_State) -> io:put_chars("erlsrv:stop/1\n"), ok.
