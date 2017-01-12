-module(erlsrv).

-behavior(application).

-export([start/0, start/2, stop/1]).

start() -> io:put_chars("MKB0\n"), application:start(erlsrv).

start(_Type, _Args) -> io:put_chars("MKB1\n"), es_sup:start_link().

stop(_State) -> ok.
