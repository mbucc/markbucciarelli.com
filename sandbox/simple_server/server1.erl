-module(server1).

-export([rpc/2, start/2]).

start(Name, Mod) ->
    Pid = spawn(fun () -> loop(Name, Mod, Mod:init()) end),
    io:format("~p~n", [Pid]),
    register(Name, Pid).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive {Name, Response} -> Response end.

loop(Name, Mod, State) ->
    receive
      {From, Request} ->
          {Response, State1} = Mod:handle(Request, State),
          From ! {Name, Response},
          loop(Name, Mod, State1)
    end.
