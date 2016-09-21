-module(ns).

-export([add/2, find/1, handle/2, init/0]).

-import(server1, [rpc/2]).

%% client routines

add(Name, Place) ->
    rpc(ns, {add, Name, Place}).

find(Name) -> rpc(ns, {find, Name}).

%% callback routines

init() -> dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) ->
    {dict:find(Name, Dict), Dict}.
