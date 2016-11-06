-module(es_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

%% Delegate to handler functions
handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    {ok, [], <<"Hello World!">>};

handle('GET',[<<"fail">>], _Req) ->
    _D = 1/0,
    {ok, [], <<"Should not get here.">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
