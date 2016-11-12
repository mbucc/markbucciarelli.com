-module(es_callback).

-export([handle/2, handle_event/3]).
-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

%% Delegate to handler functions
handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"hello">>, <<"world">>], _Req) ->
    {ok, [], <<"Hello World!">>};
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

% Dump error information to stdout.
dump(R, E, S) ->
   io:format("exception thrown for request ~p~nexception=~p~nstacktrace=~p~n", [R, E, S]).

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.

handle_event(requestthrow, [Request, Exception, Stacktrace], _) -> 
   dump(Request, Exception, Stacktrace),
   ok;
handle_event(requesterror, [Request, Exception, Stacktrace], _) ->
   dump(Request, Exception, Stacktrace),
   ok;
handle_event(requestexit, [Request, Exception, Stacktrace], _) ->
   dump(Request, Exception, Stacktrace),
   ok;
handle_event(_Event, _Data, _Args) -> ok.
