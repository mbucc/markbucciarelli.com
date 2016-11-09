-module(es_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

%% Delegate to handler functions
handle(Req, _Args) ->
    prometheus_counter:inc(http_requests_total),
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"metrics">>], _Req) ->
    {ok, [], prometheus_text_format:format()};

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    {ok, [], <<"Hello World!">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
