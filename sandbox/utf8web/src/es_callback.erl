-module(es_callback).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

%% Delegate to handler functions
handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [], _Req) ->
    {ok, Bin} = file:read_file("www/form.html"), {ok, [], Bin};
handle('POST', [], Req) ->
    Bytes = elli_request:post_arg(<<"utf8">>, Req,
                                  <<"undefined">>),
    {ok, [], Bytes};
handle(_, _, _Req) -> {404, [], <<"Not Found">>}.

handle_event(Event, Data, Args) ->
    io:format("Event=~p~n, Data=~p~n, Args=~p~n",
              [Event, Data, Args]),
    ok.
