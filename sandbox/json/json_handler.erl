-module(json_handler).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behaviour(elli_handler).

handle(Req, Args) ->
    handle(Req#req.method, elli_request:path(Req), Req, Args).

handle('GET', [<<"events">>], _Req, Args) ->
    Events = proplists:get_value(events, Args),
    {ok, [], jsx:encode(Events)};
handle(_, _, _Req, _Args) -> {404, [], <<"Not Found">>}.

handle_event(Event, Data, Args) ->
    io:format("Event=~p~n, Data=~p~n, Args=~p~n",
              [Event, Data, Args]),
    ok.
