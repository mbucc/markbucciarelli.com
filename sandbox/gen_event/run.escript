#! /usr/bin/env escript

main(_Argv) ->
    {ok, _Pid} = gen_event:start_link({local, event_dispatcher}),
    gen_event:add_handler(event_dispatcher, counter, []),
    Event = {an, event, {can, [be, any, erlang]}, term},
    ok = gen_event:notify(event_dispatcher, Event),
    Request = {so, can, the, call, request},
    Count = gen_event:call(event_dispatcher, counter, Request),
    io:format("Event count = ~w~n", [Count]).
