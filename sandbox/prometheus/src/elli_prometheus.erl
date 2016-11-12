%% @doc Exposes HTTP responses and timings as Prometheus metrics.
%%
%% Defines two metrics:
%%
%%        * the counter http_requests_total, with the label statuscode.
%%
%%        * the histogram http_response_milliseconds
%%
%% The duration measures the time, in milliseconds, between when
%% the request line was received and the response was sent. This
%% is as close we can get to the actual time of the request as
%%  seen by the user.

-module(elli_prometheus).

-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2, handle_event/3]).

% Expose /metrics for Prometheus to pull.
handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"metrics">>], _Req) ->
    {ok, [], prometheus_text_format:format()};
% All other requests are passed to normal handler.
handle(_Verb, _Path, _Req) -> ignore.

% Return path, minus any query string, as a binary.
rawpath(#req{raw_path = Path}) ->
    case binary:split(Path, [<<"?">>]) of
      [P, _] -> P;
      [P] -> P
    end.

handle_event(request_complete,
             [Req, ResponseCode, _ResponseHeaders, _ResponseBody,
              Timings],
             _Config) ->
    prometheus_counter:inc(http_requests_total,
                           [ResponseCode, rawpath(Req)]),
    RequestStart = proplists:get_value(request_start,
                                       Timings),
    RequestEnd = proplists:get_value(request_end, Timings),
    Microseconds = timer:now_diff(RequestEnd, RequestStart),
    prometheus_histogram:observe(response_time_in_microseconds, [rawpath(Req)], Microseconds),
    ok;
handle_event(chunk_complete,
             [Req, ResponseCode, ResponseHeaders, _ClosingEnd,
              Timings],
             Config) ->
    handle_event(request_complete,
                 [Req, ResponseCode, ResponseHeaders, <<>>, Timings],
                 Config);
handle_event(Event, [Req, _Exc, _Stack], _Config)
    when Event =:= request_throw;
         Event =:= request_exit;
         Event =:= request_error;
         Event =:= request_parse_error;
         Event =:= bad_request;
         Event =:= client_closed;
         Event =:= client_timeout ->
    prometheus_counter:inc(http_requests_total,
                           [Event, rawpath(Req)]),
    ok;
handle_event(elli_startup, [], _Config) ->
    prometheus_counter:new([{name, http_requests_total},
                            {help, "Total HTTP requests"},
                            {labels, [status, path]}]),
    prometheus_histogram:new([{name,
                               response_time_in_microseconds},
                              {labels, [path]},
                              {buckets, [100, 250, 500, 750, 1000]},
                              {help,
                               "Microseconds between request receipt "
                               "and response send."}]),
    ok;
handle_event(_, _, _) ->
    %% Future-proof.
    ok.
