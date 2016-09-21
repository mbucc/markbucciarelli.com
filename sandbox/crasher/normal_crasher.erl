-module(normal_crasher).
-compile([export_all]).

countdown(N) when N == 0 ->
   exit(normal);

countdown(N) when N > 0 ->
    io:format("child~p~n", [N]),
    timer:sleep(1000),
    countdown(N - 1).
