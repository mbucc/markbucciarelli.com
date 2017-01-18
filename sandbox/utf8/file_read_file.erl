% Try reading utf8 file with file:read_file/1.
-module(file_read_file).

-export([start/0]).

dump(Bin) ->
    io:format("~s",
              [[io_lib:format("~2.16.0B~n", [X]) || <<X:8>> <= Bin]]).

start() ->
    {ok, Bin} = file:read_file("utf8.txt"), dump(Bin).
