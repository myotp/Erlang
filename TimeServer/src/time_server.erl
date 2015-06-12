-module(time_server).

-compile(export_all).

start() ->
    io:format("going to start time_server application~n"),
    application:start(time_server),
    io:format("time_server started~n").
