-module(time_server).

-compile(export_all).

start() ->
    io:format("going to start time_server application~n"),
    application:start(time_server),
    ts_event_monitor:add_handler(),
    io:format("time_server started~n").
