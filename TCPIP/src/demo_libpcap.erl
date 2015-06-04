-module(demo_libpcap).

-compile(export_all).

start() ->
    DriverPid = erlpcap:start("en0"),
    erlpcap:start_capture(65535, 1, 100),
    erlpcap:loop_capture(),
    ok.
