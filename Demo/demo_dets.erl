-module(demo_dets).

-compile(export_all).

run() ->
    {ok, table} = dets:open_file(table, [{file, "abc.dat"}, {keypos, 1}]),
    dets:insert(table, {"jia", 32}),
    dets:insert(table, {"vivian", 28}),
    io:format("lookup: ~p~n", [dets:lookup(table, "jia")]),
    dets:close(table).
