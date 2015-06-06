-module(demo_dets).

-compile(export_all).

run() ->
    {ok, table} = dets:open_file(table, [{file, "abc.dat"}, {keypos, 1}]),
    dets:insert(table, {"jia", 32}),
    dets:insert(table, {"jia", 8888}),
    dets:insert(table, {"vivian", 28}),
    io:format("lookup: ~p~n", [dets:lookup(table, "jia")]),
    io:format("lookup: ~p~n", [dets:lookup(table, "XXXXXXXX")]),
    dets:close(table).

dets_to_list(Dets) ->
    Key = dets:first(Dets),
    dets_to_list(Dets, dets:next(Dets, Key), dets:lookup(Dets, Key)).

dets_to_list(Dets, Key, Acc) when is_list(Key) ->
    [{Key, Value}] = dets:lookup(Dets, Key),
    NextKey = dets:next(Dets, Key),
    dets_to_list(Dets, NextKey, [{Key, Value}|Acc]);
dets_to_list(_, '$end_of_table', Acc) ->
    Acc.
