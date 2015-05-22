-module(map_reduce).

-compile(export_all).

%% map_reduce mapper reducer :: [(k, v)] -> [(k2, v2)]
%% mapper :: k -> v -> [(k2, v2)]
%% reducer :: k2 -> [v2] -> [(k2, v2)]

map_reduce_seq(MapF, RedF, Input) ->
    Mapped = [{K2, V2} || {K, V} <- Input,
                          {K2, V2} <- MapF(K, V)],
    io:format("Mapped: ~p~n", [Mapped]),
    reduce_seq(RedF, Mapped).

reduce_seq(RedF, KVs) ->
    [KV || {K, Vs} <- group(lists:sort(KVs)),
           KV <- RedF(K, Vs)].

%% KVs must be sorted already
group(KVs) ->
    group(KVs, [], []).
group([], _, Acc) ->
    Acc;
group([{K, V1}, {K, V2}|Rest], Tmp, Acc) ->
    group([{K, V2}|Rest], Tmp ++ [V1], Acc);
group([{K, V}|Rest], Tmp, Acc) ->
    group(Rest, [], Acc ++ [{K, Tmp ++ [V]}]).

