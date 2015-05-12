-module(demo_json).

-compile(export_all).

simple_json() ->
    {struct, [{"name", "Wang"},
              {"age", 99},
              {"languages", {array, ["Python", "Erlang", "Java", "Objective-C"]}},
              {"id", 888}]}.

run() ->
    Bin = erlang:iolist_to_binary(mochijson:encode(simple_json())),
    io:format("Bin: ~p~n", [Bin]).
