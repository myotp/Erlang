-module(pmap_controller2).

-compile(export_all).

-define(MAX_PMAP, 1).
-define(SERVER, ?MODULE).
-define(TABLE, pmap_depth).

start() ->
    Pid = spawn(fun() -> init() end),
    register(?SERVER, Pid).

init() ->
    ets:new(?TABLE, [set, named_table, public, {keypos, 1}]),
    ets:insert(?TABLE, {depth, 0}),
    loop().

stop() ->
    ?SERVER ! stop.

loop() ->
    receive
        stop ->
            ok
    end.

reset_depth() ->
    ets:insert(?TABLE, {depth, 0}).

get_depth() ->
    [{_, Depth}] = ets:lookup(?TABLE, depth),
    Depth.

increase_depth() ->
    ets:update_counter(?TABLE, depth, 1).

decrease_depth() ->
    ets:update_counter(?TABLE, depth, -1).
