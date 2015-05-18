-module(pmap_controller).

-compile(export_all).

-define(MAX_PMAP, 1).
-define(SERVER, ?MODULE).

start() ->
    Pid = spawn(fun() -> loop(0) end),
    register(?SERVER, Pid).

stop() ->
    ?SERVER ! stop.

loop(N) ->
    receive
        reset_depth ->
            loop(0);
        stop ->
            ok;
        {From, get_depth} ->
            From ! N,
            loop(N);
        {From, increase_depth} ->
            From ! ok,
            loop(N+1);
        {From, decrease_depth} ->
            From ! ok,
            loop(N-1)
    end.

rpc(Msg) ->
    ?SERVER ! {self(), Msg},
    receive
        Reply ->
            Reply
    end.

reset_depth() ->
    ?SERVER ! reset_depth.

get_depth() ->
    rpc(get_depth).

increase_depth() ->
    rpc(increase_depth).

decrease_depth() ->
    rpc(decrease_depth).

