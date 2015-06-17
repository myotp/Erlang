-module(ts_app).

-include("my_dbg.hrl").

-behaviour(application).

-export([ start/2
        , stop/1
        ]).

-define(DEFAULT_PORT, 8080).

start(StartType, StartArgs) ->
    ?x(StartType),
    ?x(StartArgs),
    Port = case application:get_env(tcp_server, port) of
               {ok, P} -> P;
               undefined -> ?DEFAULT_PORT
           end,
    ?x(Port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}]),
    case ts_sup:start_link(ListenSocket) of
        {ok, Pid} ->
            ?x(Pid),
            io:format("start first child immediately~n"),
            ?x(ts_sup:start_child()),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(State) ->
    ?x(State),
    ok.
