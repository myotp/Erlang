-module(gws_connection_sup).

-behaviour(supervisor).

-export([ start_link/4
        , start_child/1
        ]).

%% Supervisor callbacks
-export([ init/1
        ]).

%%% API functions
start_link(CallBackMod, IP, Port, UserArgs) ->
    Args = [CallBackMod, IP, Port, UserArgs],
    {ok, Pid} = supervisor:start_link(?MODULE, Args),
    io:format("supervisor started at ~p~n", [Pid]),
    start_child(Pid),
    {ok, Pid}.

start_child(Server) ->
    supervisor:start_child(Server, []).

%%% Supervisor callbacks
init([CallBackMod, IP, Port, UserArgs]) ->
    BasicSocketOpts = [binary,
                       {active, false},
                       {packet, http_bin},
                       {reuseaddr, true}],
    SocketOpts = case IP of
                     undefined -> BasicSocketOpts;
                     _ -> [{ip, IP} | BasicSocketOpts]
                 end,
    {ok, ListenSocket} = gen_tcp:listen(Port, SocketOpts),
    ChildArgs = [CallBackMod, ListenSocket, UserArgs],
    Server = {gws_server, {gws_server, start_link, ChildArgs},
              temporary, brutal_kill, worker, [gws_server]},
    {ok, {{simple_one_for_one, 1000, 3600}, [Server]}}.

