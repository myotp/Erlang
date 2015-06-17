-module(ts_sup).

-include("my_dbg.hrl").

-behaviour(supervisor).

-export([ start_link/1
        , start_child/0
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_child() ->
    supervisor:start_child(?SERVER, []).

start_link(ListenSocket) ->
    ?x(ListenSocket),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ListenSocket]).

init([ListenSocket]) ->
    TcpServer = {ts_server, {ts_server, start_link, [ListenSocket]},
                 temporary, brutal_kill, worker, [ts_server]},
    {ok, {{simple_one_for_one, 0, 1}, [TcpServer]}}.
