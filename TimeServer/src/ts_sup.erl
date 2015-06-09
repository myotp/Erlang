-module(ts_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    Listener = {ts_listen_server, {ts_listen_server, start_link, []},
                Restart, Shutdown, worker, [ts_listen_server]},
    HttpSup = {ts_http_sup, {ts_http_sup, start_link, []},
                Restart, Shutdown, supervisor, [ts_http_sup]},
    {ok, {SupFlags, [Listener, HttpSup]}}.
