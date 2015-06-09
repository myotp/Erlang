-module(ts_http_sup).

-behaviour(supervisor).

-export([start_child/0]).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_child() ->
    supervisor:start_child(?SERVER, []).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    HttpServer = {ts_http_server, {ts_http_server, start_link, []},
                  temporary, brutal_kill, worker, [ts_http_server]},
    {ok, {{simple_one_for_one, 0, 1}, [HttpServer]}}.
