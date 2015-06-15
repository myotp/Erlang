%%%-------------------------------------------------------------------
%%% @author Jia Wang
%%% @doc
%%% my supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(plus_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% init/1 callback function
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    PlusServer = {plus_server, {plus_server, start_link, []},
                  Restart, Shutdown, Type, [plus_server]},

    EventManager = {plus_event, {plus_event, start_link, []},
                    Restart, Shutdown, Type, [plus_event]},

    {ok, {SupFlags, [PlusServer, EventManager]}}.
