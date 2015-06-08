%%%-------------------------------------------------------------------
%%% @author Jia Wang
%%% @doc
%%% my application
%%% @end
%%%-------------------------------------------------------------------
-module(plus_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% application callback start/2
%% @spec start(StartType, StartArgs) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case plus_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
