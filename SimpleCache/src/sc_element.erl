-module(sc_element).

-behaviour(gen_server).

-export([ start_link/2
        , create/1
        , create/2
        , fetch/1
        , replace/2
        , delete/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_LEASE_TIME, 60). %% 1 Minute

-record(state, { value
               , lease_time
               , start_time
               }).

start_link(Value, LeaseTime) ->
    io:format("[CREATE STEP 3] ~p callback start_link/2 in ~p~n",
              [self(), ?MODULE]),
    %% no registerd name, since its type is simple_one_for_one
    %% many processes from this module
    %% but to play with it, I would like to see some registered name
    case is_list(Value) of
        true ->
            Name = list_to_atom(Value),
            Args = [Value, LeaseTime],
            gen_server:start_link({local, Name}, ?MODULE, Args, []);
        false ->
            gen_server:start_link(?MODULE, [Value, LeaseTime], [])
    end.

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

create(Value, LeaseTime) ->
    io:format("[CREATE STEP 1] ~p API create/2 in ~p Arg1: ~pArg2: ~p~n",
              [self(), ?MODULE, Value, LeaseTime]),
    sc_sup:start_child(Value, LeaseTime).

init([Value, LeaseTime] = Arg) ->
    io:format("[CREATE STEP 4] ~p callback init/1 in ~p Arg:~p~n",
              [self(), ?MODULE, Arg]),
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    Timeout = time_left(StartTime, LeaseTime),
    State = #state{ value = Value
                  , start_time = StartTime
                  , lease_time = LeaseTime
                  },
    {ok, State, Timeout}.

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

handle_call(fetch, _From, #state{ value = Value
                                , start_time = StartTime
                                , lease_time = LeaseTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    Reply = {ok, Value},
    {reply, Reply, State, TimeLeft}.

handle_cast({replace, Value}, #state{ start_time = StartTime
                                    , lease_time = LeaseTime} = State) ->
    TimeLeft = time_left(StartTime, LeaseTime),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    io:format("[STOP STEP 1a] delete~n"),
    {stop, normal, State}.

handle_info(timeout, State) ->
    io:format("[STOP STEP 1b] timeout~n"),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    io:format("[STOP STEP 2] terminate/2~n"),
    sc_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

time_left(_StartTime, infinity) ->
    infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.
