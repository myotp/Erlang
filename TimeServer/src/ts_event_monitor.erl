-module(ts_event_monitor).

-behaviour(gen_event).

-export([ add_handler/0
        , delete_handler/0
        ]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RESET_INTERVAL, 60).
-define(MAX_REQ_PER_RESET_INTERVAL, 5).

-record(state, { count
               , start_time}).

add_handler() ->
    ts_event:add_handler(?MODULE, []).

delete_handler() ->
    ts_event:delete_handler(?MODULE, []).

init([]) ->
    {ok, #state{ count = 0
               , start_time = current_seconds()}}.

handle_event({new_connection, _Pid, _IP}, #state{count = N,
                                                 start_time = Start} = State) ->
    Now = current_seconds(),
    case Now - Start >= ?RESET_INTERVAL of
        true ->
            {ok, #state{count = 0, start_time = Now}};
        false ->
            case N > ?MAX_REQ_PER_RESET_INTERVAL of
                true ->
                    io:format("!!! too many requests, STOP!!!~n"),
                    application:stop(time_server),
                    {ok, State};
                false ->
                    {ok, State#state{count = N+1}}
            end
    end.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

current_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now())).
