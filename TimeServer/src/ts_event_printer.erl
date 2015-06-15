-module(ts_event_printer).

-behaviour(gen_event).

-export([ add_handler/0
        , delete_handler/0
        ]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

add_handler() ->
    ts_event:add_handler(?MODULE, []).

delete_handler() ->
    ts_event:delete_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event({new_connection, Pid, IP}, State) ->
    io:format("[New] ~p for ~p~n", [Pid, IP]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
