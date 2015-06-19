-module(ts_server).

-include("my_dbg.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, { listen_socket
               }).


start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, [ListenSocket], []).

init([ListenSocket]) ->
    {ok, #state{listen_socket = ListenSocket}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    ?px(Data),
    NewState = handle_data(Socket, Data, State),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    ?x(tcp_closed),
    {stop, normal, State};
handle_info(timeout, #state{listen_socket = ListenSocket} = State) ->
    io:format("~p waiting for new connection.~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    ?x(Socket),
    ?x(ts_sup:start_child()),
    {noreply, State}.

terminate(Reason, State) ->
    ?x(Reason),
    ?x(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_data(Socket, Data, State) ->
    gen_tcp:send(Socket, [<<"recv:">>, Data]),
    State.
