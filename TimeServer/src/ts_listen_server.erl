-module(ts_listen_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { socket
               }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [{reuseaddr, true}]),
    {ok, #state{socket = ListenSocket}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{socket = Socket} = State) ->
    {ok, ConnSocket} = gen_tcp:accept(Socket),
    {ok, Worker} = ts_http_sup:start_child(),
    gen_tcp:controlling_process(ConnSocket, Worker),
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
