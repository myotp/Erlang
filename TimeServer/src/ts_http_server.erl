-module(ts_http_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    io:format("~p:start_link/0~n", [?MODULE]),
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, _Bin}, State) ->
    io:format("~p got message through ~p~n", [self(), Socket]),
    Html = time_http_reply(),
    Header = http_header(lists:flatlength(Html)),
    gen_tcp:send(Socket, [Header, Html]),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    io:format("~p stopped~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

http_header(Length) ->
"HTTP/1.1 200 OK\r\n"
"Content-Type: html\r\n"
"Content-Length: " ++ integer_to_list(Length) ++ "\r\n"
"\r\n".

time_http_reply() ->
    ["<html><head>time</head><body>Time: ",
     io_lib:format("~p ~p~n", [date(), time()]),
     "</body></html>"].
