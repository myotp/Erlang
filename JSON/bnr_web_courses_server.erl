-module(bnr_web_courses_server).

-export([start/1]).
-export([loop_json_server/2]).

-compile(export_all).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

pretty_print(_M, L, S, V) ->
    io:format("[~10s]~-30s = ~p~n", [integer_to_list(L), S, V]).

start(Port) ->
    multi_threaded_echo_server(Port).

%%------------------------------------------------------------------------------
%% multi threaded tcp echo server
%%------------------------------------------------------------------------------
multi_threaded_echo_server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{reuseaddr, true}]),
    ?x(self()),
    ?x(ListenSocket),
    Json = bnr_json_data(),
    do_listen_loop(ListenSocket, Json).

%% Whenever a new connection, spawn a new worker to handle this connection
do_listen_loop(Socket, Json) ->
    io:format("== ~p listening on ~p~n", [self(), Socket]),
    {ok, ConnSocket} = gen_tcp:accept(Socket),
    ?x(ConnSocket),
    EchoWorker= spawn(?MODULE, loop_json_server, [ConnSocket, Json]),
    ?x(EchoWorker),
    gen_tcp:controlling_process(ConnSocket, EchoWorker),
    do_listen_loop(Socket, Json).

loop_json_server(Socket, Json) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("~p got message through ~p~n~s~n", [self(), Socket, Bin]),
            RandomSleepTime = random:uniform(3000),
            io:format("To make it interesting, I will sleep ~p~n",
                      [RandomSleepTime]),
            timer:sleep(RandomSleepTime),
            JsonReply = mochijson:encode(Json),
            Size = iolist_size(JsonReply),
            Reply = [http_response_header(Size), JsonReply],
            gen_tcp:send(Socket, Reply),
            loop_json_server(Socket, Json);
        {tcp_closed, Socket} ->
            io:format("== Closed from client side ==~n"),
            ok
    end.

bnr_json_data() ->
    mochijson:decode(raw_bnr_json_data()).

raw_bnr_json_data() ->
    {ok, Bin} = file:read_file("BNR_json_response.dat"),
    Bin.

http_response_header(Length) when is_integer(Length) ->
    http_response_header(integer_to_list(Length));
http_response_header(Length) when is_list(Length) ->
    "HTTP/1.1 200 OK\r\n"
    "Content-Length: " ++ Length ++ "\r\n"
    "\r\n".
