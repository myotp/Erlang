-module(mytcp).

-compile(export_all).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

pretty_print(_M, L, S, V) ->
    io:format("[mytcp:~3s]~-20s = ~p~n", [integer_to_list(L), S, V]).

start_server(Port) ->
    %% create a listening socket, by default the reuseaddr is false
    {ok, ListenSocket} = gen_tcp:listen(Port, []),
    {ok, ConnSocket} = gen_tcp:accept(ListenSocket),
    ?x(ConnSocket),
    loop(ConnSocket).

start_server_raw(Port) ->
    {ok, TP} = inet_tcp:getserv(Port),
    ?x(TP),
    {ok, ListenSocket} = inet_tcp:listen(TP, []),
    ?x(ListenSocket),
    {ok, ConnSocket} = inet_tcp:accept(ListenSocket),
    ?x(ConnSocket),
    loop(ConnSocket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("~p got message ~p through ~p~n", [self(), Bin, Socket]),
            gen_tcp:send(Socket, [pid_to_list(self()), " received: ", Bin]),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("== Closed from client side ==~n"),
            gen_tcp:close(Socket),
            ok
    end.
