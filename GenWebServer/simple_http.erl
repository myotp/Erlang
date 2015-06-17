-module(simple_http).

-behaviour(gen_web_server).
%% callback functions for gen_web_server behaviour
-export([ init/1
        , get/3
        ]).

-export([ start/0
        ]).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

pretty_print(M, L, S, V) ->
    io:format("[~12s:~3s] ~-25s = ~p~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

start() ->
    gws_connection_sup:start_link(?MODULE, {127,0,0,1}, 8080, []).

init(InitArg) ->
    ?x(InitArg),
    {ok, []}.

get(Request, Headers, UserData) ->
    ?x(Request),
    ?x(Headers),
    ?x(UserData),
    simple_html().

simple_html() ->
    <<"<html><body><h1>Hello, my web server</h1></body></html>">>.
