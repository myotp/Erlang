-module(plus_event_printer).

-behaviour(gen_event).

-export([ add_handler/0
        , delete_handler/0
        ]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(x(X), pretty_print(?MODULE, ?LINE, ??X, X)).

pretty_print(M, L, S, V) ->
    io:format("[~20s:~3s]~-20s = ~p~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

-record(state, {}).

add_handler() ->
    plus_event:add_handler(?MODULE, []).

delete_handler() ->
    plus_event:delete_handler(?MODULE, []).

init([]) ->
    ?x(init),
    {ok, #state{}}.

handle_event({plus_call, A, B}, State) ->
    io:format("[Event][plus_call] ~p and ~p~n", [A, B]),
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
