-module(sc_sup).

-behaviour(supervisor).

-export([ start_link/0
        , start_child/2
        ]).

-export([ init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    io:format("[CREATE STEP 2] ~p start_child/2 in ~p~n", [self(), ?MODULE]),
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

init([]) ->
    Element = {sc_element, {sc_element, start_link, []},
               temporary, brutal_kill, worker, [sc_element]},
    {ok, {{simple_one_for_one, 0, 1}, [Element]}}.
