-module(simple_cache).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%%========== API ==============================================================
-export([ new/1
        , put/3
        , put/4
        , get/2
        , stop/1
        ]).

-export([ to_list/1
        , print_state/1
        ]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 30000).

-record(state, { cache_table
               }).

%%========== API ==============================================================
new(Name) ->
    start_link(Name).

stop(Name) ->
    gen_server:call(Name, stop).

put(Cache, Key, Value) ->
    ?MODULE:put(Cache, Key, Value, ?DEFAULT_TIMEOUT).

put(Cache, Key, Value, Timeout) ->
    gen_server:cast(Cache, {put, Key, Value, Timeout}).

get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}).

to_list(Name) ->
    gen_server:call(Name, to_list).

print_state(Name) ->
    gen_server:cast(Name, print_state).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
    T = ets:new(Name, [public]),
    {ok, #state{cache_table = T}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(to_list, _From, #state{cache_table = Table} = State) ->
    List = cache_table_to_list(Table),
    {reply, List, State};
handle_call({get, Key}, _From, #state{cache_table = Table} = State) ->
    Reply = case find(Table, Key) of
                {ok, {_Key, Value, _Ref}} ->
                    {ok, Value};
                {error, not_found} ->
                    {error, not_found}
            end,
    {reply, Reply, State}.

handle_cast({put, Key, Value, Timeout}, #state{cache_table = T} = State) ->
    {ok, Ref} = timer:send_after(Timeout, self(), {timeout, Key}),
    maybe_cancel_timer(T, Key),
    ets:insert(T, {Key, Value, Ref}),
    {noreply, State};
handle_cast(print_state, #state{cache_table = T} = State) ->
    io:format("Cache ~p: ~n~p~n", [T, cache_table_to_list(T)]),
    {noreply, State}.

cache_table_to_list(Table) ->
    [{K,V} || {K,V,_Ref} <- ets:tab2list(Table)].

find(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            {error, not_found};
        [Obj] ->
            {ok, Obj}
    end.

maybe_cancel_timer(T, Key) ->
    case find(T, Key) of
        {error, not_found} ->
            ok;
        {ok, {_Key, _OldValue, Ref}} ->
            timer:cancel(Ref)
    end.

handle_info({timeout, Key}, #state{cache_table = T} = State) ->
    ets:delete(T, Key),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, #state{cache_table = T} = _State) ->
    ets:delete(T),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Eunit test cases
start_and_stop_test() ->
    {ok, Pid} = simple_cache:new(test_cache),
    ?assertEqual(Pid, whereis(test_cache)),
    simple_cache:stop(test_cache),
    ?assertEqual(undefined, whereis(test_cache)),
    ok.

put_and_get_test() ->
    simple_cache:new(test_cache),
    ?assertEqual({error, not_found}, simple_cache:get(test_cache, age)),
    simple_cache:put(test_cache, age, 35, 2000),
    ?assertEqual({ok, 35}, simple_cache:get(test_cache, age)),
    simple_cache:put(test_cache, age, 88, 2000),
    ?assertEqual({ok, 88}, simple_cache:get(test_cache, age)),
    timer:sleep(3000),
    ?assertEqual({error, not_found}, simple_cache:get(test_cache, age)),
    simple_cache:stop(test_cache),
    ok.
