-module(plus_server).

-behaviour(gen_server).

-export([ plus/2
        , plus_cast/2
        , plus_info/2
        ]).

-export([start/1]).

-export([start/0, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {magic_extra}).

plus(A, B) ->
    gen_server:call(?SERVER, {plus, A, B}).

plus_cast(A, B) ->
    gen_server:cast(?SERVER, {plus_cast, A, B}).

plus_info(A, B) ->
    ?SERVER ! {plus_info, A, B}.

start() ->
    start(0).

start(Extra) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [{magic_extra, Extra}], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Opts) ->
    io:format("Opts: ~p~n", [Opts]),
    Extra = proplists:get_value(magic_extra, Opts, 0),
    {ok, #state{magic_extra = Extra}}.

handle_call({plus, A, B} = Msg, _From, #state{magic_extra = Extra} = State) ->
    io:format("handle call: ~p~n", [Msg]),
    Reply = {ok, A + B + Extra},
    {reply, Reply, State}.

handle_cast({plus_cast, A, B} = Msg, State) ->
    io:format("handle cast: ~p~n", [Msg]),
    io:format("A+B = ~p~n", [A+B]),
    {noreply, State}.

handle_info({plus_info, A, B} = Msg, State) ->
    io:format("handle info: ~p~n", [Msg]),
    io:format("A+B = ~p~n", [A+B]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
