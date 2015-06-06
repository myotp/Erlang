-module(plus_server).

-behaviour(gen_server).

-export([plus/2]).
-export([start/1]).

-export([start/0, start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {magic_extra}).

plus(A, B) ->
    gen_server:call(?SERVER, {plus, A, B}).

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

handle_call({plus, A, B}, _From, #state{magic_extra = Extra} = State) ->
    Reply = {ok, A + B + Extra},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
