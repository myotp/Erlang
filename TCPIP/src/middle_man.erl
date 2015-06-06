-module(middle_man).

-include("debug.hrl").

-behaviour(gen_server).

-export([ request/4
        , reply/3
        ]).

-export([ start_link/1
        , start_link/2
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { retry = 5
               , retry_interval = 2000
               , timeout = 20000
               , req = []
               }).

-record(req, { args
             , ref
             , requesters
             , middle_man
             }).

%% API
start_link(Name) ->
    start_link(Name, []).

start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, Config, []).

request(Name, M, F, A) ->
    ?log("~p ~p start request ~p~n", [self(), Name, A]),
    gen_server:cast(Name, {request, self(), M, F, A}),
    receive
        Reply ->
            Reply
    end.

reply(Name, A, Reply) ->
    ?log("~p got ~p for request ~p~n", [Name, Reply, A]),
    gen_server:cast(Name, {reply, A, Reply}).

%% gen_server callback functions
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

start_middle_man(M,F,A,Retry,TimeInterval) when Retry > 0 ->
    apply(M, F, A),
    receive
        stop ->
            ok
    after TimeInterval ->
            start_middle_man(M,F,A,Retry-1,TimeInterval)
    end;
start_middle_man(_,_,_,0,_) ->
    ok.

handle_cast({request, From, M, F, A}, #state{ retry = Retry
                                            , retry_interval = TimeInterval
                                            , timeout = Timeout
                                            , req = ReqList} = State) ->
    %% FIXME, same MFA should only one entry?
    {ok, Ref} = timer:send_after(Timeout, {timeout_by_timer, A}),
    Pid = spawn_link(fun() -> ?log("middle man started : ~p~n", [self()]),
                              start_middle_man(M,F,A,Retry,TimeInterval) end),
    Req = #req{ args = A
              , ref = Ref
              , requesters = [From]
              , middle_man = Pid
              },
    {noreply, State#state{req = [Req|ReqList]}};
handle_cast({reply, A, Reply}, #state{req = ReqList} = State) ->
    NewReqList = case lists:keysearch(A, #req.args, ReqList) of
                     {value, #req{ middle_man = MiddleMan
                                   , ref = Ref
                                   , requesters = Rs}} ->
                         timer:cancel(Ref),
                         MiddleMan ! stop,
                         lists:foreach(fun(Pid) -> Pid ! Reply end, Rs),
                         lists:keydelete(A, #req.args, ReqList);
                     false ->
                         ReqList
    end,

    {noreply, State#state{req = NewReqList}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout_by_timer, A}, #state{req = ReqList} = State) ->
    Reply = {error, timeout},
    NewReqList = case lists:keysearch(A, #req.args, ReqList) of
                     {value, #req{ middle_man = MiddleMan
                                 , requesters = Rs}} ->
                         lists:foreach(fun(Pid) -> Pid ! Reply end, Rs),
                         MiddleMan ! stop,
                         lists:keydelete(A, #req.args, ReqList);
                     false ->
                         ReqList
    end,
    {noreply, State#state{req = NewReqList}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
