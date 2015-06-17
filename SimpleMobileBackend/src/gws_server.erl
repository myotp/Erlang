-module(gws_server).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { listen_socket
               , socket
               , request_line
               , headers = []
               , body = <<>>
               , content_remaining = 0
               , callback
               , user_data
               , parent
               }).

%%% API
start_link(CallBackMod, ListenSocket, UserArgs) ->
    %% Run in supervisor's context
    io:format("gws_server:start_link/3~n"),
    {ok, UserData} = CallBackMod:init(UserArgs),
    Args = [CallBackMod, ListenSocket, UserData, self()],
    io:format("start gws_server with args: ~p~n", [Args]),
    gen_server:start_link(?MODULE, Args, []).

%%% gen_server callbacks
init([CallBackMod, ListenSocket, UserData, Parent]) ->
    State = #state{ listen_socket = ListenSocket
                  , callback = CallBackMod
                  , user_data = UserData
                  , parent = Parent
                  },
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, _Socket, {http_request, _Method, _Path, _Version} = Req},
            State) ->
    io:format("~p REQ: ~p~n", [self(), Req]),
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, State#state{request_line = Req}};

handle_info({http, _Socket, {http_header, _, Name, _, Value}} = _H, State) ->
    %%io:format("~p Header: ~p~n", [self(), H]),
    %% handle header
    inet:setopts(State#state.socket, [{active, once}]),
    NewState = handle_header(Name, Value, State),
    {noreply, NewState};

handle_info({http, _Socket, http_eoh}, #state{content_remaining = 0} = State) ->
    io:format("eoh and no body~n"),
    %% end of header and no body
    %% handle request, send reply and stop this process
    NewState = handle_http_request(State),
    {stop, normal, NewState};
handle_info({http, _Socket, http_eoh}, State) ->
    io:format("eoh and with body~n"),
    %% end of header and with body
    %% change packet type to raw to receive whole body part
    inet:setopts(State#state.socket, [{active, once}, {packet, raw}]),
    {noreply, State};

handle_info({tcp, _Socket, Data}, State) when is_binary(Data) ->
    %% receive body part
    ContentRem = State#state.content_remaining - byte_size(Data),
    Body = list_to_binary([State#state.body, Data]), % connect body
    NewState = State#state{body = Body,
                           content_remaining = ContentRem},
    if ContentRem > 0 ->
            %% something left
            inet:setopts(State#state.socket, [{active, once}]),
            {noreply, NewState};
       true ->
            %% everything received
            NewState2 = handle_http_request(NewState),
            {stop, normal, NewState2}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    %% Closed by client
    {stop, normal, State};

handle_info(timeout, #state{ listen_socket = ListenSocket
                           , parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gws_connection_sup:start_child(Parent),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
handle_header('Content-Length' = Name, Value, State) ->
    io:format("header length: ~p ~p~n", [Name, Value]),
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength,
                headers = [{Name, Value} | State#state.headers]};
handle_header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
    io:format("expect 100 continue, so send it...~n"),
    %% send 100-continue to make client start upload files
    gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
    State#state{headers = [{Name, Value} | State#state.headers]};
handle_header(Name, Value, State) ->
    io:format("header [~p] => [~p] ~n", [Name, Value]),
    State#state{headers = [{Name, Value} | State#state.headers]}.

handle_http_request(#state{ callback = CallBackMod
                          , request_line = Request
                          , headers = Headers
                          , body = Body
                          , user_data = UserData
                          } = State) ->
    io:format("handle http request...~p~n", [Request]),
    {http_request, Method, _, _} = Request,
    Reply = dispatch(Method, Request, Headers, Body, CallBackMod, UserData),
    gen_tcp:send(State#state.socket, Reply),
    State.

dispatch('GET', Request, Headers, _Body, CallBackMod, UserData) ->
    CallBackMod:get(Request, Headers, UserData);
dispatch('POST', Request, Headers, Body, CallBackMod, UserData) ->
    CallBackMod:post(Request, Headers, Body, UserData).


