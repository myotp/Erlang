%% ${ERL_TOP}/erts/preloaded/src/prim_inet.erl
-module(prim_inet).

%% Primitive inet_drv interface

-export([open/3, fdopen/4, close/1]).
-export([bind/3, listen/1, listen/2, peeloff/2]).
-export([connect/3, connect/4, async_connect/4]).
-export([accept/1, accept/2, async_accept/2]).
-export([shutdown/2]).
-export([send/2, send/3, sendto/4, sendmsg/3]).
-export([recv/2, recv/3, async_recv/3]).
-export([unrecv/2]).
-export([recvfrom/2, recvfrom/3]).
-export([setopt/3, setopts/2, getopt/2, getopts/2, is_sockopt_val/2]).
-export([chgopt/3, chgopts/2]).
-export([getstat/2, getfd/1, ignorefd/2,
	 getindex/1, getstatus/1, gettype/1,
	 getifaddrs/1, getiflist/1, ifget/3, ifset/3,
	 gethostname/1]).
-export([getservbyname/3, getservbyport/3]).
-export([peername/1, setpeername/2]).
-export([sockname/1, setsockname/2]).
-export([attach/1, detach/1]).

-include("inet_sctp.hrl").
-include("inet_int.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% OPEN(tcp | udp | sctp, inet | inet6, stream | dgram | seqpacket)  ->
%%       {ok, insock()} |
%%       {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open(Protocol, Family, Type) ->
    open(Protocol, Family, Type, ?INET_REQ_OPEN, []).

fdopen(Protocol, Family, Type, Fd) when is_integer(Fd) ->
    open(Protocol, Family, Type, ?INET_REQ_FDOPEN, ?int32(Fd)).

open(Protocol, Family, Type, Req, Data) ->
    Drv = protocol2drv(Protocol),
    AF = enc_family(Family),
    T = enc_type(Type),
    try erlang:open_port({spawn_driver,Drv}, [binary]) of
	S ->
	    case ctl_cmd(S, Req, [AF,T,Data]) of
		{ok,_} -> {ok,S};
		{error,_}=Error ->
		    close(S),
		    Error
	    end
    catch
	%% The only (?) way to get here is to try to open
	%% the sctp driver when it does not exist (badarg)
	error:badarg       -> {error, eprotonosupport};
	%% system_limit if out of port slots
	error:system_limit -> {error, system_limit}
    end.

enc_family(inet) -> ?INET_AF_INET;
enc_family(inet6) -> ?INET_AF_INET6.

enc_type(stream) -> ?INET_TYPE_STREAM;
enc_type(dgram) -> ?INET_TYPE_DGRAM;
enc_type(seqpacket) -> ?INET_TYPE_SEQPACKET.

protocol2drv(tcp)  -> "tcp_inet";
protocol2drv(udp)  -> "udp_inet";
protocol2drv(sctp) -> "sctp_inet".

drv2protocol("tcp_inet")  -> tcp;
drv2protocol("udp_inet")  -> udp;
drv2protocol("sctp_inet") -> sctp;
drv2protocol(_)           -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BIND(insock(), IP, Port) -> {ok, integer()} | {error, Reason}
%%
%% bind the insock() to the interface address given by IP and Port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bind(S,IP,Port) when is_port(S), is_integer(Port), Port >= 0, Port =< 65535 ->
    case ctl_cmd(S,?INET_REQ_BIND,enc_value(set, addr, {IP,Port})) of
	{ok, [P1,P0]} -> {ok, ?u16(P1, P0)};
	{error,_}=Error -> Error
    end;

%% Multi-homed "bind": sctp_bindx(). The Op is 'add' or 'remove'.
%% If no addrs are specified, it just does nothing.
%% Function returns {ok, S} on success, unlike TCP/UDP "bind":
bind(S, Op, Addrs) when is_port(S), is_list(Addrs) ->
    case Op of
	add ->
	    bindx(S, 1, Addrs);
	remove ->
	    bindx(S, 0, Addrs);
	_ -> {error, einval}
    end;
bind(_, _, _) -> {error, einval}.

bindx(S, AddFlag, Addrs) ->
    case getprotocol(S) of
	sctp ->
	    %% Really multi-homed "bindx". Stringified args:
	    %% [AddFlag, (AddrBytes see enc_value_2(addr,X))+]:
	    Args =
		[?int8(AddFlag)|
		 [enc_value(set, addr, {IP,Port}) ||
		     {IP, Port} <- Addrs]],
	    case ctl_cmd(S, ?SCTP_REQ_BINDX, Args) of
		{ok,_} -> {ok, S};
		{error,_}=Error  -> Error
	    end;
	_ -> {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CONNECT(insock(), IP, Port [,Timeout]) -> ok | {error, Reason}
%%
%% connect the insock() to the address given by IP and Port
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate connect (mostly works for loopback)
%%               > 0  -> wait for timout ms if not connected then 
%%                       return {error, timeout} 
%%
%% ASYNC_CONNECT(insock(), IP, Port, Timeout) -> {ok, S, Ref} | {error, Reason}
%%
%%  a {inet_async,S,Ref,Status} will be sent on socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP, UDP or SCTP sockets.
%%
connect(S, IP, Port) -> connect0(S, IP, Port, -1).

connect(S, IP, Port, infinity) -> connect0(S, IP, Port, -1);
connect(S, IP, Port, Time)     -> connect0(S, IP, Port, Time).

connect0(S, IP, Port, Time) when is_port(S), Port > 0, Port =< 65535,
				 is_integer(Time) ->
    case async_connect(S, IP, Port, Time) of
	{ok, S, Ref} ->
	    receive
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error -> Error
    end.

async_connect(S, IP, Port, Time) ->
    case ctl_cmd(S, ?INET_REQ_CONNECT,
		 [enc_time(Time),?int16(Port),ip_to_bytes(IP)]) of
	{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
	{error,_}=Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ACCEPT(insock() [,Timeout] ) -> {ok,insock()} | {error, Reason}
%%
%% accept incoming connection on listen socket 
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate accept (poll)
%%               > 0  -> wait for timout ms for accept if no accept then 
%%                       return {error, timeout}
%%
%% ASYNC_ACCEPT(insock(), Timeout)
%%
%%  async accept. return {ok,S,Ref} or {error, Reason}
%%  the owner of socket S will receive an {inet_async,S,Ref,Status} on 
%%  socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP sockets only.
%%
accept(L)            -> accept0(L, -1).

accept(L, infinity)  -> accept0(L, -1);
accept(L, Time)      -> accept0(L, Time).

accept0(L, Time) when is_port(L), is_integer(Time) ->
    case async_accept(L, Time) of
	{ok, Ref} ->
	    receive 
		{inet_async, L, Ref, {ok,S}} ->
		    accept_opts(L, S);
		{inet_async, L, Ref, Error} ->
		    Error
	    end;
	Error -> Error
    end.

%% setup options from listen socket on the connected socket
accept_opts(L, S) ->
    case getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case setopts(S, Opts) of
		ok -> {ok, S};
		Error -> close(S), Error
	    end;
	Error ->
	    close(S), Error
    end.

async_accept(L, Time) ->
    case ctl_cmd(L,?INET_REQ_ACCEPT, [enc_time(Time)]) of
	{ok, [R1,R0]} -> {ok, ?u16(R1,R0)};
	{error,_}=Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LISTEN(insock() [,Backlog]) -> ok | {error, Reason}
%%
%% set listen mode on socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% For TCP or SCTP sockets. For SCTP, Boolean backlog value (enable/disable
%% listening) is also accepted:

listen(S) -> listen(S, ?LISTEN_BACKLOG).

listen(S, true) -> listen(S, ?LISTEN_BACKLOG);
listen(S, false) -> listen(S, 0);
listen(S, BackLog) when is_port(S), is_integer(BackLog) ->
    case ctl_cmd(S, ?INET_REQ_LISTEN, [?int16(BackLog)]) of
	{ok, _} -> ok;
	{error,_}=Error   -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SEND(insock(), Data) -> ok | {error, Reason}
%%
%% send Data on the socket (io-list)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is a generic "port_command" interface used by TCP, UDP, SCTP, depending
%% on the driver it is mapped to, and the "Data". It actually sends out data,--
%% NOT delegating this task to any back-end.  For SCTP, this function MUST NOT
%% be called directly -- use "sendmsg" instead:
%%
send(S, Data, OptList) when is_port(S), is_list(OptList) ->
    ?DBG_FORMAT("prim_inet:send(~p, ~p)~n", [S,Data]),
    try erlang:port_command(S, Data, OptList) of
	false -> % Port busy and nosuspend option passed
	    ?DBG_FORMAT("prim_inet:send() -> {error,busy}~n", []),
	    {error,busy};
	true ->
	    receive
		{inet_reply,S,Status} ->
		    ?DBG_FORMAT("prim_inet:send() -> ~p~n", [Status]),
		    Status
	    end
    catch
	error:_Error ->
	    ?DBG_FORMAT("prim_inet:send() -> {error,einval}~n", []),
	     {error,einval}
    end.

send(S, Data) ->
    send(S, Data, []).


%% Control command
ctl_cmd(Port, Cmd, Args) ->
    ?DBG_FORMAT("prim_inet:ctl_cmd(~p, ~p, ~p)~n", [Port,Cmd,Args]),
    Result =
	try erlang:port_control(Port, Cmd, Args) of
	    [?INET_REP_OK|Reply]  -> {ok,Reply};
	    [?INET_REP]  -> inet_reply;
	    [?INET_REP_ERROR|Err] -> {error,list_to_atom(Err)}
	catch
	    error:_               -> {error,einval}
	end,
        ?DBG_FORMAT("prim_inet:ctl_cmd() -> ~p~n", [Result]),
    Result.
