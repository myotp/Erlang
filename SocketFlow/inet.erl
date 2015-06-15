-module(inet).

-include("inet.hrl").
-include("inet_int.hrl").
-include("inet_sctp.hrl").

%% socket
-export([peername/1, sockname/1, port/1, send/2,
	 setopts/2, getopts/2, 
	 getifaddrs/0, getifaddrs/1,
	 getif/1, getif/0, getiflist/0, getiflist/1,
	 ifget/3, ifget/2, ifset/3, ifset/2,
	 getstat/1, getstat/2,
	 ip/1, stats/0, options/0, 
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1]).

-export([connect_options/2, listen_options/2, udp_options/2, sctp_options/2]).

-export([i/0, i/1, i/2]).

-export([getll/1, getfd/1, open/8, fdopen/6]).

-export([tcp_controlling_process/2, udp_controlling_process/2,
	 tcp_close/1, udp_close/1]).

%% used by sendfile
-export([lock_socket/2]).

%% used by socks5
-export([setsockname/2, setpeername/2]).

%% resolve
-export([gethostbyname/1, gethostbyname/2, gethostbyname/3, 
	 gethostbyname_tm/3]).
-export([gethostbyname_string/2, gethostbyname_self/2]).
-export([gethostbyaddr/1, gethostbyaddr/2, 
	 gethostbyaddr_tm/2]).

-export([getservbyname/2, getservbyport/2]).
-export([getaddrs/2, getaddrs/3, getaddrs_tm/3,
	 getaddr/2, getaddr/3, getaddr_tm/3]).
-export([translate_ip/2]).

-export([get_rc/0]).

close(Socket) ->
    prim_inet:close(Socket),
    receive
	{Closed, Socket} when Closed =:= tcp_closed; Closed =:= udp_closed ->
	    ok
    after 0 ->
	    ok
    end.


peername(Socket) -> 
    prim_inet:peername(Socket).

gethostname() ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    {ok,Res} = gethostname(U),
	    inet_udp:close(U),
	    {Res2,_} = lists:splitwith(fun($.)->false;(_)->true end,Res),
	    {ok, Res2};
	_ ->
	    {ok, "nohost.nodomain"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:listen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen_options() ->
    [tos, priority, reuseaddr, keepalive, linger, sndbuf, recbuf, nodelay,
     header, active, packet, buffer, mode, deliver, backlog,
     exit_on_close, high_watermark, low_watermark, bit8, send_timeout,
     send_timeout_close, delay_send, packet_size,raw].

listen_options(Opts, Family) ->
    BaseOpts = 
	case application:get_env(kernel, inet_default_listen_options) of
	    {ok,List} when is_list(List) ->
		NList = [{active, true} | lists:keydelete(active,1,List)],		       
		#listen_opts{ opts = NList};
	    {ok,{active,_Bool}} -> 
		#listen_opts{ opts = [{active,true}]};
	    {ok,Option} -> 
		#listen_opts{ opts = [{active,true}, Option]};
	    _ ->
		#listen_opts{ opts = [{active,true}]}
	end,
    case list_opt(Opts, BaseOpts, listen_options()) of
	{ok, R} ->
	    {ok, R#listen_opts {
		   ifaddr = translate_ip(R#listen_opts.ifaddr, Family)
		  }};
	Error -> Error
    end.
	
list_opt([{raw,A,B,C}|Opts], R, As) ->
    list_opt([{raw,{A,B,C}}|Opts], R, As);
list_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}      ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{ifaddr,IP}  ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{port,P}     ->  list_opt(Opts, R#listen_opts { port = P }, As);
	{fd,Fd}      ->  list_opt(Opts, R#listen_opts { fd = Fd }, As);
	{backlog,BL} ->  list_opt(Opts, R#listen_opts { backlog = BL }, As);
	binary       ->  list_add(mode, binary, R, Opts, As);
	list         ->  list_add(mode, list, R, Opts, As);
	{tcp_module,_}  -> list_opt(Opts, R, As);
	inet         -> list_opt(Opts, R, As);
	inet6        -> list_opt(Opts, R, As);
	{Name,Val} when is_atom(Name) -> list_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
list_opt([], R, _SockOpts) ->
    {ok, R}.

list_add(Name, Val, R, Opts, As) ->
    case add_opt(Name, Val, R#listen_opts.opts, As) of
	{ok, SOpts} ->
	    list_opt(Opts, R#listen_opts { opts = SOpts }, As);
	Error -> Error
    end.

-spec open(Fd :: integer(),
	   Addr :: ip_address(),
	   Port :: port_number(),
	   Opts :: [socket_setopt()],
	   Protocol :: socket_protocol(),
	   Family :: address_family(),
	   Type :: socket_type(),
	   Module :: atom()) ->
	{'ok', socket()} | {'error', posix()}.

open(Fd, Addr, Port, Opts, Protocol, Family, Type, Module) when Fd < 0 ->
    case prim_inet:open(Protocol, Family, Type) of
	{ok,S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case if is_list(Addr) ->
				 bindx(S, Addr, Port);
			    true ->
				 prim_inet:bind(S, Addr, Port)
			 end of
			{ok, _} -> 
			    inet_db:register_socket(S, Module),
			    {ok,S};
			Error  ->
			    prim_inet:close(S),
			    Error
		    end;
		Error  ->
		    prim_inet:close(S),
		    Error
	    end;
	Error ->
	    Error
    end;
open(Fd, _Addr, _Port, Opts, Protocol, Family, Type, Module) ->
    fdopen(Fd, Opts, Protocol, Family, Type, Module).

bindx(S, [Addr], Port0) ->
    {IP, Port} = set_bindx_port(Addr, Port0),
    prim_inet:bind(S, IP, Port);
bindx(S, Addrs, Port0) ->
    [{IP, Port} | Rest] = [set_bindx_port(Addr, Port0) || Addr <- Addrs],
    case prim_inet:bind(S, IP, Port) of
	{ok, AssignedPort} when Port =:= 0 ->
	    %% On newer Linux kernels, Solaris and FreeBSD, calling
	    %% bindx with port 0 is ok, but on SuSE 10, it results in einval
	    Rest2 = [change_bindx_0_port(Addr, AssignedPort) || Addr <- Rest],
	    prim_inet:bind(S, add, Rest2);
	{ok, _} ->
	    prim_inet:bind(S, add, Rest);
	Error ->
	    Error
    end.

set_bindx_port({_IP, _Port}=Addr, _OtherPort) ->
    Addr;
set_bindx_port(IP, Port) ->
    {IP, Port}.

change_bindx_0_port({IP, 0}, AssignedPort) ->
    {IP, AssignedPort};
change_bindx_0_port({_IP, _Port}=Addr, _AssignedPort) ->
    Addr.


-spec fdopen(Fd :: non_neg_integer(),
	     Opts :: [socket_setopt()],
	     Protocol :: socket_protocol(),
	     Family :: address_family(),
	     Type :: socket_type(),
	     Module :: atom()) ->
	{'ok', socket()} | {'error', posix()}.

fdopen(Fd, Opts, Protocol, Family, Type, Module) ->
    case prim_inet:fdopen(Protocol, Family, Type, Fd) of
	{ok, S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    inet_db:register_socket(S, Module),
		    {ok, S};
		Error ->
		    prim_inet:close(S), Error
	    end;
	Error -> Error
    end.
