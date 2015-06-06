-module(pcap_server).

-include("debug.hrl").
-include("config.hrl").

-behaviour(gen_server).
-export([send/1]).
-export([start/1, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { network_interface
               , port
               , pcap = undefined
               }).

%% Protocol
-define(PCAP_OPEN, 1).
-define(PCAP_LOOP, 2).
-define(INJECT,   7).

%% API
-define(SNAP_LEN, 65535).
-define(PROMISC, 1).
-define(TIMEOUT, 100).

-ifdef(DEBUG_LIBPCAP).
-define(DEBUG_LIBPCAP_PACKET(Packet), debug_libpcap_packet(Packet)).
-else.
-compile([{nowarn_unused_function, [{debug_libpcap_packet, 1}]}]).
-define(DEBUG_LIBPCAP_PACKET(Packet), ok).
-endif.

%%===== API to send data to libpcap =========
send(Data) ->
    gen_server:cast(?SERVER, {send, Data}).

%%===== gen_server callback functions =======
start(NetworkInterface) ->
    Opts = [{network_interface, NetworkInterface}],
    gen_server:start({local, ?SERVER}, ?MODULE, Opts, []).

start_link(NetworkInterface) ->
    Opts = [{network_interface, NetworkInterface}],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

init(Opts) ->
    io:format("Pid: ~p Opts: ~p~n", [self(), Opts]),
    NetworkInterface = proplists:get_value(network_interface, Opts),
    %% init port
    Port = open_port({spawn, "priv/sniff"},
                     [{packet, 2}, binary, nouse_stdio]),
    ?x(Port),
    %% open live
    {ok, Pcap} = pcap_open_live(Port, NetworkInterface,
                                ?SNAP_LEN, ?PROMISC, ?TIMEOUT),
    ?x(Pcap),
    %% start loop capture
    pcap_loop(Port, Pcap),
    {ok, #state{ network_interface = NetworkInterface
               , port = Port
               }}.

handle_call(_Call, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, Data}, #state{ port = Port
                                , pcap = Pcap} = State) ->
    Bin = list_to_binary(lists:flatten(Data)),
    pcap_inject(Port, Pcap, Bin),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% libpcap will send packets as Erlang messages
handle_info({Port, {data, Packet}}, #state{ port = Port
                                          , pcap = _Pcap} = State) ->
    ?DEBUG_LIBPCAP_PACKET(Packet),
    handle_packet(Packet),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% only handle ethernet packet
handle_packet(Packet) ->
    eth_server:recv(Packet).

%%==== libpcap driver functions =====
pcap_open_live(Port, Iface, SnapLen, Promisc, TimeoutMs) ->
    CmdBin = <<?PCAP_OPEN:8/integer,
               SnapLen:32/native-integer,
               Promisc:8/integer,
               TimeoutMs:32/native-integer,
               (length(Iface)):32/native-integer,
               (list_to_binary(Iface))/binary>>,
    Port ! {self(), {command, CmdBin}},

    receive
        {Port, {data, <<Pcap:?POINTER_SIZE/native-integer>>}} ->
            case Pcap of
        	-1 ->
        	    error;
        	_ ->
                    ?x(Pcap),
        	    {ok, Pcap}
            end
    end.

pcap_loop(Port, Pcap) ->
    CmdBin = <<?PCAP_LOOP:8/integer,
               Pcap:?POINTER_SIZE/native-integer>>,
    Port ! {self(), {command, CmdBin}}.

pcap_inject(Port, Pcap, Data) ->
    Port ! {self(), {command, <<?INJECT:8/integer, Pcap:?WSIZE/native-integer, (size(Data)):32/native-integer, Data/binary>>}}.

debug_libpcap_packet(Packet) ->
    io:format("pcaket: ~p~n", [Packet]).
