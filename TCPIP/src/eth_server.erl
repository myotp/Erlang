-module(eth_server).

-include("debug.hrl").
-include("config.hrl").

-behaviour(gen_server).

%% API
-export([ send/3
        , recv/1
        ]).

%% Export for debugging
-export([ eth_protocol/1
        , eth_protocol_to_list/1
        , print_state/0
        ]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ETH_BROAD, 16#FFFFFFFFFFFF).
-define(ETH_IP, 16#0800).
-define(ETH_ARP, 16#0806).

-define(SERVER, ?MODULE).

-ifdef(DEBUG_ETHERNET).
-define(DEBUG_ETHERNET_PACKET(Packet), debug_ethernet_packet(Packet)).
-else.
-compile([{nowarn_unused_function, [{debug_ethernet_packet, 1}]}]).
-define(DEBUG_ETHERNET_PACKET(Packet), ok).
-endif.

-record(state, { mac_address %% from which mac address (current machine)
               }).

%%==============================================================================
%% API
%%==============================================================================
send(broadcast, Protocol, Packet) ->
    send(?ETH_BROAD, Protocol, Packet);
send(To, Protocol, Packet) ->
    gen_server:cast(?SERVER, {send, To, Protocol, Packet}).

recv(Packet) ->
    gen_server:cast(?SERVER, {recv, Packet}).

print_state() ->
    gen_server:cast(?SERVER, print_state).

%%==============================================================================
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

init(Config) ->
    MacAddress = proplists:get_value(mac_address, Config, ?MAC_ADDR),
    {ok, #state{mac_address = MacAddress}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, To, Protocol, Packet}, #state{mac_address = Addr} = State) ->
    send_packet(Protocol, Packet, To, Addr),
    {noreply, State};
handle_cast({recv, Packet}, #state{mac_address = _Addr} = State) ->
    ?DEBUG_ETHERNET_PACKET(Packet),
    handle_packet(Packet),
    {noreply, State};
handle_cast(print_state, State) ->
    ?log("DEBUG eth_server State: ~p~n", [State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=================== Ethernet =================================
handle_packet(Packet) when is_binary(Packet) ->
    case Packet of
	<<_Mac:48/big-integer, _Src:48/big-integer,
	 Protocol:16/big-integer,
	 Data/binary>> -> % Eth Packet for us
	    case eth_protocol(Protocol) of
                arp ->
                    arp_server:recv(Data);
                ip ->
                    ip_server:recv(Data);
                _OtherProtocol ->
                    not_implemented
            end;
	<<?ETH_BROAD:48/big-integer, _Src:48/big-integer,
	 Protocol:16/big-integer,
	 Data/binary>> -> % Eth Broadcasted packet
            ?x(Protocol),
	    {eth_protocol(Protocol), Data};
	_ ->
	    {error, not_for_us}
    end.

send_packet(Protocol, Packet, To, From) ->
    EthProtocol = eth_protocol(Protocol),
    pcap_server:send([<<To:48/big-integer, From:48/big-integer,
                        EthProtocol:16/big-integer>>,
                      Packet]).

eth_protocol(ip)       -> ?ETH_IP;
eth_protocol(arp)      -> ?ETH_ARP;
eth_protocol(?ETH_IP)  -> ip;
eth_protocol(?ETH_ARP) -> arp;
eth_protocol(_UnknownProtocol) ->
    unknown_protocol.

eth_protocol_to_list(Int) when is_integer(Int) ->
    eth_protocol_to_list(eth_protocol(Int));
eth_protocol_to_list(Atom) when is_atom(Atom) ->
    erlang:atom_to_list(Atom).

debug_ethernet_packet(Packet) ->
    <<Mac:48/big-integer, Src:48/big-integer,
      Protocol:16/big-integer,
      _Data/binary>> = Packet,
    io:format("<<Ethernet>> Protocol (~s) -> ~s~n",
             [ integer_to_list(Protocol, 16)
             , eth_protocol_to_list(Protocol)
             ]),
    io:format("[~s] ==> [~s]~n",
              [ addr_util:mac_integer_to_list(Src)
              , addr_util:mac_integer_to_list(Mac)
              ]),
    ok.
