-module(arp_server).

-include("debug.hrl").
-include("config.hrl").

-behaviour(gen_server).

-export([ recv/1
        , find_mac_address_by_ip/1
        , send_mac_request/1
        , cache_to_list/0
        , get_ip_and_mac/0
        , print_state/0
        ]).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ARP_CACHE, arp_cache).
-define(ARP_CACHE_TIMEOUT, 30000).

-define(ETH_BROAD, 16#ffffffffffff).
-define(ETH_IP,  16#0800).
-define(ETH_ARP, 16#0806).

-define(ETH_LEN,     6).
-define(IP_LEN,      4).
-define(ARP_REQUEST, 1).
-define(ARP_REPLY,   2).

-define(ARP_MIDDLE_MAN, arp_middle_man).

-record(state, { ip_address
               , mac_address
%               , capture      :: self | all %% capture which addresses
               }).

-ifdef(DEBUG_ARP).
-define(DEBUG_ARP_PACKET(Packet), debug_arp_packet(Packet)).
-else.
-compile([{nowarn_unused_function, [ {debug_arp_packet, 1}
                                   , {arp_type_to_list, 1}]}]).
-define(DEBUG_ARP_PACKET(Packet), ok).
-endif.

%%==============================================================================
%% API
%%==============================================================================
recv(Packet) ->
    gen_server:cast(?SERVER, {recv, Packet}).

print_state() ->
    gen_server:cast(?SERVER, print_state).

find_mac_address_by_ip(Ip) when is_list(Ip) ->
    find_mac_address_by_ip(addr_util:ip_list_to_integer(Ip));
find_mac_address_by_ip(Ip) when is_tuple(Ip) ->
    find_mac_address_by_ip(addr_util:ip_tuple_to_integer(Ip));
find_mac_address_by_ip(IpDst) when is_integer(IpDst) ->
    case simple_cache:get(?ARP_CACHE, IpDst) of
        {ok, Mac} ->
            {ok, Mac};
        _ ->
            middle_man:request(?ARP_MIDDLE_MAN, ?MODULE, send_mac_request,
                               [IpDst])
    end.

send_mac_request(IpDst) when is_integer(IpDst)  ->
    {ok, {MyIp, MyMac}} = get_ip_and_mac(),
    Packet = make_packet(?ARP_REQUEST, IpDst, 0, MyIp, MyMac),
    debug:pretty_print_packet(Packet),
    eth_server:send(broadcast, arp, Packet).

get_ip_and_mac() ->
    gen_server:call(?SERVER, get_ip_and_mac).

%%==============================================================================
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

init(Config) ->
    Ip = proplists:get_value(ip_address, Config),
    Mac = proplists:get_value(mac_address, Config, ?MAC_ADDR),
    start_arp_cache(),
    start_arp_middle_man(),
    State = #state{ ip_address = Ip
                  , mac_address = Mac},
    {ok, State}.

handle_call(get_ip_and_mac, _From, #state{ ip_address = Ip
                                         , mac_address = Mac} = State) ->
    {reply, {ok, {Ip, Mac}}, State}.

handle_cast({recv, Packet}, #state{ ip_address = Ip
                                  , mac_address = Mac} = State) ->
    ?DEBUG_ARP_PACKET(Packet),
    handle_packet(Packet, Ip, Mac),
    {noreply, State};
handle_cast(print_state, State) ->
    ?log("DEBUG eth_server State: ~p~n", [State]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_packet(Packet, LocalIp, LocalMac) ->
    case Packet of
        <<1:16/big-integer, ?ETH_IP:16/big-integer,
          ?ETH_LEN:8/integer, ?IP_LEN:8/integer, Type:16/big-integer,
          SrcMac:48/big-integer,
          SrcIp:32/big-integer, _DstMac:48/big-integer,
          LocalIp2:32/big-integer, _/binary>> ->
            maybe_update_arp_cache(Type, SrcIp, SrcMac),
            case LocalIp == LocalIp2 of
                true ->
                    maybe_reply_middle_man(Type, SrcIp, SrcMac),
                    case Type of
                        ?ARP_REQUEST ->
                            send_arp_reply(SrcIp, SrcMac, LocalIp, LocalMac);
                        _ ->
                            ok
                    end;
                false ->
                    ignore_other_ip_addresses
            end;
        _ ->
            io:format("HELP!!!~n"),
            ?DEBUG_ARP_PACKET(Packet)
    end.

send_arp_reply(DstIp, DstMac, Ip, Mac) -> % Send Arp Reply
    Packet = make_packet(?ARP_REPLY, DstIp, DstMac, Ip, Mac),
    io:format("going to send packet back....~n"),
    debug:pretty_print_packet(Packet, 4),
    eth_server:send(DstMac, arp, Packet).

make_packet(Type, IpDst, MacDst, Ip, Mac) ->
    <<1:16/big-integer, ?ETH_IP:16/big-integer,
      ?ETH_LEN:8/integer, ?IP_LEN:8/integer, Type:16/big-integer,
      Mac:48/big-integer,
      Ip:32/big-integer,
      MacDst:48/big-integer,
      IpDst:32/big-integer>>.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

debug_arp_packet(Packet) ->
    <<1:16/big-integer, ?ETH_IP:16/big-integer,
      ?ETH_LEN:8/integer, ?IP_LEN:8/integer, Type:16/big-integer,
      Mac:48/big-integer,
      Ip:32/big-integer,
      MacDst:48/big-integer,
      IpDst:32/big-integer,
      _/binary>> = Packet,
    io:format("<< ARP Packet >>~n"),
    io:format("Type: ~p -> ~s~n", [Type, arp_type_to_list(Type)]),
    io:format("[~s](~s) ==> [~s](~s)~n",
              [ addr_util:mac_integer_to_list(Mac)
              , addr_util:ip_integer_to_list(Ip)
              , addr_util:mac_integer_to_list(MacDst)
              , addr_util:ip_integer_to_list(IpDst)
              ]),
    debug:pretty_print_packet(Packet),
    ok.

cache_to_list() ->
    [{addr_util:ip_integer_to_list(Ip), addr_util:mac_integer_to_list(Mac)}
     || {Ip, Mac} <- simple_cache:to_list(?ARP_CACHE)].

start_arp_cache() ->
    simple_cache:new(?ARP_CACHE).

start_arp_middle_man() ->
    middle_man:start_link(?ARP_MIDDLE_MAN).

maybe_reply_middle_man(?ARP_REPLY, Ip, Mac) ->
    middle_man:reply(?ARP_MIDDLE_MAN, [Ip], {ok, Mac});
maybe_reply_middle_man(_, _, _) ->
    ok.

maybe_update_arp_cache(?ARP_REQUEST, _, _) ->
    skip_request_packet;
maybe_update_arp_cache(?ARP_REPLY, Ip, Mac) ->
    update_arp_cache(Ip, Mac);
maybe_update_arp_cache(_, _, _) ->
    ok.

update_arp_cache(Ip, Mac) ->
    simple_cache:put(?ARP_CACHE, Ip, Mac, ?ARP_CACHE_TIMEOUT).

arp_type_to_list(?ARP_REQUEST) ->
    "request";
arp_type_to_list(?ARP_REPLY) ->
    "reply".

%%==============================================================================
%% Demo
%%==============================================================================
-define('ping 192.168.1.155', "
<<Packet>>
00 01 08 00 |    0   1   8   0
06 04 00 01 |    6   4   0   1
7C D1 C3 F4 |  124 209 195 244
E7 C1 C0 A8 |  231 193 192 168
01 65 00 00 |    1 101   0   0
00 00 00 00 |    0   0   0   0
C0 A8 01 9B |  192 168   1 155
").

%%========== show ARP cache ==============================================
%% jiaw@hundhaj:~/me/git/misc/mytcp(master)$ arp -a
%% ? (192.168.1.1) at f8:d1:11:f4:f7:70 on en0 ifscope [ethernet]
%% ? (192.168.1.102) at 6c:c2:6b:9:44:d9 on en0 ifscope [ethernet]
%% ? (192.168.1.255) at ff:ff:ff:ff:ff:ff on en0 ifscope [ethernet]

%% after ping 192.168.1.155 and use my dummy ARP reply :)
%% jiaw@hundhaj:~/me/git/misc/mytcp(master)$ arp -a
%% ? (192.168.1.1) at f8:d1:11:f4:f7:70 on en0 ifscope [ethernet]
%% ? (192.168.1.102) at 6c:c2:6b:9:44:d9 on en0 ifscope [ethernet]
%% ? (192.168.1.155) at (incomplete) on en0 ifscope [ethernet]
