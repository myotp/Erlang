-module(erlpcap).

-include("debug.hrl").
-include("config.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-export([ start_capture/3
        , loop_capture/0
        ]).

-define(SERVER, erlang_libpcap_driver_server).

-define(PCAP_OPEN, 1).
-define(PCAP_LOOP, 2).
-define(INJECT,   7).



-record(state, {erlang_port,
                pcap = undefined, %% (pcap *) in C
                iface}).


%%===== API functions =========
start(Iface) ->
    Pid = spawn(?MODULE, init, [Iface]),
    register(?SERVER, Pid),
    ?log("erlpcap start ~p with Iface ~p~n", [Iface, Pid]),
    Pid.

start_capture(SnapLen, Promisc, Tout_ms) ->
    ?SERVER ! {capture, self(), SnapLen, Promisc, Tout_ms},
    receive
	Rep ->
	    Rep
    end.

send(Port, Data) ->
    Port ! {send, Data, self()},
    receive
	ok ->
	    ok
    end.

loop_capture() ->
    ?SERVER ! {loop, self()},
    receive
        ok ->
            ok
    end.

%%==============================================================================
init(Iface) ->
    Port = open_port({spawn, "priv/sniff"},
                     [{packet, 2}, binary, nouse_stdio]),
    ?log("erlpcap opened port: ~p~n", [Port]),
    loop(#state{erlang_port = Port,
                iface = Iface}).

loop(#state{ pcap = Pcap
           , erlang_port = Port
           , iface = Iface} = State) ->
    receive
	{send, Data, From} when is_list(Data) ->
	    Bin_Data = list_to_binary(lists:flatten(Data)),
	    pcap_inject(Port, Pcap, Bin_Data),
	    From ! ok,
	    loop(State);

	{capture, From, SnapLen, Promisc, TimeoutMs} = Msg ->
            ?log("[MSG] ~p~nCurrent Pcap: ~p~n", [Msg, Pcap]),
            case Pcap of
                undefined ->
                    {ok, NewPcap} = pcap_open_live(Port, Iface, SnapLen, Promisc, TimeoutMs),
                    From ! ok,
                    loop(State#state{pcap = NewPcap});
                _ ->
                    From ! {error, already_open},
                    loop(State)
            end;
	{loop, From} = Msg ->
            ?log("[MSG] ~p~n", [Msg]),
            ?log("Current State: ~p~n", [State]),
	    pcap_loop(Port, Pcap),
	    From ! ok,
	    loop(State);
        {Port, {data, Packet}} ->
            %%eth:decode(Packet),
            %% TODO
            %% eth_server:recv(Packet),
            %% send package to ethernet handler
            io:format("packet: ~p~n", [Packet]),
            loop(State);
        UnknownMsg ->
            ?x(UnknownMsg),
            loop(State)
    end.

%%%============= libpcap driver functions ======================================
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
