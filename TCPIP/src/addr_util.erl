-module(addr_util).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-export([ ip_tuple_to_list/1
        , ip_tuple_to_integer/1
        , ip_list_to_tuple/1
        , ip_list_to_integer/1
        , ip_integer_to_tuple/1
        , ip_integer_to_list/1
        , ip_to_integer/1

        , mac_integer_to_list/1
        , mac_integer_to_list/2
        ]).

-define(MAX_BYTE, 256).

%% IP Address
ip_to_integer(I) when is_integer(I) -> I;
ip_to_integer(T) when is_tuple(T)   -> ip_tuple_to_integer(T);
ip_to_integer(L) when is_list(L)    -> ip_list_to_integer(L).

ip_tuple_to_list(Ip) when is_tuple(Ip) ->
    inet_parse:ntoa(Ip).

ip_tuple_to_integer({A,B,C,D}) ->
    (A bsl 24) + (B bsl 16) + (C bsl 8) + D.

ip_list_to_tuple(IpList) ->
    {ok, Ip} = inet_parse:address(IpList),
    Ip.

ip_list_to_integer(Ip) ->
    ip_tuple_to_integer(ip_list_to_tuple(Ip)).

ip_integer_to_tuple(0) ->
    {0,0,0,0};
ip_integer_to_tuple(Int) ->
    ip_integer_to_tuple(Int, []).

ip_integer_to_tuple(Int, Acc) when Int >= ?MAX_BYTE ->
    Rem = Int rem ?MAX_BYTE,
    ip_integer_to_tuple(Int div ?MAX_BYTE, [Rem|Acc]);
ip_integer_to_tuple(Int, Acc) ->
    list_to_tuple([Int|Acc]).

ip_integer_to_list(Int) ->
    ip_tuple_to_list(ip_integer_to_tuple(Int)).

-define(DIGITS_IN_MAC_ADDRESS, 12).
mac_integer_to_list(MacAddr) when is_integer(MacAddr) ->
    mac_integer_to_list(MacAddr, upper_case).
mac_integer_to_list(MacAddr, Case) when is_integer(MacAddr) ->
    RawStr = erlang:integer_to_list(MacAddr, 16),
    MaybeChangeCase = case Case of
                          lower_case ->
                              string:to_lower(RawStr);
                          _ ->
                              RawStr
                      end,
    to_mac_addr_list(maybe_leading_zero(MaybeChangeCase)).

maybe_leading_zero(S) ->
    case length(S) of
        ?DIGITS_IN_MAC_ADDRESS ->
            S;
        N ->
            repeat($0, ?DIGITS_IN_MAC_ADDRESS - N) ++ S
    end.

repeat(A, N) when N > 0 ->
    [A|repeat(A, N-1)];
repeat(_, 0) ->
    [].

to_mac_addr_list(RawStr) ->
    to_mac_addr_list(lists:reverse(RawStr), 1, []).

to_mac_addr_list([], _, Acc) ->
    Acc;
to_mac_addr_list([H|T], N, Acc) when N rem 2 == 0
                                     andalso N < ?DIGITS_IN_MAC_ADDRESS->
    to_mac_addr_list(T, N+1, [$:, H|Acc]);
to_mac_addr_list([H|T], N, Acc) ->
    to_mac_addr_list(T, N+1, [H|Acc]).

%% TEST
ip_test_() ->
    [ ?_assertEqual(127 * 256 * 256 * 256 + 1, ip_tuple_to_integer({127,0,0,1}))
    , ?_assertEqual(127 * 256 * 256 * 256 + 256,
                    ip_tuple_to_integer({127,0,1,0}))
    , ?_assertEqual("127.1.2.3", ip_tuple_to_list({127,1,2,3}))
    , ?_assertEqual("127.1.2.3", ip_tuple_to_list(ip_list_to_tuple("127.1.2.3")))
    , ?_assertEqual("127.11.22.33",
                    ip_integer_to_list(ip_list_to_integer("127.11.22.33")))
    , ?_assertEqual({127,0,0,1},
                    ip_integer_to_tuple(ip_tuple_to_integer({127,0,0,1})))
    , ?_assertEqual({127,0,1,0},
                    ip_integer_to_tuple(ip_tuple_to_integer({127,0,1,0})))
    , ?_assertEqual({127,1,0,0},
                    ip_integer_to_tuple(ip_tuple_to_integer({127,1,0,0})))

    , ?_assertEqual("127.0.0.1",
                    ip_integer_to_list(ip_list_to_integer("127.0.0.1")))
    , ?_assertEqual("127.0.1.0",
                    ip_integer_to_list(ip_list_to_integer("127.0.1.0")))
    , ?_assertEqual("127.1.0.0",
                    ip_integer_to_list(ip_list_to_integer("127.1.0.0")))
    ].

maybe_leading_zero_test_() ->
    [ ?_assertEqual("000000000001", maybe_leading_zero("1"))
    , ?_assertEqual("000000000021", maybe_leading_zero("21"))
    , ?_assertEqual("ABC987654321", maybe_leading_zero("ABC987654321"))
    ].

mac_test_() ->
    [ ?_assertEqual("00:00:00:00:00:01", mac_integer_to_list(1))
    , ?_assertEqual("12:34:56:AB:CD:EF", mac_integer_to_list(16#123456abcdef))
    , ?_assertEqual("12:34:56:ab:cd:ef",
                    mac_integer_to_list(16#123456abcdef, lower_case))
    ].
