-module(debug).

-export([ pretty_print/4
        , pretty_print_packet/1
        , pretty_print_packet/2
        ]).

pretty_print(M, L, S, V) ->
    io:format("[~20s:~3s]~-20s = ~p~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

-define(BYTES_PER_LINE, 4).

pretty_print_packet(Bin) ->
    pretty_print_packet(Bin, ?BYTES_PER_LINE).

pretty_print_packet(Bin, BytesPerLine) ->
    io:format("<< Packet >>~n"),
    S = pretty_string(binary_to_list(Bin), BytesPerLine, 1, {[], []}, []),
    io:format("~s~n", [S]).

maybe_prefix_zero([A]) ->
    [$0, A];
maybe_prefix_zero(S) ->
    S.

byte_to_hex_string(Int) ->
    maybe_prefix_zero(integer_to_list(Int, 16)).

byte_to_decimal_string(Int) ->
    io_lib:format("~4w", [Int]).

pretty_string([], _Bytes, 1, {[], []}, Acc) ->
    Acc;
pretty_string([], Bytes, N, {HexL, L}, Acc) ->
    Acc ++ concat(HexL) ++ repeat($\s, (Bytes - N + 1) * 3)
        ++ " | "++ L;
pretty_string([H|T], Bytes, Bytes, {HexL, L}, Acc) ->
    NewAcc = Acc ++ concat(HexL) ++ " " ++ byte_to_hex_string(H) ++ " | " ++
        L ++ byte_to_decimal_string(H) ++ "\n",
    pretty_string(T, Bytes, 1, {[], []}, NewAcc);
pretty_string([H|T], Bytes, N, {HexL, L}, Acc) ->
    Temp = {HexL ++ [byte_to_hex_string(H)], L ++ [byte_to_decimal_string(H)]},
    pretty_string(T, Bytes, N+1, Temp, Acc).

concat(L) ->
    string:join(L, " ").

repeat(Char, N) ->
    [Char || _ <- lists:seq(1, N)].
