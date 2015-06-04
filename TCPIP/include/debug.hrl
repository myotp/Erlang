-define(log(Fmt, Args), io:format("[LOG][~p:~p] " ++ Fmt,
                                  [?MODULE, ?LINE] ++ Args)).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                          debug:pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

-ifdef(DEBUG_ALL_PACKET).
-define(DEBUG_ALL_PACKET(Packet), debug:pretty_print_packet(Packet)).
-else.
-define(DEBUG_ALL_PACKET(Packet), ok).
-endif.
