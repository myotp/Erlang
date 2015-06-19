-module(my_dbg).

-export([ pretty_print/4
        , pretty_print_with_pid/4
        ]).

pretty_print(M, L, S, V) ->
    io:format("[~12s:~3s] ~-25s = ~p~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

pretty_print_with_pid(M, L, S, V) ->
    io:format("[~12s:~3s] ~-9s ~-15s = ~p~n",
              [atom_to_list(M), integer_to_list(L), pid_to_list(self()), S, V]).
