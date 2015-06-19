-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     my_dbg:pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

-define(px(X), begin
                   fun() ->
                      __YY__ = X,
                      my_dbg:pretty_print_with_pid(?MODULE, ?LINE, ??X, __YY__),
                      __YY__
                   end()
               end).

