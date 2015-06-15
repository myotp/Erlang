-module(ts_event).

-export([ start_link/0
        , add_handler/2
        , delete_handler/2
          %% Real Events
        , notify_new_connection/2
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

delete_handler(Module, Args) ->
    gen_event:delete_handler(?SERVER, Module, Args).

notify_new_connection(Pid, IP) ->
    gen_event:notify(?SERVER, {new_connection, Pid, inet:ntoa(IP)}).
