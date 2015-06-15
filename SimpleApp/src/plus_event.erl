-module(plus_event).

-export([ start_link/0
        , add_handler/2
        , delete_handler/2
        , plus_call/2
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Module, Args) ->
    gen_event:add_handler(?SERVER, Module, Args).

delete_handler(Module, Args) ->
    gen_event:delete_handler(?SERVER, Module, Args).

plus_call(A, B) ->
    gen_event:notify(?SERVER, {plus_call, A, B}).
