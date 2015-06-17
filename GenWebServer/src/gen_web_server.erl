-module(gen_web_server).

%%% API
-export([ start_link/3
        , start_link/4
        , http_reply/1
        , http_reply/2
        , http_reply/3
        ]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [ {init, 1}
    , {get, 3}
    ];
behaviour_info(_Other) ->
    undefined.

%%% API
start_link(CallBackMod, Port, UserArgs) ->
    start_link(CallBackMod, undefined, Port, UserArgs).

start_link(CallBackMod, IP, Port, UserArgs) ->
    gws_connection_sup:start_link(CallBackMod, IP, Port, UserArgs).

http_reply(Code) ->
    http_reply(Code, <<>>).

http_reply(Code, Body) ->
    http_reply(Code, [{"Content-Type", "text/html"}], Body).

http_reply(Code, Headers, Body) ->
    ContentBytes = iolist_to_binary(Body),
    Length = byte_size(ContentBytes),
    [io_lib:format("HTTP/1.1 ~s\r\n~s\r\nContent-Length: ~w\r\n\r\n",
                   [response(Code), headers(Headers), Length]),
     ContentBytes].

response(100)  -> "100 Continue";
response(200)  -> "200 OK";
response(404)  -> "404 Not Found";
response(Code) -> integer_to_list(Code).

headers([{Header, Text} | Rest]) ->
    [io_lib:format("~s: ~s\r\n", [Header, Text]) | headers(Rest)];
headers([]) ->
    [].
