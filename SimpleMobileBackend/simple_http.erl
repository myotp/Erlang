-module(simple_http).

-behaviour(gen_web_server).
%% callback functions for gen_web_server behaviour
-export([ init/1
        , get/3
        , post/4
        ]).

-export([ start/0
        ]).

-compile(export_all).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

-define(WEB_ROOT, "./web").
-define(TABLE_ID, persons).

pretty_print(M, L, S, V) ->
    io:format("[~12s:~3s] == ~s =====>>> ~n~p~n~n~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

start() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ets:insert(?TABLE_ID, {"Jordan", 28}),
    ets:insert(?TABLE_ID, {"Doooom", 33}),
    ets:insert(?TABLE_ID, {"Kooloo", 34}),
    gws_connection_sup:start_link(?MODULE, {0,0,0,0}, 8080, []).

init(InitArg) ->
    ?x(InitArg),
    {ok, []}.


generate_persons_page() ->
    Persons = all_persons(),
    generate_persons_page(Persons).

generate_persons_page([]) ->
    "";
generate_persons_page([{Name, Age}|Rest]) ->
    "<p>" ++ Name ++ " : " ++ integer_to_list(Age) ++ "</p>"
        ++ generate_persons_page(Rest).

get({http_request, 'GET', {abs_path, <<"/">>}, Version}, Headers, UserData) ->
    get({http_request, 'GET', {abs_path, <<"/index.html">>}, Version},
        Headers, UserData);
get({_, _, {abs_path, <<"/all_persons.json">>}, _}, _, _) ->
    Json = all_persons_json(),
    gen_web_server:http_reply(200, [{'Content-Type', "application/json"}], Json);

get({_, _, {abs_path, <<"/all_persons.html">>}, _}, _, _) ->
    Page = generate_persons_page(),
    gen_web_server:http_reply(200, Page);
get({_, _, {abs_path, PathBin}, _}, _Headers, _UserData) ->
    %% FIXME, static/dynamic file
    ?x(PathBin),
    Filename = binary_to_list(PathBin),
    ?x(Filename),
    ContentType = content_type(Filename),
    Path = get_file_path(Filename),
    ?x(Path),
    {ok, Bin} = file:read_file(Path),
    Reply = gen_web_server:http_reply(200, [{'Content-Type', ContentType}],Bin),
    ?x(Reply),
    Reply;
get(Request, Headers, UserData) ->
    ?x(Request),
    ?x(Headers),
    ?x(UserData),
    simple_html().

get_file_path("/") ->
    get_file_path("/index.html");
get_file_path("/" ++ Rest) ->
    filename:join(?WEB_ROOT, Rest).

content_type(Filename) ->
    case filename:extension(Filename) of
        ".html" ->
            "text/html";
        ".png" ->
            "image/png";
        _ ->
            "text/plain"
    end.

post({_,_,{_, <<"/new_person">>}, _}, Headers, Body, _UserData) ->
    Args = get_args(Headers, Body),
    new_person(Args),
    gen_web_server:http_reply(301, [{"Location", "/Thanks.html"}], <<>>);
post(Request, Headers, Body, UserData) ->
    ?x(Request),
    ?x(Headers),
    ?x(Body),
    ?x(UserData),
    gen_web_server:http_reply(301, [{"Location", "/Thanks.html"}], <<>>).

new_person(Args) ->
    Name = proplists:get_value("name", Args),
    Age = to_integer(proplists:get_value("age", Args)),
    ets:insert(?TABLE_ID, {Name, Age}).

to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Int) when is_integer(Int) ->
    Int.

get_args(Headers, Body) ->
    case get_content_type(Headers) of
        "json" ->
            parse_json_request(Body);
        "text" ->
            parse_text_request(Body)
    end.

get_content_type(Headers) ->
    ContentType = proplists:get_value('Content-Type', Headers),
    ?x(ContentType),
    case ContentType of
        <<"application/json">> ->
            "json";
        _ ->
            "text"
    end.

parse_json_request(Body) ->
    {struct, KVs} = mochijson:decode(Body),
    KVs.

parse_text_request("") ->
    [];
parse_text_request(Body) when is_binary(Body) ->
    parse_text_request(binary_to_list(Body));
parse_text_request(Body) ->
    io:format("To parse request: ~p~n", [Body]),
    parse_key(Body, "").

parse_key([$=|Rest], Key) ->
    parse_value(Rest, "", Key);
parse_key([H|T], Key) ->
    parse_key(T, Key ++ [H]).

parse_value([$&|Rest], Value, Key) ->
    [{Key, Value} | parse_text_request(Rest)];
parse_value("", Value, Key) ->
    [{Key, Value}];
parse_value([H|T], Value, Key) ->
    parse_value(T, Value ++ [H], Key).

simple_html() ->
    <<"<html><body><h1>Hello, my web server</h1></body></html>">>.

all_persons() ->
    ets:tab2list(?TABLE_ID).

all_persons_json() ->
    lists:flatten(mochijson:encode(all_persons_json(all_persons()))).

all_persons_json(Persons) ->
    {struct, [{"persons", {array, gen_all_persons_json(Persons)}}]}.

gen_all_persons_json([]) ->
    [];
gen_all_persons_json([{Name, Age}|Rest]) ->
    [{struct, [{"name", Name}, {"age", Age}]}|gen_all_persons_json(Rest)].

sample_persons() ->
    {struct, [{"persons", {array, [{struct, [{"name", "wang"}, {"age", 111}]},
                                   {struct, [{"name", "Bob"}, {"age", 222}]}]}}]}.

sample_persons_json() ->
    lists:flatten(mochijson:encode(sample_persons())).
