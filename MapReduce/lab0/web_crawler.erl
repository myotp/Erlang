%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module defines a simple web crawler using map-reduce.
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
-module(web_crawler).
-compile(export_all).

-define(DETS_TABLE, mycrawler).

%% Crawl from a URL, following links to depth D.
%% Before calling this function, the inets service must
%% be started using inets:start().
crawl(Url, D) ->
    inets:start(),
    {ok, ?DETS_TABLE} = dets:open_file(?DETS_TABLE, [{file, "web.dat"},
                                                     {keypos, 1}]),
    io:format("open table done~n"),
    Pages = follow(D, [{Url,undefined}]),
    [{U, Body} || {U,Body} <- Pages,
                  Body /= undefined].

follow(0, KVs) ->
    KVs;
follow(D, KVs) ->
%%    follow(D-1, map_reduce:map_reduce_par(fun map/2, 20, fun reduce/2, 1, KVs)).
    follow(D-1, map_reduce:map_reduce_seq(fun map/2, fun reduce/2, KVs)).

map(Url,undefined) ->
    Body = fetch_url(Url),
    [{Url,Body}] ++ [{U, undefined} || U <- find_urls(Url, Body)];

map(Url, Body) ->
    [{Url, Body}].

reduce(Url, Bodies) ->
    case [B || B <- Bodies, B /= undefined] of
        [] ->
            [{Url, undefined}];
        [Body] ->
            [{Url, Body}]
    end.

fetch_url(Url) ->
    case dets:lookup(?DETS_TABLE, Url) of
        [] ->
            Body = do_fetch_url(Url),
            dets:insert(?DETS_TABLE, {Url, Body}),
            Body;
        [{Url, Body}] ->
            io:format("[DETS] ~p~n", [Url]),
            Body
    end.

do_fetch_url(Url) ->
    case skip_url(Url) of
        true ->
            io:format("[SKIP] ~p~n", [Url]),
            "";
        false ->

            case httpc:request(get, {Url, []}, [{timeout, 1000}], []) of
                {ok, {{_, 200, _}, _Headers, Body}} ->
                    io:format("[OK] ~p~n", [Url]),
                    Body;
                {ok, {{_, Error, _}, _Headers, _Body}} ->
                    io:format("[~p] ~p~n", [Error, Url]),
                    "";
                _ ->
                    ""
            end
    end.

skip_url(Url) ->
    skip_non_text_files(Url).

skip_non_text_files(Url) ->
    lists:any(fun(Pattern) -> end_with(Url, Pattern) end,
              url_filters:skip_file_types()).

end_with(S, Pattern) ->
    string:len(S) - string:len(Pattern) - string:rstr(S, Pattern) < 0.

%% Find all the urls in an Html page with a given Url.
find_urls(Url,Html) ->
    Lower = string:to_lower(Html),
    %% Find all the complete URLs that occur anywhere in the page
    Absolute = case re:run(Lower, "http://.*?(?=\")", [global]) of
                   {match, Locs} ->
                       [lists:sublist(Html, Pos + 1, Len)
                        || [{Pos, Len}] <- Locs];
                   _ ->
                       []
               end,
    %% Find links to files in the same directory, which need to be
    %% turned into complete URLs.
    Relative = case re:run(Lower, "href *= *\"(?!http:).*?(?=\")", [global]) of
                   {match, RLocs} ->
                       [lists:sublist(Html, Pos+1, Len)
                        || [{Pos, Len}] <- RLocs];
                   _ ->
                       []
               end,
    Absolute ++ [Url ++ "/" ++ lists:dropwhile(fun(Char) -> Char == $/ end,
                                               tl(lists:dropwhile(fun(Char) -> Char /= $" end, R))) || R <- Relative].
