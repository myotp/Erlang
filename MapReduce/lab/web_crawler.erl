%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module defines a simple web crawler using map-reduce.
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
-module(web_crawler).
-compile(export_all).

%% Crawl from a URL, following links to depth D.
%% Before calling this function, the inets service must
%% be started using inets:start().
crawl(Url, D) ->
    Pages = follow(D, [{Url, undefined}]),
    [{U, Body} || {U, Body} <- Pages, Body /= undefined].

follow(0, KVs) ->
    KVs;
follow(D, KVs) ->
    KVs2 = map_reduce:map_reduce_seq(fun map/2, fun reduce/2, KVs),
    %%KVs2 = map_reduce:map_reduce_par(fun map/2, 20, fun reduce/2, 1, KVs),
    follow(D - 1, KVs2).

map(Url, undefined) ->
    Body = fetch_url(Url),
    Urls = find_urls(Url, Body),
    io:format("URLs for ~s: ~n~p~n", [Url, Urls]),
    [{Url, Body}] ++ [{U, undefined} || U <- Urls];
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
      case httpc:request(Url) of
          {ok, {_, _Headers, Body}} ->
              Body;
          _ ->
              ""
      end.


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
                       io:format("====== Relatives: ~p~n", [RLocs]),
                       [lists:sublist(Html, Pos+1, Len)
                        || [{Pos, Len}] <- RLocs];
                   _ ->
                       []
               end,
    Absolute ++ [relative_to_absolute(Url, R) || R <- Relative].

relative_to_absolute(Url, [$/|_Relative] = SubUrl) ->
    root_folder(Url) ++ SubUrl;
relative_to_absolute(Url, Relative) ->
    Abs = folder(Url) ++ lists:dropwhile(fun(Char) -> Char == $/ end,
                                         tl(lists:dropwhile(fun(Char) -> Char /= $" end, Relative))),
    io:format("Relative: ~s and URL: ~p~n", [Relative, Url]),
    io:format("Absolute: ~s~n", [Abs]),
    Abs.

folder("http://" ++ Url) ->
    case string:rchr(Url, $/) of
        0 ->
            "http://" ++ Url ++ "/";
        N when is_integer(N) andalso N > 1 ->
            "http://" ++ string:sub_string(Url, 1, N)
    end.

root_folder("http://" ++ Url) ->
    case string:chr(Url, $/) of
        0 ->
            "http://" ++ Url;
        N when is_integer(N) andalso N > 1 ->
            "http://" ++ string:sub_string(Url, 1, N-1)
    end.
