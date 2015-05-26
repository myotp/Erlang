%%%-----------------------------------------------------------------------------
%%% Another example to show how to use Map-Reduce
%%%-----------------------------------------------------------------------------
-module(page_rank).

-compile(export_all).

run_demo() ->
    Urls = ["http://www.google.com", "http://www.bing.com"],
    page_rank(Urls).

page_rank(Urls) ->
    map_reduce:map_reduce_seq(fun mapper/2, fun reducer/2,
                              [{Url, dummy} || Url <- Urls]).

mapper(Url, dummy) ->
    Html = fetch_url(Url),
    Urls = find_url(Url, Html),
    [{U, 1} || U <- Urls].

reducer(Url, Ns) ->
    [{Url, lists:sum(Ns)}].

%% Use local files to simulate real web pages
fetch_url(Url) ->
    case file:read_file(url_to_filename(Url)) of
        {ok, Bin} ->
            Bin;
        {error, _} ->
            <<"">>
    end.

url_to_filename("http://" ++ Hostname) ->
    "data/" ++ Hostname ++ ".html".

find_url(Url, Html) ->
    UrlPattern = "<a +href *= *\"(.+)\"",
    case re:run(Html, UrlPattern, [global, {capture, all_but_first, list}]) of
        nomatch ->
            [];
        {match, Urls} ->
            lists:map(fun(SubUrl) -> normalize_url(Url, SubUrl) end,
                      lists:concat(Urls))
    end.

normalize_url(_, "http://" ++ _ = SubUrl) ->
    SubUrl;
normalize_url(Url, [$/|_] = SubUrl) -> %% /root.html
    root_folder(Url) ++ SubUrl;
normalize_url(Url, SubUrl) -> %% local.html
    folder(Url) ++ SubUrl.

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


test() ->
    Urls = [ normalize_url("http://www.a.com", "http://www.b.com")
           , normalize_url("http://www.a.com/a.html", "http://www.b.com")
           , normalize_url("http://www.a.com/a.html", "local.html")
           , normalize_url("http://www.a.com/a.html", "/root.html")
           ],
    lists:foreach(fun(U) -> io:format("~p~n", [U]) end, Urls).
