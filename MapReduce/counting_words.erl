%%%-----------------------------------------------------------------------------
%%% An example to show how to use Map-Reduce

%% Init ->
%% File1 : Hello, World.
%% File2 : Hello, Java.
%% [{"file1", "Hello, world"}, {"file2", "Hello, Java"}]

%% after map =>
%% [{"hello", 1}, {"world", 1}, {"hello", 1}, {"java", 1}]

%% after group =>
%% [{"hello", [1, 1]}, {"java", 1}, {"world", 1}]
%% after reduce =>
%% [{"hello",2},{"java",1},{"world",1}]
%%%-----------------------------------------------------------------------------
-module(counting_words).

-compile(export_all).

run_demo() ->
    File1 = "data/file1",
    File2 = "data/file2",
    count_words([File1, File2]).

count_words(Files) ->
    MapReduceInput = [{File, body(File)} || File <- Files],
    map_reduce:map_reduce_seq(fun mapper/2, fun reducer/2, MapReduceInput).

mapper(_File, Body) ->
    [{string:to_lower(Word), 1} || Word <- words(Body)].

reducer(Word, Occs) ->
    [{Word, lists:sum(Occs)}].

body(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

words(Content) ->
    [Word || Word <- re:split(Content, "[^a-zA-Z*]", [{return, list}]),
             Word =/= []].
