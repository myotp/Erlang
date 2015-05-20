-module(dot).

-compile(export_all).

run() ->
    run("sample_events").

run(EventsFileName) ->
    Events = read_events_file(EventsFileName),
    write_to_dot_file(count(Events), EventsFileName),
    generate_png_file(EventsFileName),
    ok.

%% Filename => [{StartState, EndState, Event}]
read_events_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Lines = string:tokens(binary_to_list(Bin), "\r\n"),
    lists:map(fun line_to_input/1, Lines).

line_to_input(Line) ->
    lists:map(fun string:strip/1, re:split(Line, ",", [{return, list}])).

count(L) ->
    count(lists:sort(L), [], 0).

count([E], Result, N)     -> lists:reverse([{E, N+1}|Result]);
count([H,H|T], Result, N) -> count([H|T], Result, N+1);
count([H|T], Result, N)   -> count(T, [{H, N+1}|Result], 0).

%% Write to dot file
write_to_dot_file(EventsCount, EventsFileName) ->
    DotFileName = dot_file_name(EventsFileName),
    {ok, File} = file:open(DotFileName, [write]),
    file:write(File, "digraph " ++ EventsFileName ++ "\n{\n"),
    lists:foreach(fun(Count) -> write_transition(File, Count) end, EventsCount),
    file:write(File, "}"),
    file:close(File).

write_transition(File, {[StartState, EndState, Event], Count}) ->
    S = "  " ++ StartState ++ " -> " ++ EndState ++ "  [label=\""
        ++ Event ++ ":" ++ integer_to_list(Count) ++ "\"];\n",
    file:write(File, S).

%% Generate png file
generate_png_file(EventsFileName) ->
    Command = "dot -Tpng " ++ dot_file_name(EventsFileName) ++ " -o "
        ++ png_file_name(EventsFileName),
    os:cmd(Command).

dot_file_name(FileName) ->
    FileName ++ ".dot".

png_file_name(FileName) ->
    FileName ++ ".png".


