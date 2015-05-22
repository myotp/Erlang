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
    file:write(File, "digraph " ++ digraph_name(EventsFileName) ++ "\n{\n"),
    %%write_transitions(File, EventsCount),
    write_transitions_maybe_invisible_nodes(File, EventsCount),
    file:write(File, "}"),
    file:close(File).

%% no dot in digraph name
digraph_name(FileName) ->
    re:replace(FileName, "\\.", "_", [{return, list}, global]).

%% write transitions normally
write_transitions(File, EventsCount) ->
    lists:foreach(fun(Count) -> write_transition(File, Count) end, EventsCount).
write_transition(File, {[StartState, EndState, Event], Count}) ->
    S = "  " ++ StartState ++ " -> " ++ EndState
        ++ "  [label=\"" ++ edge_label(Event, Count) ++ "\"];\n",
    file:write(File, S).

edge_label(Event, Count) ->
    Event ++ "[" ++ integer_to_list(Count) ++ "]".

%% write transitions with invisible nodes
write_transitions_maybe_invisible_nodes(File, EventsCount) ->
    do_write2(File, EventsCount, []).

do_write2(File, [{[State, State, Event], Count}|Rest], SameStates) ->
    N = case proplists:get_value(State, SameStates) of
            undefined -> 1;
            Other -> Other
        end,
    TmpStateName = State ++ integer_to_list(N),
    S1 = "  " ++ TmpStateName ++ " [fixedsize=true height=0 style=invis];\n",
    S2 = "  " ++ State ++ " -> " ++ TmpStateName ++ " [dir=none label=\""
        ++ edge_label(Event, Count) ++ "\"]; "
        ++ TmpStateName ++ " -> " ++ State ++ ";\n",
    file:write(File, S1),
    file:write(File, S2),
    NewAcc = [{State, N+1}|proplists:delete(State, SameStates)],
    do_write2(File, Rest, NewAcc);
do_write2(File, [DiffStatsTransition|Rest], Acc) ->
    write_transition(File, DiffStatsTransition),
    do_write2(File, Rest, Acc);
do_write2(_File, [], _) ->
    ok.

%% Generate png file
%% FIXME, handle failuers :)
generate_png_file(EventsFileName) ->
    Command = "dot -Tpng " ++ dot_file_name(EventsFileName) ++ " -o "
        ++ png_file_name(EventsFileName),
    os:cmd(Command).

dot_file_name(FileName) ->
    FileName ++ ".dot".

png_file_name(FileName) ->
    FileName ++ ".png".


