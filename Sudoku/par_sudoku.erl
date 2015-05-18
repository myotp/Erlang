-module(par_sudoku).
%-include_lib("eqc/include/eqc.hrl").
-compile(export_all).
-define(MAX_PMAP_DEPTH, 2).
-define(pmap_controller, pmap_controller).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
		unblocks(
		  refine_rows(
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

refine_rows(M) ->
    lists:map(fun refine_row/1,M).

refine_row(Row) ->
    Entries = entries(Row),
    NewRow =
	[if is_list(X) ->
		 case X--Entries of
		     [] ->
			 exit(no_solution);
		     [Y] ->
			 Y;
		     NewX ->
			 NewX
		 end;
	    true ->
		 X
	 end
	 || X <- Row],
    NewEntries = entries(NewRow),
    %% check we didn't create a duplicate entry
    case length(lists:usort(NewEntries)) == length(NewEntries) of
	true ->
	    NewRow;
	false ->
	    exit(no_solution)
    end.

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->		      
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    %% CatchRefineF = fun(G) -> catch refine(update_element(M,I,J,G)) end,
    %% Ms = pmap(CatchRefineF, Guesses),
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

solve(M) ->
    ?pmap_controller:reset_depth(),
    Solution = solve_refined(refine(fill(M))),
    case valid_solution(Solution) of
	true ->
	    Solution;
	false ->
	    exit({invalid_solution,Solution})
    end.

solve_refined(M) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    maybe_par_solve_one(guesses(M))
    end.

%%%================ parallel guess solution ======================
maybe_par_solve_one(Ms) ->
    case ?pmap_controller:get_depth() of
        N when N < ?MAX_PMAP_DEPTH ->
            par_solve_one(Ms);
        _ ->
            solve_one(Ms)
    end.

par_solve_one(Ms) ->
    Result = pmap(fun do_solve_one/1, Ms),
    case [R || R <- Result, R =/= no_solution] of
        [] ->
            exit(no_solution);
        [H|_] ->
            H
    end.

do_solve_one(M) ->
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
            no_solution;
	Solution ->
	    Solution
    end.

solve_one([]) ->
    exit(no_solution);
solve_one([M]) ->
    solve_refined(M);
solve_one([M|Ms]) ->
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
	    solve_one(Ms);
	Solution ->
	    Solution
    end.

%% benchmarks

-define(EXECUTIONS,10).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
    ?pmap_controller:start(),
    {ok,Puzzles} = file:consult("problems.txt"),
    TCResult = timer:tc(?MODULE,benchmarks,[Puzzles]),
    ?pmap_controller:stop(),
    TCResult.

%%%==================== parallel benchmarks ======================
par_benchmarks() ->
    ?pmap_controller:start(),
    {ok, Puzzles} = file:consult("problems.txt"),
    TCResult = timer:tc(?MODULE, par_benchmarks, [Puzzles]),
    ?pmap_controller:stop(),
    TCResult.

par_benchmarks(Puzzles) ->
    SolveF = fun({Name, M}) -> {Name, bm(fun() -> solve(M) end)} end,
    pmap(SolveF, Puzzles).

pmap(Fun, List) ->
    case ?pmap_controller:get_depth() of
        N when N < ?MAX_PMAP_DEPTH ->
            ?pmap_controller:increase_depth(),
            Parent = self(),
            Pids = [spawn_job(Parent, Fun, Job) || Job <- List],
            Results = [receive {Pid, Result} -> Result end || Pid <- Pids],
            ?pmap_controller:decrease_depth(),
            Results;
        _ ->
            lists:map(Fun, List)
    end.

spawn_job(Parent, Fun, {Name, _Arg}=Job) when is_atom(Name) ->
    spawn(fun() -> %% make name visible in percept result
                   UniqueName = unique_name(Name),
                   register(UniqueName, self()),
                   Result = Fun(Job),
                   Parent ! {self(), Result} end);
spawn_job(Parent, Fun, Job) ->
    spawn(fun() -> Result = Fun(Job),
                   Parent ! {self(), Result} end).

%% TODO, handle repeat names
unique_name(Name) ->
    Name.

%% check solutions for validity

valid_rows(M) ->
    lists:all(fun valid_row/1,M).

valid_row(Row) ->
    lists:usort(Row) == lists:seq(1,9).

valid_solution(M) ->
    valid_rows(M) andalso valid_rows(transpose(M)) andalso valid_rows(blocks(M)).
