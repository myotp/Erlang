-module(tips).

-compile(export_all).

-export([ not_crash_shell/0 %% how to not crash Erlang shell when there is crash
        ]).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

pretty_print(_M, L, S, V) ->
    io:format("[~3s]~-20s = ~p~n",
              [integer_to_list(L), S, V]).

%%==============================================================================
%% How to not crash shell
%%==============================================================================
%% By default, a crash shell will start a new process with a new pid
%% 1> self().
%% <0.31.0>
%% 2> 1 = 2.
%% ** exception error: no match of right hand side value 2
%% 3> self().
%% <0.34.0>
%% But this will lose ETS and such process related stuff
%% to not crash shell, you can do something like this:
%% 4> catch_exception(true).
%% false
%% 5> self().
%% <0.34.0>
%% 6> 1 = 2.
%% * exception error: no match of right hand side value 2
%% 7> self().
%% <0.34.0>
not_crash_shell() ->
    ok.

%%==============================================================================
%% How to tail recursive call a local defined anonymous function
%%==============================================================================
%% I want to define an anonymous function like this:
%% F = fun(0) -> 0;
%%        (X) -> X + F(X - 1) %% But here, F is undefined, how to do that?
%%     end,
%% F(5).
call_anonymous_fun_itself() ->
    F = fun(_F, 0) -> 0;
           (F,  N) -> N + F(F, N - 1)
        end,
    F(F, 5).

demo_y_combinator() ->
    Fac = fun(F) ->
                  fun(0) -> 1;
                     (N) -> N * F(N-1)
                  end
          end,
    ?x((y(Fac))(5)).

y(M) ->
    G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
    G(G).


%%==============================================================================
%% erlang:display/1
%%==============================================================================
%% copied from redbug.erl
'erlang:display/1'() ->
    X = process_info(self(),current_function),
    io:format("X is: ~p~n", [X]),
    erlang:display(X),
    ok.

%%==============================================================================
%% direct I/O to specific device
%%==============================================================================
direct_io_to_device() ->
    io:fwrite(standard_io, "hello, world~n", []).

change_group_leader_will_change_io_format_default_device() ->
    OriginalGroupLeader = ?x(group_leader()),
    Filename = "tmp_file_for_io",
    {ok, File} = file:open(Filename, [write]),
    io:format("0, usually you can see this, and then nothing~n"),
    ?x(group_leader(File, self())),
    io:format("1, now you can't see this in shell~n"),
    io:fwrite(standard_io, "2, this will go to file also~n", []),
    io:format(user, "!!! But you still can see this in Erlang shell~n", []),
    io:format("3, all io format redirected to file ~s~n", [Filename]),
    file:close(File),
    ?x(group_leader(OriginalGroupLeader, self())),
    io:format("4, io format again~n"),
    ok.

%%==============================================================================
%% demo Erlang doc
%%==============================================================================
%% @doc this is some spec
%% @spec demo_erlang_doc(integer(), atom()) -> bool()
%% @end
-spec demo_erlang_doc(X, Y::atom()) -> boolean() when X :: integer().
demo_erlang_doc(A, haha) when A > 0 ->
    true;
demo_erlang_doc(_, Y) when is_atom(Y) ->
    false.

%%==============================================================================
%% erlang term to string
%%==============================================================================
demo_term_to_string() ->
    demo_term_to_string({test, 1, 2, 3}).
demo_term_to_string(Term) ->
    ?x(Term),
    ?x(lists:flatten(io_lib:format("~p", [Term]))),
    ?x("not necessary to flatten if you just send it to I/O"),
    io:format("S: ~s~n", [io_lib:format("~p", [Term])]),
    io:format("S: ~p~n", [io_lib:format("~p", [Term])]),
    ok.


%%%----------------- erlang:process_info/1 -------------------------------------
%%==============================================================================
%% pid to name
%%==============================================================================
%% another use of process_info/1
pid_to_name() ->
    Pid = spawn_link(fun() -> timer:sleep(1000) end),
    pid_to_name(Pid).

pid_to_name(Pid) ->
    ?x(register(pid_to_name_test_name, Pid)),
    RegisteredName = process_info(Pid, registered_name),
    ?x(RegisteredName),
    ?x(unregister(pid_to_name_test_name)),
    ok.


%%==============================================================================
%% pretty print memory usage
%%==============================================================================
pretty_print_memory_usage() ->
    ?x(erlang:memory()),
    ?x('pretty-print'),
    lists:map(fun({X,M}) ->
                      io:format("~20.1fMB ~p~n",[M/1024/1024,X])
              end, erlang:memory()),
    ok.

%%==============================================================================
%% dot call
%%==============================================================================
%% Question in Erlang mailing list
%% I was looking inside eunit.hrl and found a number of defines like this one:
%% What is interesting for me is the DOT before the call. I've never seen
%% it before. I've googled, but found nothing.
%% Answer:
%% Hi! This artefact is just so that the eunit header file will work also in modules that use "packages" (experimental, now deprecated, so eunit.hrl can be updated when packages support is removed from OTP).
demo_dot_call() ->
    no_dot = .no_dot,
    io:format("You will not see leading dot in atom: (~p) ~n", [.my_dot_atom]).


%%==============================================================================
%% convert MD5 to hex string
%%==============================================================================
demo_md5_to_hex() ->
    MD5 = erlang:md5("hello, world"),
    ?x(md5_hex_stupid(MD5)),
    ?x(md5_hex_binary_comprehension(MD5)),
    ?x(md5_hex_best(MD5)),
    ok.

md5_hex_stupid(Bin) ->
    lists:flatten(lists:map(fun(X) ->
                                    [char(X div 16), char(X rem 16)]
                            end, binary_to_list(Bin))).
char(X) when X < 10 ->
    $0 + X;
char(X) ->
    $a + X - 10.

%% binary comprehension
md5_hex_binary_comprehension(Bin) ->
    lists:map(fun char/1, binary_to_list(<< <<X, Y>> || <<X:4, Y:4>> <= Bin >>)).

%% http://www.enchantedage.com/hex-format-hash-for-md5-sha1-sha256-and-sha512
md5_hex_best(Binary) when is_binary(Binary) ->
    lists:flatten(lists:map(fun(X) ->
                                    io_lib:format("~2.16.0b", [X])
                            end, binary_to_list(Binary))).

%%==============================================================================
%% -ifndef
%%==============================================================================
%% demo defined macro when compile a module
%% Learned this from rebar
-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

demo_compile_file_with_macro() ->
    %% by default, no VCS_INFO defined
    ?MODULE:print_macro(),
    %% compile it again, and define macro
    compile:file(?FILE, [{d, 'VCS_INFO', "user defined VCS_INFO"}]),
    c:l(?MODULE),
    ?MODULE:print_macro().
print_macro() ->
    io:format("VCS_INFO: ~p~n", [?VCS_INFO]).

%%==============================================================================
%% term_to_file and file_to_term
%%==============================================================================
term_to_file(Term, Filename) ->
    file:write_file(Filename, io_lib:format("~p.\n", [Term])).
file_to_term(Filename) ->
    {ok, [Term]} = file:consult(Filename),
    Term.

%%==============================================================================
%% how to check gen_server state
%%==============================================================================
-define(SERVER_NAME, simple_gen_server).
demo_show_gen_server_process_state() ->
    catch exit(whereis(?SERVER_NAME), kill),
    timer:sleep(500),
    {ok, Pid} = ?SERVER_NAME:start_link(),
    %% with erlang:process_info/1, you can't see state
    ?x(erlang:process_info(Pid)),
    %% but sys can show it
    ?x(sys:get_status(?SERVER_NAME)),
    ?x(sys:get_status(Pid)),
    Pid.

'sys:get_status/1'() ->
    Pid = spawn(fun demo_sys_get_status/0),
    ?x(sys:get_status(Pid, 3000)),
    ?x(sys:get_status(Pid)),
    ok.

demo_sys_get_status() ->
    receive
        {system, {From, Ref}, get_status} ->
            ?x(From),
            ?x(Ref),
            %% simulate reply to gen:call
            From ! {Ref, {dummy_status, {1,3,"hello, world"}}},
            demo_sys_get_status()
    after 30000 ->
            ok
    end.

%%==============================================================================
%% show running processes info in Erlang shell
%%==============================================================================
'c:regs/0'() ->
    Pid = spawn(fun() -> receive never_stop -> ok after 500 -> ok end end),
    register('===demo c:regs/0===', Pid),
    Pid ! 111,
    Pid ! 222,
    Pid ! 333,
    %% will show 3 Msgs
    c:regs().

%%==============================================================================
%% easy way to show module info in Erlang shell
%%==============================================================================
%% Many useful functions in module c, need to learn
show_module_info_in_erlang_shell() ->
    'c:m/1'().
'c:m/1'() ->
    c:m(?MODULE).

%%==============================================================================
%% find the process which consumes the most memory
%%==============================================================================
find_most_memory_process() ->
    lists:reverse(lists:keysort(2,[{P, erlang:process_info(P, heap_size)} || P <- erlang:processes()])).

%%==============================================================================
%% measure erlang performance with erlang:statistics/1, better than timer:tc/1
%% since you can put it anywhere
%%==============================================================================
show_run_time() ->
    statistics(wall_clock),
    statistics(runtime),
    {TimeOneSecond, _Result} = timer:tc(fun slow_fun/0),
    {_Total, TimeSinceLastCall} = statistics(wall_clock),
    {_Total2, RuntimeSinceLastCall} = statistics(runtime),
    ?x(TimeOneSecond),
    ?x(TimeSinceLastCall),
    ?x(RuntimeSinceLastCall),
    ok.

%% takes ~2 seconds in my Macbook Air
slow_fun() ->
    [lists:sum(lists:seq(1, N)) || N <- lists:seq(1, 7500)].

%% (foo@hundhaj)39> tips:show_all_threads_running_time(4).
%% [314]WallClock            = 3772
%% [315]Runtime              = 14220
%% statistics(runtime) will show all threads running time
%% NOTE: timer:sleep/1 will not account for runtime
show_all_threads_running_time(NumWorkers) ->
    statistics(wall_clock),
    statistics(runtime),
    Self = self(),
    spawn(fun() -> spawn_workers(Self, NumWorkers) end),
    receive_workers(NumWorkers),
    {_, WallClock} = statistics(wall_clock),
    {_, Runtime} = statistics(runtime),
    ?x(WallClock),
    ?x(Runtime).

receive_workers(0) ->
    done;
receive_workers(N) ->
    receive
        done ->
            receive_workers(N-1)
    end.

spawn_workers(_From, 0) ->
    ok;
spawn_workers(From, N) ->
    spawn(fun() -> spawn_workers(From, N-1) end),
    slow_fun(),
    From ! done.

%%------------------------------------------------------------------------------
%% user input
%%------------------------------------------------------------------------------
demo_input_password() ->
    io:format("password: "),
    Password = io:get_password(),
    ?x(Password),
    [_,_,_] = Chars = io:get_chars("type 3 chars, (enter is OK): ", 3),
    ?x(Chars),
    Line = io:get_line("type a line, end enter is included "),
    ?x(Line),
    $\n = lists:last(Line),
    ok.

%%------------------------------------------------------------------------------
%% compile and load all modules in current directory
%%------------------------------------------------------------------------------
demo_compile_all_and_load_modules() ->
    make:all([load]).

%%==============================================================================
%% Use ERL_LIBS to include code paths
%%==============================================================================
%% With ERL_LIBS environment variable, all ERL_LIBS/*/ebin/*.beam will be in
%% code path
-define(tmux_command_start_erlang_with_this_command, '
ERL_LIBS=${HOME}/erlang_libs erl
').
demo_use_erl_libs_to_include_code_paths() ->
    see_comments.

-define(tmux_save_erlang_shell_to_file, '
%% Mac OS X
script mylog erl
%% Linux
script -c "erl" mylog
').
demo_save_erlang_shell_to_file() ->
    ok.

%%% One process send the same message to itself, it will copy that message
%%% and in my Erlang VM, it shows message_alloc called
%%% I found this when I did mytcp and found it has nothing related big message
%%% or small message.
message_alloc_will_be_called_if_a_process_send_a_message_to_itself() ->
    spawn(fun() -> self() ! whatever end).
