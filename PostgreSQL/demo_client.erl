-module(demo_client).

-export([ default_connection/0
        , demo_other_host_connection/0
        , demo_simple_connection/0
        , demo_get_table_spec/0
        ]).

-compile(export_all).

-define(x(X), begin
                  fun() ->
                     __YY__ = X,
                     pretty_print(?MODULE, ?LINE, ??X, __YY__),
                     __YY__
                  end()
              end).

-define(DB_HOST, "192.168.88.101").
-define(DB_IP, "192.168.88.101").
-define(DB_USER, "wangjia").
-define(DB, "my_db").

pretty_print(M, L, S, V) ->
    io:format("[~5s:~3s]~-20s = ~p~n",
              [atom_to_list(M), integer_to_list(L), S, V]).

call_stack() ->
  try erlang:error(x) catch _:_->tl(erlang:get_stacktrace()) end.

start_deps() ->
    ?x(application:start(crypto)),
    ?x(application:start(public_key)),
    ?x(application:start(ssl)),
    ok.

default_connection() ->
    {ok, Conn} = ?x(pgsql:connect(?DB_HOST, ?DB_USER, "", [{database, ?DB}])),
    Conn.

%% to make this work, has to set
%% listen_addresses = '*' in ${PGDATA}/postgresql.conf
%% andalso
%% insert next line to pg_hba.conf
%% host    all         all         192.168.88.1/32        trust
demo_other_host_connection() ->
    {ok, Conn} = ?x(pgsql:connect(?DB_IP, ?DB_USER,"",[{database, ?DB}])),
    Conn.

demo_simple_connection() ->
    {ok, Conn} = ?x(pgsql:connect(?DB_HOST,?DB_USER,"",[{database, ?DB}])),
    {ok, Columns, Values} = pgsql:squery(Conn, "SELECT * FROM demo_table;"),
    ?x(Columns),
    ?x(Values),
    lists:map(fun print_column/1, Columns),
    ?x(pgsql:close(Conn)),
    ok.

%% http://www.postgresql.org/docs/8.1/static/datatype.html#DATATYPE-TABLE
%% int8 -> bigint
%% int4, int -> integer
%% [bool,date,inet,int4,int8,numeric,text,timestamp]
print_column({column, Name, Type, Size, Modifier, Format}) ->
    io:format("========= column info ============~n"),
    io:format("    Name:   ~p~n", [Name]),
    io:format("    Type:   ~p~n", [Type]),
    io:format("    Size:   ~p~n", [Size]),
    io:format("Modifier:   ~p~n", [Modifier]),
    io:format("  Format:   ~p~n", [Format]),
    io:format("=========== end ==================~n"),
    ok.

%%------------------------------------------------------------------------------
%% get table spec
%%------------------------------------------------------------------------------
%% Precondition:
%% DB: my_db
%% Table: demo_table
demo_get_table_spec() ->
    Conn = default_connection(),
    FullSpec = table_spec(Conn, "demo_table"),
    ?x(FullSpec),
    NameTypeSpec = table_spec(Conn, "demo_table", name_and_type),
    ?x(NameTypeSpec),
    ok.

table_spec(C, Table) ->
    table_spec(C, Table, full).
table_spec(C, Table, Flag) ->
    {ok,Columns,_} = pgsql:squery(C, "SELECT * FROM " ++ Table ++ " LIMIT 1;"),
    Columns2 = lists:map(fun column_binary_to_list/1, Columns),
    lists:map(fun(Column) -> filter_column_spec(Column, Flag) end, Columns2).

column_binary_to_list({column, Name, Type, Size, Modifier, Format}) ->
    {column, binary_to_list(Name), Type, Size, Modifier, Format}.

filter_column_spec(Column, full) ->
    Column;
filter_column_spec({column, Name, Type, _, _, _}, name_and_type) ->
    {Name, Type}.
