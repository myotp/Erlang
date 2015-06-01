%%% To run this demo, make sure you have EQC.
%% How to run
%% erl -pa eqcmini/eqc-1.0.1/ebin
%% c(demo).
%% eqc:quickcheck(demo:prop_sum()).

-module(demo).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_sum() ->
    ?FORALL({A, B}, {int(), int()},
            A+B == magic_sum(A, B)).

magic_sum(A, B) ->
    A + B.

