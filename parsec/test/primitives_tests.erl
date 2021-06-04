-module(primitives_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/include/types.hrl").

fail_test() ->
    F1 = fun primitives:fail/1,
    Inp = "hello,world",
    ?assertMatch([], F1(Inp)),
    ok.

literal_test() ->
    L3 = primitives:literal($3),
    Inp = "345",
    ?assertMatch([{$3,"45"}], L3(Inp)),
    ok.

satisfy_test() ->
    Predicate = fun($h) -> true; (_) -> false end,
    S1 = fun primitives:satisfy/1,
    Inp = "hello,world",
    ?assertMatch([], (S1(Predicate))("")),
    ?assertMatch([], (S1(Predicate))("oops")),
    ?assertMatch([{$h,"ello,world"}], (S1(Predicate))(Inp)),
    ok.

succeed_test() ->
    S1 = fun primitives:succeed/1,
    Inp = "hello,world",
    ?assertMatch([{hello,Inp}], (S1(hello))(Inp)),
    ok.
