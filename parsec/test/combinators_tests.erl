-module(combinators_tests).
-include_lib("eunit/include/eunit.hrl").

alt_identity_test() ->
    P = primitives:literal($3),
    C1 = combinators:alt(primitives:fail(), P),
    C2 = combinators:alt(P, primitives:fail()),
    Inp = "345",
    ?assertEqual(P(Inp), C1(Inp)),
    ?assertEqual(P(Inp), C2(Inp)),
    ok.

then_test() ->
    P1 = primitives:literal($a),
    P2 = primitives:literal($b),
    C = combinators:then(P1, P2),
    Inp = "abcd",
    ?assertMatch([{{$a,$b},"cd"}], C(Inp)),
    ok.

using_test() ->
    P = primitives:literal($h),
    F = fun(C) -> {literal,C} end,
    C = combinators:using(P, F),
    Inp = "hello,world",
    ?assertMatch([{{literal,$h},"ello,world"}], C(Inp)),
    ok.

many_test() ->
    P = primitives:literal($a),
    C = combinators:many(P),
    Inp = "aaab",
    ?assertMatch([{"aaa","b"},{"aa","ab"},{"a","aab"},{"","aaab"}], C(Inp)),
    ok.
