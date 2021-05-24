-module(primitives_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/include/types.hrl").

epsilon_test() ->
    E = fun primitives:epsilon/0,
    CurriedE = fun primitives:epsilon/0,
    ?assertMatch([], E()),
    ?assertMatch("", E()),
    ?assertMatch("", CurriedE()),
    ok.

fail_test() ->
    F0 = primitives:curry(fun primitives:fail/0),
    F1 = fun primitives:fail/1,
    Inp = "hello,world",
    ?assertMatch([], F0(Inp)),
    ?assertMatch([], F1(Inp)),
    ok.

satisfy_test() ->
    Predicate = fun($h) -> true; (_) -> false end,
    S1 = fun primitives:satisfy/1,
    S2 = primitives:curry(fun primitives:satisfy/2),
    Inp = "hello,world",
    ?assertMatch([], (S1(Predicate))("")),
    ?assertMatch([], (S1(Predicate))("oops")),
    ?assertMatch([{$h,"ello,world"}], (S1(Predicate))(Inp)),
    ?assertMatch([], (S2(Predicate))("")),
    ?assertMatch([], (S2(Predicate))("oops")),
    ?assertMatch([{$h,"ello,world"}], (S2(Predicate))(Inp)),
    ok.

-spec succeed() -> fun((Val::to()) -> parser(from(),to())).
succeed() ->
    fun(Val) -> fun(Inp) -> [{Val,Inp}] end end.

succeed_test() ->
    S0 = primitives:curry(fun succeed/0),
    S1 = fun primitives:succeed/1,
    S2 = fun primitives:succeed/2,
    CurriedS2 = primitives:curry(S2),
    Inp = "hello,world",
    ?assertMatch([{hello,Inp}], (S0(hello))(Inp)),
    ?assertMatch([{hello,Inp}], (S1(hello))(Inp)),
    ?assertMatch([{hello,Inp}], (S2(hello,Inp))),
    ?assertMatch([{hello,Inp}], (CurriedS2(hello))(Inp)),
    ok.
