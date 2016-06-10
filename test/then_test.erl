-module(then_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

literal_test() ->
    P1 = parser:literal($a),
    P2 = parser:literal($b),
    V = (parser:then(P1, P2))("abcd"),
    E = [#parsing{ parsed= {$a, $b}, rest= "cd" }],
    ?assertEqual(E, V).

no_associativity_test() ->
    P1 = parser:literal($a),
    P2 = parser:literal($b),
    P3 = parser:literal($c),
    Input = "abc",
    Vt1 = (parser:then(P1, parser:then(P2, P3)))(Input),
    Vt2 = (parser:then(parser:then(P1, P2), P3))(Input),
    E = [#parsing{ parsed= {$a, {$b, $c}}, rest= [] }],
    ?assertEqual(E, Vt1),
    ?assertNotEqual(Vt1, Vt2).

