-module(alt_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

identity_test() ->
    P1 = parser:p(fun parser:succeed/2, "hello"),
    P2 = parser:p(fun parser:fail/1),
    Input = "hello,world",
    V1 = (parser:alt(P1, P2))(Input),
    V2 = (parser:alt(P2, P1))(Input),
    V3 = P1(Input),
    E = [#parsing{ parsed= "hello", rest= Input }],
    ?assertEqual(E, V1),
    ?assertEqual(V1, V2),
    ?assertEqual(V2, V3).

associativity_test() ->
    P1 = parser:p(fun parser:succeed/2, "hello"),
    P2 = parser:p(fun parser:fail/1),
    P3 = parser:p(fun parser:succeed/2, "world"),
    Input = "hello,world",
    V1 = (parser:alt(parser:alt(P1, P2), P3))(Input),
    V2 = (parser:alt(P1, parser:alt(P2, P3)))(Input),
    E = [#parsing{ parsed= "hello", rest= Input },
	 #parsing{ parsed= "world", rest= Input }],
    ?assertEqual(E, V1),
    ?assertEqual(V1, V2).

