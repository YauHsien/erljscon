-module(arithmetic_tests).
-include_lib("eunit/include/eunit.hrl").

expn_test() ->
    P = arithmetic:expn(),
    Inp = "2+(4-1)*3",
    ?assertMatch(
       [ {{add,{num,2},{mul,{sub,{num,4},{num,1}},{num,3}}}, ""},
         {{add,{num,2},{sub,{num,4},{num,1}}},               "*3"},
         {{num,2},                                           "+(4-1)*3"}
       ],
       P(Inp)),
    ok.
