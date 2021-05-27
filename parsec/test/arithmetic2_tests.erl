-module(arithmetic2_tests).
-include_lib("eunit/include/eunit.hrl").

expn_test() ->
    P = fun arithmetic2:expn/1,
    Inp = "2+(4-1)*3",
    ?assertMatch([{11,""},{5,"*3"},{2,"+(4-1)*3"}], P(Inp)).

