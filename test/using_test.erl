-module(using_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

-spec dummy({char(), char()}) -> string();
	   ({string(), char()}) -> string().
dummy({A, B}) when not is_list(A) ->
    [A, B].

using_test() ->
    P1 = parser:then(parser:literal($a), parser:literal($b)),
    P2 = parser:using(P1, fun dummy/1),
    Input = "abc",
    V1 = P1(Input),
    E1 = [#parsing{ parsed= {$a, $b}, rest= "c" }],
    V2 = P2(Input),
    E2 = [ Pr#parsing{ parsed= dummy(Pr#parsing.parsed) } || #parsing{}= Pr <- V1],
    E = [#parsing{ parsed= "ab", rest= "c" }],
    ?assertEqual(E1, V1),
    ?assertEqual(E, E2),
    ?assertEqual(E2, V2).
