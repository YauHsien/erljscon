-module(many_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

many_test() ->
    P = parser:many(parser:literal($a)),
    Input = "aaab",
    V = P(Input),
    E = [#parsing{ parsed= "aaa", rest= "b" },
	 #parsing{ parsed= "aa", rest= "ab" },
	 #parsing{ parsed= "a", rest= "aab" },
	 #parsing{ parsed= "", rest= "aaab" }],
    ?assertEqual(E, V).

