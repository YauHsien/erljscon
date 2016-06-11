-module(symbol_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

symbol_test() ->
    P = parser:symbol("hi"),
    Input = "  hi   there",
    V = P(Input),
    E = [#parsing{ parsed= "hi", rest= "there" },
	 #parsing{ parsed= "hi", rest= " there" },
	 #parsing{ parsed= "hi", rest= "  there" },
	 #parsing{ parsed= "hi", rest= "   there" }],
    ?assertEqual(E, V).

