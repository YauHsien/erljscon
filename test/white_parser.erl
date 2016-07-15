-module(white_parser).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

white_test() ->
    P = parser:white(),
    Inp = " \n",
    V = P(Inp),
    E = [#parsing{ parsed= " \n", rest= "" },
	 #parsing{ parsed= " ", rest= "\n" },
	 #parsing{ parsed= "", rest= " \n" }
	],
    ?assertEqual(E, V).

