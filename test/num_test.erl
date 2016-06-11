-module(num_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

num_test() ->
    P = parser:num(),
    Input = "123hello,world",
    V = P(Input),
    E = [#parsing{ parsed= "123", rest= "hello,world" },
	 #parsing{ parsed= "12", rest= "3hello,world" },
	 #parsing{ parsed= "1", rest= "23hello,world" }],
    ?assertEqual(E, V).

