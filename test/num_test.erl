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

colon_num_test() ->
    P = parser:then(parser:literal($:), parser:num()),
    Inp = ":1",
    V = P(Inp),
    E = [#parsing{ parsed= {$:, "1"}, rest= "" }],
    ?assertEqual(E, V).


