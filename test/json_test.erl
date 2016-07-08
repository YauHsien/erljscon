-module(json_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

unicode_test() ->
    P = json:unicode(),
    Inp = "\\u1234",
    V = P(Inp),
    E = [#parsing{ parsed= "\\u1234", rest= "" }],
    ?assertEqual(E, V).

escape_sequence_test() ->
    P = parser:then(
	  parser:string("hello"),
	  json:escape_sequence()
	 ),
    Inp = "hello\\n",
    V = P(Inp),
    E = [#parsing{ parsed= {"hello", "\\n"}, rest= "" }],
    ?assertEqual(E, V).

string_NonEmpty_test() ->
    P = json:string(),
    Inp = "\"hello,world\"",
    V = P(Inp),
    E1= #parsing{ parsed= "\"hello,world\"", rest= "" },
    E = [E1, E1, E1, E1],
    ?assertEqual(E, V).

string_Empty_test() ->
    P = json:string(),
    Inp = "\"\"",
    V = P(Inp),
    E = [#parsing{ parsed= "\"\"", rest= "" }],
    ?assertEqual(E, V).

