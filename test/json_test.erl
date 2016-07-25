-module(json_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/json.hrl").

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
    E= [#parsing{ parsed= "\"hello,world\"", rest= "" }],
    ?assertEqual(E, V).

string_Empty_test() ->
    P = json:string(),
    Inp = "\"\"",
    V = P(Inp),
    E = [#parsing{ parsed= "\"\"", rest= "" }],
    ?assertEqual(E, V).

num_test() ->
    P = json:num(),
    Inp = "0.31416e2a",
    V = P(Inp),
    E = [#parsing{ parsed= "0", rest= ".31416e2a" },
	 #parsing{ parsed= "0.31416", rest= "e2a" },
	 #parsing{ parsed= "0.31416e2", rest= "a" },
	 #parsing{ parsed= "0.3141", rest= "6e2a" },
	 #parsing{ parsed= "0.314", rest= "16e2a" },
	 #parsing{ parsed= "0.31", rest= "416e2a" },
	 #parsing{ parsed= "0.3", rest= "1416e2a" }
	],
    ?assertEqual(E, V).

true_test() ->
    P = json:true(),
    Inp = "true,",
    V = P(Inp),
    E = [#parsing{ parsed= "true", rest= "," }],
    ?assertEqual(E, V).

false_test() ->
    P = json:false(),
    Inp = " false,",
    V = P(Inp),
    E = [#parsing{ parsed= "false", rest= "," }],
    ?assertEqual(E, V).

null_test() ->
    P = json:null(),
    Inp = "null ,",
    V = P(Inp),
    E = [#parsing{ parsed= "null", rest= "," },
	 #parsing{ parsed= "null", rest= " ," }],
    ?assertEqual(E, V).

key_value_test() ->
    P = json:key_value(),
    Inp = "\"hello\":\"world\"",
    V = P(Inp),
    E = [#parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" }],
    ?assertEqual(E, V).

key_value_num_test() ->
    P = json:key_value(),
    Inp = "\"hello\":3.14",
    V = P(Inp),
    E = [#parsing{ parsed= {"\"hello\"", "3"}, rest= ".14" },
	 #parsing{ parsed= {"\"hello\"", "3.14"}, rest= "" },
	 #parsing{ parsed= {"\"hello\"", "3.1"}, rest= "4" }],
    ?assertEqual(E, V).

object_test() ->
    P = json:object(),
    Inp = "{\"hello\":1,\"world\":2}",
    V = P(Inp),
io:fwrite("~p~n", [V]),
    E = [#parsing{ parsed= #object{ elements= [{"\"hello\"", "1"}, {"\"world\"", "2"}] },
		   rest= "" }],
    ?assertEqual(E, V).
	
array_test() ->
    P = json:array(),
    Inp = "[\"hello\",\"world\",1,2,3]",
    V = P(Inp),
    E = [#parsing{ parsed= ["\"hello\"", "\"world\"", "1", "2", "3"],
		   rest= "" }],
    ?assertEqual(E, V).

