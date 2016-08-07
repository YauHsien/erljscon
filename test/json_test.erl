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
    P = parser:nibble(json:string()),
    Inp = "   \"hello,world\" ",
    V = P(Inp),
    E= [#parsing{ parsed= "\"hello,world\"", rest= "" },
	#parsing{ parsed= "\"hello,world\"", rest= " " }],
    ?assertEqual(E, V).

string_Empty_test() ->
    P = parser:nibble(json:string()),
    Inp = "   \"\" ",
    V = P(Inp),
    E = [#parsing{ parsed= "\"\"", rest= "" },
	 #parsing{ parsed= "\"\"", rest= " " }],
    ?assertEqual(E, V).

num_test() ->
    P = parser:nibble(json:num()),
    Inp = " 0.31416e2 ",
    V = P(Inp),
    E = [#parsing{ parsed= "0", rest= ".31416e2 " },
	 #parsing{ parsed= "0.31416", rest= "e2 " },
	 #parsing{ parsed= "0.31416e2", rest= "" },
	 #parsing{ parsed= "0.31416e2", rest= " " },
	 #parsing{ parsed= "0.3141", rest= "6e2 " },
	 #parsing{ parsed= "0.314", rest= "16e2 " },
	 #parsing{ parsed= "0.31", rest= "416e2 " },
	 #parsing{ parsed= "0.3", rest= "1416e2 " }
	],
    ?assertEqual(E, V).

true_test() ->
    P = json:true(),
    Inp = "  true,",
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
    Inp = "\"hello\" : \"world\"",
    V = P(Inp),
    E = [#parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" },
	 #parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" },
	 #parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" },
	 #parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" }],
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
    Inp = "{\"hello\":1,\"world\":2 }",
    V = P(Inp),
    E = [#parsing{ parsed= #object{ elements= [{"\"hello\"", "1"}, {"\"world\"", "2"}] }, rest= "" },
	 #parsing{ parsed= #object{ elements= [{"\"hello\"", "1"}, {"\"world\"", "2"}] }, rest= "" }],
    ?assertEqual(E, V).
	
array_test() ->
    P = json:array(),
    Inp = "  [\"hello\" ,\"world\",1,2,3 ]",
    V = P(Inp),
    E = [#parsing{ parsed= ["\"hello\"", "\"world\"", "1", "2", "3"],
		   rest= "" }],
    ?assertEqual(E, V).

%% example_test() ->
%%     P = json:json(),
%%     Inp = "{
%%     \"glossary\": {
%%         \"title\": \"example glossary\",
%% 		\"GlossDiv\": {
%%             \"title\": \"S\",
%% 			\"GlossList\": {
%%                 \"GlossEntry\": {
%%                     \"ID\": \"SGML\",
%% 					\"SortAs\": \"SGML\",
%% 					\"GlossTerm\": \"Standard Generalized Markup Language\",
%% 					\"Acronym\": \"SGML\",
%% 					\"Abbrev\": \"ISO 8879:1986\",
%% 					\"GlossDef\": {
%%                         \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
%% 						\"GlossSeeAlso\": [\"GML\", \"XML\"]
%%                     },
%% 					\"GlossSee\": \"markup\"
%%                 }
%%             }
%%         }
%%     }
%% }",
%%     GlossDef = #object{ elements= [{ "\"para\"", "\"A meta-markup langauge, used to create markup langugages such as DockBook.\"" },
%% 				   { "\"GlossSeeAlso\"", ["\"GML\"", "\"XML\""] }] },
%%     GlossEntry = #object{ elements= [{"\"ID\"", "\"SGML\""},
%% 				     {"\"SortAs\"", "\"SGML\""},
%% 				     {"\"GlossTerm\"", "\"Standard Generalized Markup Language\""},
%% 				     {"\"Acronym\"", "\"SGML\""},
%% 				     {"\"Abbrev\"", "\"ISO 8879:1986\""},
%% 				     {"\"GlossDef\"", GlossDef},
%% 				     {"\"GlossSee\"", "\"markup\""}] },
%%     GlossList = #object{ elements= [{ "\"GlossEntry\"", GlossEntry }] },
%%     GlossDiv = #object{ elements= [{ "\"title\"", "\"S\"" },
%% 				   { "\"GlossList\"", GlossList }] },
%%     Glossary = #object{ elements= [{ "\"title\"", "\"example glossary\"" },
%% 				   { "\"GlossDiv\"", GlossDiv }] },
%%     Obj = #object{ elements= [{ "\"glossary\"", Glossary }] },
%%     V = P(Inp),
%%     E = [#parsing{ parsed= Obj, rest= "" }],
%%     ?assertEqual(E, V).
