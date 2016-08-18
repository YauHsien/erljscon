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
    Inp = "   \"hello,world\" ",
    V = P(Inp),
    E= #parsing{ parsed= "\"hello,world\"", rest= " " },
    ?assertMatch([E|_], V).

string_Empty_test() ->
    P = json:string(),
    Inp = "   \"\"  ",
    V = P(Inp),
    E = #parsing{ parsed= "\"\"", rest= "  " },
    ?assertMatch([E|_], V).

integer_test() ->
    P = json:integer(),
    Inp = "10 ",
    V = P(Inp),
    E = #parsing{ parsed= "10", rest= " " },
    ?assertMatch([E|_], V).

num_test() ->
    P = json:num(),
    Inp = "  0.31416e2 ",
    V = P(Inp),
    E = #parsing{ parsed= "0.31416e2", rest= " " },
    ?assertMatch([E|_], V).

true_test() ->
    P = json:true(),
    Inp = "  true ,",
    V = P(Inp),
    E = #parsing{ parsed= "true", rest= "," },
    ?assertMatch([E|_], V).

false_test() ->
    P = json:false(),
    Inp = " false,",
    V = P(Inp),
    E = #parsing{ parsed= "false", rest= "," },
    ?assertMatch([E|_], V).

null_test() ->
    P = json:null(),
    Inp = "null ,",
    V = P(Inp),
    E = #parsing{ parsed= "null", rest= "," },
    ?assertMatch([E|_], V).

key_value_test() ->
    P = json:key_value(),
    Inp = "   \"hello\" :   \"world\"   ",
    V = P(Inp),
    E = #parsing{ parsed= {"\"hello\"", "\"world\""}, rest= "" },
    ?assertMatch([E|_], V).

key_value_num_test() ->
    P = json:key_value(),
    Inp = "  \"hello\": 3.14 ",
    V = P(Inp),
    E = #parsing{ parsed= {"\"hello\"", "3.14"}, rest= "" },
    ?assertMatch([E|_], V).

object_test() ->
    P = json:object(),
    Inp = "  {   \"hello\" :1, \"world\": 2 }",
    V = P(Inp),
    E = #parsing{ parsed= #object{ elements= [{"\"hello\"", "1"}, {"\"world\"", "2"}] }, rest= "" },
    ?assertMatch([E|_], V).
	
array_test() ->
    P = json:array(),
    Inp = "   [ \"hello\" , \"world\" ,  1,2, 3   ]   ",
    V = P(Inp),
    E = #parsing{ parsed= ["\"hello\"", "\"world\"", "1", "2", "3"], rest= "" },
    ?assertMatch([E|_], V).

array_empty_test() ->
    P = json:array(),
    Inp = "  [ ]  ",
    V = P(Inp),
    E = #parsing{ parsed= [], rest= "" },
    ?assertMatch([E|_], V).

%% json_test() ->
%%     P = json:json(),
%%     Inp = "{\"hello\": \"world\"}",
%%     V = P(Inp),
%%     E = #parsing{ parsed= #object{}, rest= "" },
%%     ?assertMatch([E|_], V).

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
%%     E = #parsing{ parsed= Obj, rest= "" },
%%     ?assertMatch([E|_], V).
