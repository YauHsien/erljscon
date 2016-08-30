%% -*- coding: utf-8 -*-
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



chars_test() ->
    P = json:chars(),
    I = [ "嗨，世界" ],
    lists:map(fun(Inp= "嗨，世界")  -> ?assertEqual( [#parsing{ parsed= "嗨", rest= "，世界" },
						      #parsing{ parsed= "嗨，", rest= "世界" },
						      #parsing{ parsed= "嗨，世", rest= "界" },
						      #parsing{ parsed= "嗨，世界", rest= "" }], P(Inp) )
	      end, I).



char_test() ->
    P = json:char(),
    I = [ [$\x{0}], "\\t", "\\u1234" ],
    lists:map(fun(Inp= [$\x{0}])  -> ?assertEqual( [], P(Inp) );
		 (Inp= "\\t")     -> ?assertEqual( [#parsing{ parsed= "\\t", rest= "" }], P(Inp) );
		 (Inp= "\\u1234") -> ?assertEqual( [#parsing{ parsed= "\\u1234", rest= "" }], P(Inp) )
	      end, I).



acceptable_char_test() ->
    P = json:acceptable_char(),
    I = [ "\"", "\\", [$\x{0}], "嗨，世界" ],
    lists:map(fun(Inp= "\"")       -> ?assertEqual( [], P(Inp) );
		 (Inp= "\\")       -> ?assertEqual( [], P(Inp) );
		 (Inp= [$\x{0}])   -> ?assertEqual( [], P(Inp) );
		 (Inp= "嗨，世界") -> ?assertEqual( [#parsing{ parsed= "嗨", rest= "，世界" }], P(Inp) )
	      end, I).



escape_sequence_test() ->
    P = json:escape_sequence(),
    I = [ "\\\\", "\\t", "\\n" ],
    lists:map(fun(Inp= "\\\\") -> ?assertEqual( [#parsing{ parsed= "\\\\", rest= "" }], P(Inp) );
		 (Inp= "\\t")  -> ?assertEqual( [#parsing{ parsed= "\\t", rest= "" }], P(Inp) );
		 (Inp= "\\n")  -> ?assertEqual( [#parsing{ parsed= "\\n", rest= "" }], P(Inp) )
	      end, I).



number_test() ->
    P = json:number(),
    I = [ "12", "12.3", "12e-3", "-12.3E4" ],
    lists:map(fun(Inp= "12")      -> ?assertEqual( [#parsing{ parsed= "1", rest= "2" },
						    #parsing{ parsed= "12", rest= "" }], P(Inp) );
		 (Inp= "12.3")    -> ?assertEqual( [#parsing{ parsed= "1", rest= "2.3" },
						    #parsing{ parsed= "12", rest= ".3" },
						    #parsing{ parsed= "12.3", rest= "" }], P(Inp) );
		 (Inp= "12e-3")   -> ?assertEqual( [#parsing{ parsed= "1", rest= "2e-3" },
						    #parsing{ parsed= "12", rest= "e-3" },
						    #parsing{ parsed= "12e-3", rest= "" }], P(Inp) );
		 (Inp= "-12.3E4") -> ?assertEqual( [#parsing{ parsed= "-1", rest= "2.3E4" },
						    #parsing{ parsed= "-12", rest= ".3E4" },
						    #parsing{ parsed= "-12.3", rest= "E4" },
						    #parsing{ parsed= "-12.3e4", rest= "" }], P(Inp) )
	      end, I).



int_test() ->
    P = json:int(),
    I = [ "01", "123", "-01", "-123" ],
    lists:map(fun(Inp= "01")   -> ?assertEqual( [#parsing{ parsed= "0", rest= "1" }], P(Inp) );
		 (Inp= "123")  -> ?assertEqual( [#parsing{ parsed= "1", rest= "23" },
						 #parsing{ parsed= "12", rest= "3" },
						 #parsing{ parsed= "123", rest= "" }], P(Inp) );
		 (Inp= "-01")  -> ?assertEqual( [#parsing{ parsed= "-0", rest= "1" }], P(Inp) );
		 (Inp= "-123") -> ?assertEqual( [#parsing{ parsed= "-1", rest= "23" },
						 #parsing{ parsed= "-12", rest= "3" },
						 #parsing{ parsed= "-123", rest= "" }], P(Inp) )
	      end, I).



frac_test() ->
    P = json:frac(),
    I = [ ".0", ".234" ],
    lists:map(fun(Inp= ".0")   -> ?assertEqual( [#parsing{ parsed= ".0",   rest= "" }], P(Inp) );
		 (Inp= ".234") -> ?assertEqual( [#parsing{ parsed= ".2", rest= "34" },
						 #parsing{ parsed= ".23", rest= "4" },
						 #parsing{ parsed= ".234", rest= "" }], P(Inp) )
	      end, I).



exp_test() ->
    P = json:exp(),
    I = [ "e-3", "E+21", "e1" ],
    lists:map(fun(Inp= "e-3")  -> ?assertEqual( [#parsing{ parsed= "e-3", rest= "" }],  P(Inp) );
		 (Inp= "E+21") -> ?assertEqual( [#parsing{ parsed= "e+2", rest= "1" },
						 #parsing{ parsed= "e+21", rest= "" }], P(Inp) );
		 (Inp= "e1")   -> ?assertEqual( [#parsing{ parsed= "e1", rest= "" }],   P(Inp) )
	      end, I).



digits_test() ->
    P = json:digits(),
    I = [ "0", "01", "689" ],
    lists:map(fun(Inp= "0")   -> ?assertEqual( [#parsing{ parsed= "0",   rest= "" }], P(Inp) );
		 (Inp= "01")  -> ?assertEqual( [#parsing{ parsed= "0",   rest= "1" },
						#parsing{ parsed= "01",  rest= "" }], P(Inp) );
		 (Inp= "689") -> ?assertEqual( [#parsing{ parsed= "6",   rest= "89" },
						#parsing{ parsed= "68",  rest= "9" },
						#parsing{ parsed= "689", rest= "" }], P(Inp) )
	      end, I).



e_test() ->
    P = json:e(),
    I = [ "e2", "e+2", "e-2", "E2", "E+2", "E-2" ],
    lists:map(fun(Inp= [$e,$+|_]) -> ?assertEqual( [#parsing{ parsed= 'e+', rest= "2" },
						    #parsing{ parsed= 'e',  rest= "+2" }], P(Inp) );
		 (Inp= [$e,$-|_]) -> ?assertEqual( [#parsing{ parsed= 'e-', rest= "2" },
						    #parsing{ parsed= 'e',  rest= "-2" }], P(Inp) );
		 (Inp= [$e|_])    -> ?assertEqual( [#parsing{ parsed= e,    rest= "2" }],  P(Inp) );
		 (Inp= [$E,$+|_]) -> ?assertEqual( [#parsing{ parsed= 'e+', rest= "2" },
						    #parsing{ parsed= 'e',  rest= "+2" }], P(Inp) );
		 (Inp= [$E,$-|_]) -> ?assertEqual( [#parsing{ parsed= 'e-', rest= "2" },
						    #parsing{ parsed= 'e',  rest= "-2" }], P(Inp) );
		 (Inp= [$E|_])    -> ?assertEqual( [#parsing{ parsed= 'e',  rest= "2" }],  P(Inp) ) end, I).
