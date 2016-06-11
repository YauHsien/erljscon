-module(example_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/example.hrl").

expn_test() ->
    P = parser_example:expn(),
    Input = "2+(4-1)*3",
    V = P(Input),
    E = [#parsing{ parsed= #plus{ left= #num{ value= "2" },
				  right= #times{ left= #minus{ left= #num{ value= "4" },
							       right= #num{ value= "1" } },
						 right= #num{ value= "3" } } },
		   rest= "" },
	 #parsing{ parsed= #plus{ left= #num{ value= "2" },
				  right= #minus{ left= #num{ value= "4" },
						 right= #num{ value= "1" } } },
		   rest= "*3" },
	 #parsing{ parsed= #num{ value= "2" }, rest= "+(4-1)*3" }],
    ?assertEqual(E, V).

eval_test() ->
    Input = "2+(4-1)*3",
    V = parser_example:eval(Input),
    E = [#parsing{ parsed= 11, rest= "" },
	 #parsing{ parsed= 5, rest= "*3" },
	 #parsing{ parsed= 2, rest= "+(4-1)*3" }],
    ?assertEqual(E, V).
    
