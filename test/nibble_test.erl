-module(nibble_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

nibble_test() ->
    P = parser:nibble(parser:string("hello")),
    Input = "  hello
   ,world",
    V = P(Input),
    ?assertMatch([#parsing{ parsed= "hello", rest= ",world" },
		  #parsing{ parsed= "hello", rest= " ,world" }|_], V).

