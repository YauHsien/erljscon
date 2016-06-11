-module(return_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

return_test() ->
    P = parser:return(parser:string("hello"), hello),
    Input = "hello,world",
    V = P(Input),
    E = [#parsing{ parsed= hello, rest= ",world" }],
    ?assertEqual(E, V).
