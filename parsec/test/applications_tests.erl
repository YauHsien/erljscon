-module(applications_tests).
-include_lib("eunit/include/eunit.hrl").

number_test() ->
    P = applications:number(),
    Inp = "123a",
    ?assertMatch([{"123","a"},{"12","3a"},{"1","23a"}], P(Inp)),
    ok.

world_test() ->
    P = applications:word(),
    Inp = "abc0",
    ?assertMatch([{"abc","0"},{"ab","c0"},{"a","bc0"}], P(Inp)),
    ok.

string_test() ->
    P = (applications:string())("begin"),
    Inp = "begin end",
    ?assertMatch([{"begin"," end"}], P(Inp)),
    ok.
