-module(applications_tests).
-include_lib("eunit/include/eunit.hrl").

number_test() ->
    P = fun applications:number/1,
    Inp = "123a",
    ?assertMatch([{"123","a"},{"12","3a"},{"1","23a"}], P(Inp)),
    ok.

word_test() ->
    P = fun applications:word/1,
    Inp = "abc0",
    ?assertMatch([{"abc","0"},{"ab","c0"},{"a","bc0"}], P(Inp)),
    ok.

string_test() ->
    P = applications:string("begin"),
    Inp = "begin end",
    ?assertMatch([{"begin"," end"}], P(Inp)),
    ok.
