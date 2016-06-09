-module(fn_util_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

parts_test() ->
    {M, F, A} = V = fn_util:parts(fun parser:succeed/2),
    E = {parser, succeed, 2},
    io:fwrite("~p ~n", [fun M:F/A]),
    ?assertEqual(E, V).
