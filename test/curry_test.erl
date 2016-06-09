-module(curry_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/curry.hrl").

curry(_, _) ->
    hello_world.

curry_test() ->
    P = fn_util:curry(fun parser:succeed/2),
    % io:fwrite("~p~n", [erlang:fun_info(P)]),
    % io:fwrite("~p~n",
    %           [process_info(proplists:get_value(pid, erlang:fun_info(P)))]),
    ?assertEqual(self(),
		 proplists:get_value(pid, erlang:fun_info(P))),
    % io:fwrite("~p~n", [fn_util:parts(P)]),
    P1 = apply(P, [5]),
    % io:fwrite("~p~n", [erlang:fun_info(P1)]),
    P2 = apply(P1, ["hello,world"]),
    ?assertEqual(false, is_function(P2)),
    V = P2,
    E = [#parsing{ parsed= 5, rest= "hello,world" }],
    ?assertEqual(E, V).


