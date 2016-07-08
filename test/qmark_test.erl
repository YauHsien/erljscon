-module(qmark_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

do_test() ->
    P = fun json:escape/1,
    Inps = [{$", "\\\""}, {$\\, "\\\\"}, {$/, "\\\/"}, {$b, "\\b"},
	    {$f, "\\f"}, {$n, "\\n"}, {$r, "\\r"}, {$t, "\\t"}],
    Vs = lists:map(fun({X, Y}) -> {X, (P(X))(Y)} end, Inps),
    Es = [[#parsing{ parsed= "\\\"", rest= ""}],
	 [#parsing{ parsed= "\\\\", rest= ""}],
	 [#parsing{ parsed= "\\/", rest= ""}],
	 [#parsing{ parsed= "\\b", rest= ""}],
	 [#parsing{ parsed= "\\f", rest= ""}],
	 [#parsing{ parsed= "\\n", rest= ""}],
	 [#parsing{ parsed= "\\r", rest= ""}],
	 [#parsing{ parsed= "\\t", rest= ""}]
	],
    lists:map(fun({{X, V}, E}) -> io:fwrite("testing: ~s~n", [[X]]), ?assertMatch(E, V) end, lists:zip(Vs, Es)).
