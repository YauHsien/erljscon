-module(fn_util).
-compile(export_all).
-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).

%curry(F) when is_function(F, 2) ->
%    fun(X) -> fun(Y) -> F(X, Y) end end.

curry(F) ->
    {_M, _F, A} = parts(F),
    curry(F, A, []).

curry(F, A, C) when A == length(C) ->
    apply(F, lists:reverse(C));
curry(F, A, C) when A > length(C) ->
    fun(X) ->
	    curry(F, A, [X|C])
    end.

curryl(F, N) ->
    curryl(F, N, []).
curryl(F, N, Args) ->
    ?DELAY(curry(F, N, Args)).

curryll(F, N) ->
    curryll(F, N, []).
curryll(F, 0, Args) ->
    fun() -> apply(F, lists:reverse(Args)) end;
curryll(F, N, Args) when N > 0 ->
    fun() -> fun(X) ->
	    curryll(F, N-1, [X|Args])
    end end.

cons({X,Y}) ->
    if
	is_list(Y) ->
	    [X|Y];
	true ->
	    [X,Y]
    end.

append({Xs, Ys}) ->
    erlang:append(Xs, Ys).

parts(F) when is_function(F) ->
    P = erlang:fun_info(F),
    G = fun proplists:get_value/2,
    [M, F1, A] =
	lists:map(fun([G1, As1]) -> apply(G1, As1) end,
		  [[G, [module, P]],
		   [G, [name, P]],
		   [G, [arity, P]]]),
    {M, F1, A}.

prepend_curry(F, N) ->
    lists:map(fun(N1) -> prepend_curry1(F, N1) end,
	      lists:seq(1, N)).

prepend_curry1(F, N1) ->
    ok.
