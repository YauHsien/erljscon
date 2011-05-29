-module(stdlib).
-compile(export_all).
-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).

curry(F) when is_function(F, 2) ->
    fun(X) -> fun(Y) -> F(X, Y) end end.

curry(F, N) ->
    curry(F, N, []).
curry(F, 0, Args) ->
    apply(F, lists:reverse(Args));
curry(F, N, Args) when N > 0 ->
    fun(X) ->
	    curry(F, N-1, [X|Args])
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
