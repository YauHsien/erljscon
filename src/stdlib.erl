-module(stdlib).
-compile(export_all).

cons({X,Y}) ->
    if
	is_list(Y) ->
	    [X|Y];
	true ->
	    [X,Y]
    end.

append({Xs, Ys}) ->
    erlang:append(Xs, Ys).
