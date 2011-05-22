-module(parser).
-compile(export_all).

%% Walk through G. Hutton's paper.

succeed(V, Inp) -> [{V, Inp}].
succeed(V) ->
    fun(Inp) ->
	    [{V, Inp}]
    end.
fail(_inp) -> [].
fail() ->
    fun(_inp) ->
	    []
    end.

satisfy(_p, []) -> [];
satisfy(P, [X|Xs]) ->
    case apply(P, [X]) of
	true ->
	    succeed(X, Xs);
	false ->
	    fail(Xs)
    end.

satisfy(Predicate) ->
    fun(List) ->
	    case List of
		[] ->
		    [];
		[X|Xs] ->
		    case apply(Predicate, [X]) of
			true ->
			    succeed(X, Xs);
			false ->
			    fail(Xs)
		    end
	    end
    end.

literal(A) ->
    satisfy(fun(X)-> X == A end).

alt(P1, P2) ->
    fun(Inp) ->
	    apply(P1, [Inp]) ++ apply(P2, [Inp])
    end.

then(P1, P2) ->
    fun(Inp) ->
	    [ {{V1, V2}, Out2} || {V1, Out1} <- apply(P1, [Inp])
				, {V2, Out2} <- apply(P2, [Out1]) ]
    end.

using(P, F) ->
    fun(Inp) ->
	    [ {apply(F, [V]), Out} || {V, Out} <- apply(P, [Inp]) ]
    end.

many(P) ->
    fun(Inp) ->
	    apply(alt(using(then(P, apply(fun many/1, [P]))
			    , fun({X,Xs}) -> [X|Xs] end)
		      , succeed([]))
		  , [Inp])
    end.

some(P) ->
    fun(Inp) ->
	    apply(using(then(P, apply(fun many/1, [P]))
			, fun({X,Xs}) -> [X|Xs] end)
		  , [Inp])
    end.

number() ->
    fun(Inp) ->
	    apply(some(satisfy(
			 fun(C) ->
				 case C of
				     $0 -> true;
				     $1 -> true;
				     $2 -> true;
				     $3 -> true;
				     $4 -> true;
				     $5 -> true;
				     $6 -> true;
				     $7 -> true;
				     $8 -> true;
				     $9 -> true;
				     _other -> false
				 end
			 end
			))
		  , [Inp])
    end.
