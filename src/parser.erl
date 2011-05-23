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
			 fun(C) -> (C >= $0) and (C =< $9) end
			))
		  , [Inp])
    end.

word() ->
    fun(Inp) ->
	    apply(some(satisfy(
			 fun(C) -> ((C >= $a) and (C =< $z))
				       or ((C >= $A) and (C =< $Z))
			 end
			 ))
		  , [Inp])
    end.

string(Str) ->
    fun(Inp) ->
	    case Str of
		[] ->
		    apply(succeed([]), [Inp]);
		[X|Xs] ->
		    apply(using(then(literal(X), string(Xs))
				, fun({Y,Ys}) -> [Y|Ys] end
				)
			  , [Inp])
	    end
    end.

xthen(P1, P2) ->
    using(then(P1, P2), fun({_x, Y}) -> Y end).

thenx(P1, P2) ->
    using(then(P1, P2), fun({X, _y}) -> X end).

return(P, V) ->
    using(P, apply(fun(X) -> fun(_y) -> X end end, [V])).

expn() ->
    fun(Inp) ->
	    apply(
	      alt(using(then(term(), xthen(literal($+), term()))
			, fun({X, Y}) -> {X, "+", Y} end)
		  , alt(using(then(term(), xthen(literal($-), term()))
			      , fun({X, Y}) -> {X, "-", Y} end)
			, term())
		 )
	      , [Inp])
    end.

term() ->
    fun(Inp) ->
	    apply(
	      alt(using(then(factor(), xthen(literal($*), factor()))
			, fun({X, Y}) -> {X, "*", Y} end)
		  , alt(using(then(factor(), xthen(literal($/), factor()))
			      , fun({X, Y}) -> {X, "/", Y} end)
			, factor())
		 )
	      , [Inp])
    end.

factor() ->
    fun(Inp) ->
	    apply(
	      alt(using(number(), fun(X) -> X end)
		  , xthen(literal($(), thenx(expn(), literal($))))
		 )
	      , [Inp])
    end.
