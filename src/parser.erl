-module(parser).
-compile(export_all).

-import(stdlib).

%% Walking through Graham Hutton's paper: Higher-Order Functions for Parsing.

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

satisfy(_predicate, []) -> [];
satisfy(Predicate, [X|Xs]) ->
    case apply(Predicate, [X]) of
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

%%=================================
number_() ->
    satisfy(fun(C) -> (C >= $0) and (C =< $9) end).

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
%%==================================

xthen(P1, P2) ->
    using(then(P1, P2), fun({_x, Y}) -> Y end).

thenx(P1, P2) ->
    using(then(P1, P2), fun({X, _y}) -> X end).

return(P, V) ->
    using(P, apply(fun(X) -> fun(_y) -> X end end, [V])).

%%===================================
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
	      alt(using(number_(), fun(X) -> X end)
		  , xthen(literal($(), thenx(expn(), literal($))))
		 )
	      , [Inp])
    end.
%%====================================

non_control_char() ->
    fun(Inp) ->
	    apply(
	      satisfy(fun(C) -> ((C =/= $") and (C =/= $\\) and (C > $\x{1F})) end)
	      , [Inp])
    end.

double_quote() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($")), [Inp])
    end.

back_slash() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($\\)), [Inp])
    end.

slash() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($/)), [Inp])
    end.

bksp() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($b)), [Inp])
    end.

ff() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($f)), [Inp])
    end.

nl() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($n)), [Inp])
    end.

cr() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($r)), [Inp])
    end.

tab() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($t)), [Inp])
    end.

unicode() ->
    fun(Inp) ->
	    apply(
	      using(then(literal($\\)
		   , using(then(literal($u)
			  , using(then(hex_digit()
				 , using(then(hex_digit()
					, using(then(hex_digit(), hex_digit()
						    ), fun stdlib:cons/1)
				       ), fun stdlib:cons/1)
				 ), fun stdlib:cons/1)
		       ), fun stdlib:cons/1)
	       ), fun stdlib:cons/1)
	      , [Inp])
    end.

hex_digit() ->
    fun(Inp) ->
	    apply(satisfy(fun(C) -> ((C >= $0) and (C =< $9))
					or ((C >= $a) and (C =< $f))
					or ((C >= $A) and (C =< $F))
			  end)
		  , [Inp])
    end.

%%=====================================
char() ->
    fun(Inp) ->
	    apply(
	      alt(non_control_char()
		  , alt(double_quote()
			, alt(back_slash()
			      , alt(slash()
				    , alt(bksp()
					  , alt(ff()
						, alt(nl()
						      , alt(cr()
							    , alt(tab()
								  , unicode()
								 )))))))))
	      , [Inp])
    end.

chars() ->
    some(char()).

string() ->
    alt(xthen(literal($"), thenx(succeed([]), literal($")))
	, xthen(literal($"), thenx(chars(), literal($")))
       ).

%%=================================
e() ->
    alt(satisfy(fun(C) -> (C == $e) or (C == $E) end)
	, then(satisfy(fun(C) -> (C == $e) or (C == $E) end), satisfy(fun(C) -> (C == $+) or (C == $-) end))).

digit() ->
    satisfy(fun(C) -> (C >= $0) and (C =< $9) end).

digit1_9() ->
    satisfy(fun(C) -> (C >= $1) and (C =< $9) end).

digits() ->
    some(digit()).

exp() ->
    using(then(e(), digits())
	  , fun(Tuple) ->
		    case Tuple of
			{$e, Num} ->
			    [$e|Num];
			{$E, Num} ->
			    [$e|Num];
			{{$e, Sign}, Num} ->
			    case Sign of
				$+ ->
				    [$e|Num];
				$- ->
				    [$e, $-|Num]
			    end;
			{{$E, Sign}, Num} ->
			    case Sign of
				$+ ->
				    [$e|Num];
				$- ->
				    [$e, $-|Num]
			    end
		    end
	    end
	 ).

frac() ->
    using(then(literal($.), digits()), fun stdlib:cons/1).

int() ->
    alt(using(then(digit(), succeed([])), fun stdlib:cons/1)
	, alt(using(then(digit1_9(), digits()), fun stdlib:cons/1)
	      , alt(using(then(literal($-), digit()), fun stdlib:cons/1)
		    , using(then(then(literal($-), digit1_9()), digits()), fun({{$-, D}, Num}) -> [$-, D|Num] end)
		    ))).

number() ->
    alt(int()
	, alt(using(then(int(), frac()), fun stdlib:append/1)
	      , alt(using(then(int(), exp()), fun stdlib:append/1)
		    , using(then(using(then(int(), frac()), fun stdlib:append/1), exp()), fun stdlib:append/1)
		    ))).
