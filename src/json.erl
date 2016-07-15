-module(json).
-compile(export_all).

value() ->
    parser:alt(
      ?MODULE:string(),
      parser:alt(
	num(),
	parser:alt(
	  fn_util:lazy(fun object/0, []),
	  parser:alt(
	    fn_util:lazy(fun array/0, []),
	    parser:alt(
	      true(),
	      parser:alt(false(), null())))))).

object() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    parser:using(
      parser:then(
	parser:literal(${),
	parser:using(
	  parser:then(
	    parser:alt(
	      parser:using(
		parser:then(
		  key_value(),
		  parser:alt(parser:many(then_kv()), Ignore)
		 ),
		fun append/1
	       ),
	      Ignore
	     ),
	    parser:literal($})
	   ),
	  fun({A, B}) -> lists:append(A, [B]) end
	 )
       ),
      fun cons/1
     ).

array() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    parser:using(
      parser:then(
	parser:literal($[),
	parser:using(
	  parser:then(
	    parser:alt(
	      parser:using(
		parser:then(
		  value(),
		  parser:alt(then_value(), Ignore)
		 ),
		fun append/1
	       ),
	      Ignore
	     ),
	    parser:literal($])
	   ),
	  fun({A, B}) -> lists:append(A, [B]) end
	 )
       ),
      fun cons/1
     ).

then_value() ->
    parser:using(
      parser:then(parser:literal($,), value()),
      fun cons/1
     ).

key_value() ->
    parser:using(
      parser:then(
	?MODULE:string(),
	parser:using(
	  parser:then(
	    parser:literal($:),
	    value()
	   ),
	  fun cons/1
	 )
       ),
      fun append/1
     ).

then_kv() ->
    parser:alt(
      parser:using(
	parser:then(
	  parser:using(
	    parser:then(parser:literal($,), key_value()),
	    fun cons/1
	   ),
	  fn_util:lazy(fun then_kv/0, [])
	 ),
	fun append/1
       ),
      parser:p(fun parser:succeed/2, [""])
     ).

escape_sequence() ->
    lists:foldr(fun(Ch, R) -> parser:alt(escape(Ch), R) end,
		unicode(),
		[$", $\\, $\/, $b, $f, $n, $r, $t]).
			 

escape($") -> escape1($");
escape($\\) -> escape1($\\); 
escape($\/) -> escape1($\/);
escape($b) -> escape1($b);
escape($f) -> escape1($f); 
escape($n) -> escape1($n);
escape($r) -> escape1($r);
escape($t) -> escape1($t).

escape1(C) ->
    parser:using(
      parser:then(parser:literal($\\), parser:literal(C)),
      fun({A, B}) -> [A, B] end
     ).

hxdigit() ->
    lists:foldr(fun(Ch, R) -> parser:alt(parser:literal(Ch), R) end,
		parser:literal($F),
		[$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
		 $a, $b, $c, $d, $e, $f,
		 $A, $B, $C, $D, $E]).

hxdigits(0) ->
    parser:p(fun parser:succeed/2, "");
hxdigits(N) when N > 0 ->
    parser:using(parser:then(hxdigit(), hxdigits(N-1)),
		 fun({A,B}) -> [A|B] end).

unicode() ->
    parser:using(
      parser:then(parser:literal($\\),
		  parser:using(
		    parser:then(parser:alt(parser:literal($u),
					   parser:literal($U)),
				hxdigits(4)),
		    fun({A,B}) -> [A|B] end)
		 ),
      fun({A,B}) -> [A|B] end
     ).

string() ->
    Append = fun({A, B}) -> lists:append(A, B) end,
    Quote = parser:using(
	      parser:literal($"),
	      fun(Ch) -> [Ch] end
	     ),
    parser:using(
      parser:then(
	Quote,
	parser:using(
	  parser:then(
	    parser:many(
	      parser:alt(parser:acceptable_char(), escape_sequence())
	     ),
	    Quote
	   ),
	  Append
	 )
       ),
      Append
     ).

num() ->
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, ""),
    Sign =
	parser:alt(
	  Ignore,
	  parser:using(parser:literal($-), fun(C) -> [C] end)
	 ),
    Integral =
	parser:alt(
	  parser:literal($0),
	  parser:using(
	    parser:then(
	      parser:using(parser:digit1_9(), fun(C) -> [C] end),
	      parser:many(parser:digit())
	     ),
	    Cons
	   )
	 ),
    Decimal =
	parser:alt(
	  Ignore,
	  parser:using(
	    parser:then(
	      parser:literal($.),
	      parser:some(parser:digit())
	     ),
	    Cons
	   )
	 ),
    Exp =
	parser:alt(
	  Ignore,
	  parser:using(
	    parser:then(
	      parser:alt(
		parser:literal($e),
		parser:literal($E)
	       ),
	      parser:using(
		parser:then(
		  parser:alt(
		    parser:using(parser:literal($+), fun(C) -> [C] end),
		    parser:alt(
		      Ignore,
		      parser:using(parser:literal($-), fun(C) -> [C] end)
		     )
		   ),
		  parser:some(parser:digit())
		 ),
		Append
	       )
	     ),
	    Cons
	   )
	 ),
    parser:using(
      parser:then(
	Sign,
	parser:using(
	  parser:then(
	    Integral,
	    parser:using(
	      parser:then(
		Decimal,
		Exp
	       ),
	      Append
	     )
	   ),
	  Cons
	 )
       ),
      Append
     ).

true() ->
    parser:nibble(parser:string("true")).

false() ->
    parser:nibble(parser:string("false")).

null() ->
    parser:nibble(parser:string("null")).


%% -------------------------------------
%% Internal
%% -------------------------------------

foldr(_F, Z, []) ->
    Z;
foldr(F, Z, [H|T]) ->
    F(H, foldr(F, Z, T)).

cons({A, B}) ->
    [A|B].

append({A, B}) ->
    lists:append(A, B).
