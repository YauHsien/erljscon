-module(json).
-compile(export_all).

parse_value() ->
    parser:alt(
      parser:string()
      , parser:alt(
	  parser:number()
	  , parser:alt(
	      parser:true()
	      , parser:alt(
		  parser:false()
		  , parser:null()
		  )))).

parse_elements() ->
    fun() ->
	      parser:alt(
		parse_value()
		, parser:then(
		    parse_value()
		    , parser:xthen(
			parser:skip()
			, parser:xthen(
			    parser:literal($,)
			    , parser:xthen(
				parser:skip()
				, parse_elements()
			       )))
		   ))
    end.

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
