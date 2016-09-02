-module(json).
-compile(export_all).
-include("../include/json.hrl").

json() ->
    parser:alt(fn_util:lazy(fun object/0, []),
	       parser:alt(fn_util:lazy(fun array/0, []),
			  fn_util:lazy(fun value/0, []))).
      

value() ->
    parser:alt(?MODULE:string(),
	       parser:alt(num(),
			  parser:alt(fn_util:lazy(fun object/0, []),
				     parser:alt(fn_util:lazy(fun array/0, []),
						parser:alt(true(),
							   parser:alt(false(),
								      null())))))).

object() ->
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, [""]),
    Id = fun(X) -> X end,
    EncObject = fun(E) -> #object{ elements= E } end,
    parser:using(
      parser:xthen(parser:nibble(parser:literal(${)),
		   parser:using(
		     parser:thenx(
		       parser:alt(Ignore,
				  parser:using(
				    parser:nibble(
				      parser:then(key_value(),
						  parser:nibble(
						    parser:some(then_kv())))),
				    Cons)),
		       parser:nibble(parser:literal($}))
		      ),
		     Id)),
      EncObject).
	       

array() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    Unpack = fun([X]) -> X; (X) -> X end,
    parser:using(
      parser:xthen(parser:nibble(parser:literal($[)),
		   parser:thenx(
		     parser:alt(parser:using(parser:then(value(),
							 parser:some(then_value())),
					     fun cons/1),
				Ignore),
		     parser:nibble(parser:literal($])))),
		   Unpack).

then_value() ->
    parser:xthen(parser:nibble(parser:literal($,)),
		 value()).

key_value() ->
    parser:then(?MODULE:string(),
		parser:xthen(parser:nibble(parser:literal($:)),
			     parser:nibble(value()))).

then_kv() ->
    parser:xthen(parser:literal($,), key_value()).
	      

escape_sequence() ->
    parser:using(parser:then(parser:literal($\\),
			     lists:foldr(fun(Ch, R) -> parser:alt(parser:literal(Ch), R) end,
					 fun parser:fail/1,
					 [$", $\\, $\/, $b, $f, $n, $r, $t])),
		 fun({A, L}) when is_list(L) -> lists:map(fun(X) -> cons({A, X}) end, L);
		    ({A, X}) when not is_list(X) -> cons({A, pack(X)}) end).

escape($") -> escape1($");
escape($\\) -> escape1($\\); 
escape($\/) -> escape1($\/);
escape($b) -> escape1($b);
escape($f) -> escape1($f); 
escape($n) -> escape1($n);
escape($r) -> escape1($r);
escape($t) -> escape1($t).

escape1(C) -> parser:using(parser:then(parser:literal($\\), parser:literal(C)),
			   fun cons/1).

hxdigit() -> lists:foldr(fun(Ch, R) -> parser:alt(parser:literal(Ch), R) end,
			 parser:literal($F),
			 [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
			  $a, $b, $c, $d, $e, $f,
			  $A, $B, $C, $D, $E]).

hxdigits(0) ->            parser:p(fun parser:succeed/2, "");
hxdigits(N) when N > 0 -> parser:using(parser:then(hxdigit(), hxdigits(N-1)),
				       fun({A,B}) -> [A|B] end).

unicode() ->
    parser:using(parser:then(parser:literal($\\),
			     parser:using(parser:then(parser:literal($u),
						      hxdigits(4)),
					  fun cons/1)),
		 fun cons/1).



acceptable_char() ->
    P = fun($")  -> false;
	   ($\\) -> false;
	   (C) when C =< $\x{1F} -> false;
	   (_)   -> true end,
    parser:using(parser:p(fun parser:satisfy/2, P), fun pack/1).



integer() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Ignore = parser:p(fun parser:succeed/2, ""),
    parser:alt(parser:using(parser:then(parser:digit1_9(),
					parser:alt(parser:some(parser:digit()), Ignore)),
			    Cons),
	       parser:using(parser:literal($0), C2Str)).

num() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, ""),

    Sign = parser:nibble(parser:alt(Ignore,
				    parser:using(parser:literal($-), C2Str))),

    Decimal =
	parser:alt(parser:using(parser:then(parser:literal($.),
					    parser:some(parser:digit())),
				Cons),
		   Ignore),
    Exp =
	parser:alt(parser:using(parser:then(parser:alt(parser:literal($e), parser:literal($E)),
					    integer()),
				Cons),
		   Ignore),

    parser:using(parser:then(Sign,
			     parser:using(parser:then(integer(),
						      parser:using(parser:then(Decimal, Exp), Append)),
					  Append)),
		 Append).

true() ->
    parser:nibble(parser:string("true")).

false() ->
    parser:string("false").

null() ->
    parser:string("null").



string() ->
    Quote = parser:literal($"),
    EndQuote = parser:using(Quote, fun pack/1),
    parser:alt(parser:using(parser:then(Quote, EndQuote),
			    fun cons/1),
	       parser:using(parser:then(Quote,
					parser:using(parser:then(chars(),
								 EndQuote),
						     fun append/1)),
			    fun cons/1)).



char() ->
    parser:alt(acceptable_char(),
	       parser:alt(escape_sequence(),
			  unicode())).



chars() ->
    parser:alt(char(),
	       parser:using(parser:then(char(), fn_util:lazy(fun chars/0, [])),
			    fun append/1)).



number() ->
    parser:alt(int(),
	       parser:alt(parser:using(parser:then(int(), frac()), fun append/1),
			  parser:alt(parser:using(parser:then(int(), exp()), fun append/1),
				     parser:using(parser:then(int(),
							      parser:using(parser:then(frac(), exp()), fun append/1)), fun append/1)))).



int() ->
    parser:alt(parser:using(parser:digit(), fun pack/1),
	       parser:alt(parser:using(parser:then(parser:digit1_9(), digits()),
				       fun cons/1),
			  parser:alt(parser:using(parser:then(parser:literal($-),
							      parser:using(parser:digit(), fun pack/1)),
						  fun cons/1),
				     parser:using(parser:then(parser:literal($-),
							      parser:using(parser:then(parser:digit1_9(),
										       digits()),
									   fun cons/1)),
						  fun cons/1)))).



frac() ->
    parser:using(parser:then(parser:literal($.), digits()),
		 fun cons/1).



exp() ->
    parser:using(parser:then(e(), digits()),
		 fun({'e', T})  -> [$e|T];
		    ({'e+', T}) -> [$e,$+|T];
		    ({'e-', T}) -> [$e,$-|T] end).



digits() ->
    parser:alt(parser:using(parser:digit(), fun pack/1),
	       parser:using(
		 parser:then(parser:digit(),
			     fn_util:lazy(fun digits/0, [])),
		 fun cons/1)).



e() ->
    parser:using(parser:then(parser:alt(parser:literal($e), parser:literal($E)),
			     parser:alt(parser:alt(parser:literal($+), parser:literal($-)),
					ignore())),
		 fun pack_e/1).
					  

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



pack(X) -> [X].



pack_e({$E, Ap}) -> pack_e({$e, Ap});
pack_e({Ch, Ap}) -> case Ap of
			[""] -> list_to_atom([Ch]);
			_    -> list_to_atom([Ch, Ap])
		    end.



ignore() ->
    parser:p(fun parser:succeed/2, [""]).
