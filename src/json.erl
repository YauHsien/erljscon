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

escape($") -> escape1($");
escape($\\) -> escape1($\\); 
escape($\/) -> escape1($\/);
escape($b) -> escape1($b);
escape($f) -> escape1($f); 
escape($n) -> escape1($n);
escape($r) -> escape1($r);
escape($t) -> escape1($t).

escape1(C) -> parser:then(parser:literal($\\), parser:literal(C)).

