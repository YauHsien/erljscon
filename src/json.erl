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




