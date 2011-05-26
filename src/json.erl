-module(json).
-import(erljscon).

object() ->
    fun(Inp) ->
	    apply(
	      , [Inp])
    end.


