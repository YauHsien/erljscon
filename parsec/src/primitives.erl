-module(primitives).
-include("include/types.hrl").
-export(
  [ curry/1,
    epsilon/0,
    fail/0,
    fail/1,
    satisfy/1,
    satisfy/2,
    succeed/1,
    succeed/2
  ]).

curry(Func) ->
  case erlang:fun_info(Func,arity) of
      {arity, 0} ->
          Func();
      {arity, 2} ->
          fun(A1) -> fun(A2) -> Func(A1, A2) end end
  end.

epsilon() ->
    "".

-spec fail() -> parser(from(),to()).
fail() ->
    fun(_inp) -> [] end.

-spec fail(Inp::from()) -> [].
fail(Inp) ->
    (fail())(Inp).

-spec satisfy(Predicate::fun((any())->bool())) -> parser(from(),to()).
satisfy(Predicate) ->
    fun([]) ->
            fail([]);
       ([X|Xs]) ->
            case Predicate(X) of
                true ->
                    succeed(X, Xs);
                false ->
                    fail(Xs)
            end
    end.

-spec satisfy(Predicate::fun((any())->bool()), Inp::from()) -> [{to(), Rest::from()}] | [].
satisfy(Predicate, Inp) ->
    (satisfy(Predicate))(Inp).

-spec succeed(Val::to()) -> parser(from(),to()).
succeed(Val) ->
    fun(Inp) -> [{Val,Inp}] end.

-spec succeed(Val::to(), Inp::from()) -> [{to(), Rest::from()}].
succeed(Val, Inp) ->
    (succeed(Val))(Inp).
