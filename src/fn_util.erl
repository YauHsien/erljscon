-module(fn_util).
-compile(export_all).
-export([curry/1, lazy/2, apply/2]).
-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).
-include("../include/lazy.hrl").

-spec curry(function()) -> function() | any().
curry(F) ->
    {_M, _F, A} = parts(F),
    curry(F, A, []).

curry(F, A, C) when A == length(C) ->
    erlang:apply(F, lists:reverse(C));
curry(F, A, C) when A > length(C) ->
    fun(X) ->
	    curry(F, A, [X|C])
    end.

-spec lazy(function(), list()) -> #lazy{}.
lazy(F, As) when is_function(F) andalso is_list(As) ->
    #lazy{ function= F, args= As }.

-spec apply(#lazy{} | function(), list()) -> any().
apply(#lazy{ function= F, args= As }, As1) when is_list(As1) ->
    erlang:apply(erlang:apply(F, As), As1);
apply(F, As) when is_function(F) andalso is_list(As) ->
    erlang:apply(F, As).

curryl(F, N) ->
    curryl(F, N, []).
curryl(F, N, Args) ->
    ?DELAY(curry(F, N, Args)).

curryll(F, N) ->
    curryll(F, N, []).
curryll(F, 0, Args) ->
    fun() -> erlang:apply(F, lists:reverse(Args)) end;
curryll(F, N, Args) when N > 0 ->
    fun() -> fun(X) ->
	    curryll(F, N-1, [X|Args])
    end end.

cons({X,Y}) ->
    if
	is_list(Y) ->
	    [X|Y];
	true ->
	    [X,Y]
    end.

append({Xs, Ys}) ->
    erlang:append(Xs, Ys).

parts(F) when is_function(F) ->
    P = erlang:fun_info(F),
    G = fun proplists:get_value/2,
    [M, F1, A] =
	lists:map(fun([G1, As1]) -> erlang:apply(G1, As1) end,
		  [[G, [module, P]],
		   [G, [name, P]],
		   [G, [arity, P]]]),
    {M, F1, A}.

prepend_curry(F, N) ->
    lists:map(fun(N1) -> prepend_curry1(F, N1) end,
	      lists:seq(1, N)).

prepend_curry1(F, N1) ->
    ok.
