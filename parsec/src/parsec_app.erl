%%%-------------------------------------------------------------------
%% @doc parsec public API
%% @end
%%%-------------------------------------------------------------------

-module(parsec_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    parsec_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
