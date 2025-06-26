%%%-------------------------------------------------------------------
%% @doc chatt public API
%% @end
%%%-------------------------------------------------------------------

-module(chatt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chatt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
