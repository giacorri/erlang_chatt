-module(ansi).
-export([red/1, green/1, blue/1]).

%% ANSI color codes
-define(RED, "\e[31m").
-define(GREEN, "\e[32m").
-define(BLUE, "\e[34m").
-define(RESET, "\e[0m").

red(Text) ->
    color(?RED, Text).

green(Text) ->
    color(?GREEN, Text).

blue(Text) ->
    color(?BLUE, Text).

color(Code, Text) ->
    lists:flatten(io_lib:format("~s~s~s", [Code, Text, ?RESET])).
