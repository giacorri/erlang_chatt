-module(chatt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    all/0,
    connects_and_receives_prompt/1,
    % broadcasts_between_clients/1,
    connect_and_login/1
]).

%% Test suite metadata
suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    application:ensure_all_started(chatt),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

% all() -> [connects_and_receives_prompt, broadcasts_between_clients].
all() -> [connects_and_receives_prompt].


%% Test 1: Connection and username prompt
connects_and_receives_prompt(_Config) ->
    {ok, Socket} = gen_tcp:connect("localhost", 1234, [
        binary, {packet, line}, {active, false}
    ]),
    {ok, Prompt} = gen_tcp:recv(Socket, 0),
    ct:pal("Received prompt: ~p", [Prompt]),
    ok = gen_tcp:close(Socket),
    ?assertMatch(<<"Enter your username:\n">>, Prompt).

% %% Test 2: Simulate 2 clients and message broadcast
% broadcasts_between_clients(_Config) ->
%     %% Connect two clients
%     {ok, Sock1} = connect_and_login("Alice"),
%     {ok, Sock2} = connect_and_login("Bob"),

%     %% Alice sends a message
%     gen_tcp:send(Sock1, <<"Hello, Bob!\n">>),

%     %% Bob should receive it
%     {ok, Msg} = gen_tcp:recv(Sock2, 0, 2000),
%     ct:pal("Bob received: ~p", [Msg]),

%     %% Assert message contains "Alice says: Hello"
%     ?assert(lists:prefix("Alice says:", binary_to_list(Msg))),

%     %% Cleanup
%     gen_tcp:close(Sock1),
%     gen_tcp:close(Sock2).

%% Helper to connect and login
connect_and_login(Username) ->
    {ok, Sock} = gen_tcp:connect("localhost", 1234, [
        binary, {packet, line}, {active, false}
    ]),
    %% Wait for username prompt
    {ok, <<"Enter your username:\n">>} = gen_tcp:recv(Sock, 0),
    %% Send the username
    gen_tcp:send(Sock, list_to_binary(Username ++ "\n")),
    %% Receive welcome message
    {ok, _Welcome} = gen_tcp:recv(Sock, 0),
    {ok, Sock}.