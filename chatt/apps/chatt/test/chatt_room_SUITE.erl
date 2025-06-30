-module(chatt_room_SUITE).

-include_lib("chatt_room.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/assert.hrl").
-type socket() :: port().
%% Exported callbacks
-export([
    all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    create_room_test/1,
    join_room_test/1,
    destroy_room_by_creator_test/1,
    destroy_room_by_other_test/1,
    switch_room_test/1,
    private_message_test/1,
    receive_message/2,
    private_room_visibility_test/1
]).

%% === Test Cases List ===
all() ->
    [
        create_room_test,
        join_room_test,
        destroy_room_by_creator_test,
        destroy_room_by_other_test,
        switch_room_test,
        private_message_test,
        private_room_visibility_test
    ].

%% === Setup / Teardown ===
init_per_suite(Config) ->
    application:ensure_all_started(chatt),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    % ok = application:stop(chatt),
    % ok = application:start(chatt),
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% === Test Cases ===

create_room_test(_Config) ->
    gen_server:cast(chatt_room, {command, "alice", "create lobby"}),
    timer:sleep(100), %% give async cast time to apply
    State = sys:get_state(chatt_room),
    Room = maps:get("lobby", State#state.rooms),
    ?assert(sets:is_element("alice", Room)),
    ?assertEqual("lobby", maps:get("alice", State#state.user_rooms)),
    ?assertEqual("alice", maps:get("lobby", State#state.room_creators)).

join_room_test(_Config) ->
    gen_server:cast(chatt_room, {command, "bob", "join lobby"}),
    timer:sleep(100),
    State = sys:get_state(chatt_room),
    Room = maps:get("lobby", State#state.rooms),
    ?assert(sets:is_element("bob", Room)),
    ?assertEqual("lobby", maps:get("bob", State#state.user_rooms)).

destroy_room_by_creator_test(_Config) ->
    gen_server:cast(chatt_room, {command, "alice", "destroy lobby"}),
    timer:sleep(100),
    State = sys:get_state(chatt_room),
    ?assertNot(maps:is_key("lobby", State#state.rooms)),
    ?assertNot(maps:is_key("lobby", State#state.room_creators)).

destroy_room_by_other_test(_Config) ->
    gen_server:cast(chatt_room, {command, "alice", "create temp"}),
    gen_server:cast(chatt_room, {command, "bob", "join temp"}),
    timer:sleep(100),
    gen_server:cast(chatt_room, {command, "bob", "destroy temp"}),
    timer:sleep(100),
    State = sys:get_state(chatt_room),
    %% Should not be destroyed
    ?assert(maps:is_key("temp", State#state.rooms)),
    ?assertEqual("alice", maps:get("temp", State#state.room_creators)).

switch_room_test(_Config) ->
    gen_server:cast(chatt_room, {command, "alice", "create room1"}),
    gen_server:cast(chatt_room, {command, "alice", "create room2"}),
    timer:sleep(100),
    State = sys:get_state(chatt_room),
    ?assertEqual("room2", maps:get("alice", State#state.user_rooms)),
    Room1 = maps:get("room1", State#state.rooms),
    Room2 = maps:get("room2", State#state.rooms),
    ?assertNot(sets:is_element("alice", Room1)),
    ?assert(sets:is_element("alice", Room2)).

private_message_test(_Config) ->
    {ok, Port} = application:get_env(chatt, chatt_port),

    {ok, AliceSock} = login_fake_client("localhost", Port, "alice"),
    {ok, BobSock} = login_fake_client("localhost", Port, "bob"),
    {ok, CharlieSock} = login_fake_client("localhost", Port, "charlie"),

    %% Send a private message
    Message = "Hi Bob, it's private.",
    ok = chatt_room:send_private("alice", "bob", Message),

    %% Receive message on Bob's socket
    %% Here we simulate socket receive
    timer:sleep(100),
    receive_message(BobSock, Message),

    %% Alice should NOT receive the message
    {error, timeout} = gen_tcp:recv(CharlieSock, 0, 100),

    %% Cleanup
    gen_tcp:close(AliceSock),
    gen_tcp:close(BobSock),
    gen_tcp:close(CharlieSock),
    ok.

receive_message(Socket, ExpectedMessage) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            Msg = binary_to_list(Data),
            ?assert(string:str(Msg, ExpectedMessage) > 0);
        Error ->
            io:format("[ERROR] Failed to receive message: ~p~n", [Error]),
            ?assertEqual(ok, Error) %% force fail
    end.

-spec login_fake_client(Host :: string(), Port :: integer(), Username :: string()) -> {ok, socket()} | {error, term()}.
login_fake_client(Host, Port, Username) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {ok, Sock} ->
            % Receive login prompt
            case gen_tcp:recv(Sock, 0, 1000) of
                {ok, Prompt} ->
                    io:format("[TEST] Received prompt: ~s~n", [binary_to_list(Prompt)]),
                    % Send username + newline
                    gen_tcp:send(Sock, Username ++ "\n"),
                    % Receive login confirmation
                    case gen_tcp:recv(Sock, 0, 1000) of
                        {ok, Confirmation} ->
                            io:format("[TEST] Received confirmation: ~s~n", [binary_to_list(Confirmation)]);
                        _ ->
                            ok
                    end,
                    {ok, Sock};
                Error ->
                    {error, {no_login_prompt, Error}}
            end;
        Error -> Error
    end.

private_room_visibility_test(_Config) ->
    {ok, Port} = application:get_env(chatt, chatt_port),

    {ok, AliceSock} = login_fake_client("localhost", Port, "alice"),
    {ok, BobSock} = login_fake_client("localhost", Port, "bob"),
    {ok, CharlieSock} = login_fake_client("localhost", Port, "charlie"),

    %% Alice creates a private room and invites Bob
    gen_tcp:send(AliceSock, "/create_private secret\n"),
    gen_tcp:send(AliceSock, "/invite bob\n"),

    %% Bob lists rooms — should see "secret"
    gen_tcp:send(BobSock, "/rooms\n"),
    timer:sleep(100),
    receive_message(BobSock, "secret"),

    %% Charlie lists rooms — should NOT see "secret"
    gen_tcp:send(CharlieSock, "/rooms\n"),
    {ok, Msg} = gen_tcp:recv(CharlieSock, 0, 200),
    ?assertNot(lists:member("secret", binary_to_list(Msg))),

    %% Bob joins the private room
    gen_tcp:send(BobSock, "/join secret\n"),
    timer:sleep(100),
    State = sys:get_state(chatt_room),
    ?assertEqual("secret", maps:get("bob", State#state.user_rooms)),

    %% Charlie tries to join — should fail
    gen_tcp:send(CharlieSock, "/join secret\n"),
    timer:sleep(100),
    receive_message(CharlieSock, "Room <secret> is private"),

    %% Cleanup
    gen_tcp:close(AliceSock),
    gen_tcp:close(BobSock),
    gen_tcp:close(CharlieSock),
    ok.
