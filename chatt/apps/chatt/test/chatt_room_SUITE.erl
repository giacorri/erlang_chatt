-module(chatt_room_SUITE).

-include_lib("chatt_room.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/assert.hrl").

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
    switch_room_test/1
]).

%% === Test Cases List ===
all() ->
    [
        create_room_test,
        join_room_test,
        destroy_room_by_creator_test,
        destroy_room_by_other_test,
        switch_room_test
    ].

%% === Setup / Teardown ===
init_per_suite(Config) ->
    application:ensure_all_started(chatt),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    %% Reset state here if needed
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
