-module(chatt_persist).
-export([
    init/0,
    save_room/3, load_rooms/0,
    save_message/2, load_messages/2,
    save_invite/2, load_user_invites/1
]).

-include_lib("erlcloud/include/erlcloud_ddb.hrl").

get_dynamo_config() ->
    erlcloud_ddb:new("fakeMyKeyId", "fakeSecretAccessKey", "localhost", 8000, "http").

init() ->
    % DdbConfig = erlcloud_ddb:new("fakeMyKeyId", "fakeSecretAccessKey", "localhost", 8000, "http").
    ok.

save_room(RoomName, IsPrivate, Creator) ->
    DdbConfig = get_dynamo_config(),
    Item = [
        {"PK", {s, "room#" ++ RoomName}},
        {"private", {bool, IsPrivate}},
        {"creator", {s, Creator}}
    ],
    erlcloud_ddb:put_item("ChattRooms", Item, DdbConfig).

load_rooms() ->
    {ok, Resp} = erlcloud_ddb:scan("ChattRooms"),
    [parse_room(Item) || Item <- Resp#ddb_scan.items].

parse_room(Item) ->
    <<"room#", Rest/binary>> = proplists:get_value("PK", Item),
    Name = Rest,
    {bool, Private} = proplists:get_value("private", Item),
    {s, Creator} = proplists:get_value("creator", Item),
    #{name => Name, private => Private, creator => Creator}.

save_message(Room, MessageBin) ->
    DdbConfig = get_dynamo_config(),
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = "msg#" ++ integer_to_list(Now),
    Item = [
        {"PK", {s, "room#" ++ binary_to_list(Room)}},
        {"SK", {s, Key}},
        {"message", {s, binary_to_list(MessageBin)}}
    ],
    erlcloud_ddb:put_item("ChattMessages", Item, DdbConfig).

load_messages(Room, N) ->
    KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "room#" ++ binary_to_list(Room)),
    {ok, Result} = erlcloud_ddb:query("ChattMessages", [{key_conditions, KeyCond}, {scan_index_forward, false}, {limit, N}]),
    lists:reverse([list_to_binary(proplists:get_value("message", Msg)) || Msg <- Result#ddb_q.items]).

save_invite(User, Room) ->
    DdbConfig = get_dynamo_config(),
    Item = [
        {"PK", {s, "invite#" ++ binary_to_list(User)}},
        {"room", {s, binary_to_list(Room)}}
    ],
    erlcloud_ddb:put_item("ChattInvites", Item, DdbConfig).

load_user_invites(User) ->
    KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "invite#" ++ binary_to_list(User)),
    {ok, Result} = erlcloud_ddb:query("ChattInvites", [{key_conditions, KeyCond}]),
    [list_to_binary(proplists:get_value("room", I)) || I <- Result#ddb_q.items].
