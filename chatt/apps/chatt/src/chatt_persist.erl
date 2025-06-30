-module(chatt_persist).
-export([
    init/0,
    save_room/3, load_rooms/0,
    save_message/2, load_messages/2,
    save_invite/2, load_user_invites/1
]).

-include_lib("erlcloud/include/erlcloud_ddb.hrl").

%% @doc Initializes the module.
%% Note: The actual configuration for local DynamoDB is now handled in get_dynamo_config/0.
init() ->
    % While these are set, the get_dynamo_config/0 function will override
    % the endpoint, which is crucial for local DynamoDB.
    % These can be kept for consistency if you later switch to real AWS,
    % but for local, they are mostly placeholders.
    % application:set_env(erlcloud, aws_access_key_id, "fakeMyKeyId"),
    % application:set_env(erlcloud, aws_secret_access_key, "fakeSecretAccessKey"),
    % application:set_env(erlcloud, aws_security_token, "your_token_if_needed"), % Can be anything for local
    % application:set_env(erlcloud, aws_region, "us-east-1"), % Region can be arbitrary for local
    ok.

%% @doc Creates and returns a DynamoDB configuration specifically for local DynamoDB.
% get_dynamo_config() ->
%     % Start with a base config (credentials and region are placeholders for local)
%     Config = erlcloud_aws:new_config(
%         application:get_env(erlcloud, aws_access_key_id, "fakeMyKeyId"),
%         application:get_env(erlcloud, aws_secret_access_key, "fakeSecretAccessKey"),
%         application:get_env(erlcloud, aws_region, "us-east-1")
%     ),
%     % Override the endpoint to point to your local DynamoDB container (port 8000 is default)
%     erlcloud_aws:set_option(Config, endpoint, "http://localhost:8000").

%% @doc Saves a chat room to the DynamoDB table "ChattRooms".
save_room(RoomName, IsPrivate, Creator) ->
    Item = [
        {"PK", {s, "room#" ++ RoomName}},
        {"private", {bool, IsPrivate}},
        {"creator", {s, Creator}}
    ],
    erlcloud_ddb:put_item("ChattRooms", Item).

%% @doc Loads all chat rooms from the DynamoDB table "ChattRooms".
load_rooms() ->
    {ok, Resp} = erlcloud_ddb:scan("ChattRooms", []),
    [parse_room(Item) || Item <- Resp#ddb_scan.items].

%% @doc Parses a DynamoDB item into a room record.
parse_room(Item) ->
    {s, <<"room#", Rest/binary>>} = proplists:get_value("PK", Item), % Ensure atom for binary matching
    Name = Rest,
    {bool, Private} = proplists:get_value("private", Item),
    {s, Creator} = proplists:get_value("creator", Item),
    #{name => Name, private => Private, creator => Creator}.

%% @doc Saves a message to the DynamoDB table "ChattMessages".
save_message(Room, MessageBin) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = "msg#" ++ integer_to_list(Now),
    Item = [
        {"PK", {s, "room#" ++ binary_to_list(Room)}},
        {"SK", {s, Key}}, % Ensure SK (Sort Key) is correctly defined for composite key
        {"message", {s, binary_to_list(MessageBin)}}
    ],
    erlcloud_ddb:put_item("ChattMessages", Item).

%% @doc Loads the last N messages for a given room from the "ChattMessages" table.
load_messages(Room, N) ->
    KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "room#" ++ binary_to_list(Room)),
    {ok, Result} = erlcloud_ddb:query("ChattMessages", [
        {key_conditions, KeyCond},
        {scan_index_forward, false},
        {limit, N}
    ]),
    lists:reverse([list_to_binary(proplists:get_value("message", Msg)) || Msg <- Result#ddb_q.items]).

%% @doc Saves an invite for a user to a room in the "ChattInvites" table.
save_invite(User, Room) ->
    Item = [
        {"PK", {s, "invite#" ++ binary_to_list(User)}},
        {"room", {s, binary_to_list(Room)}}
    ],
    erlcloud_ddb:put_item("ChattInvites", Item).

%% @doc Loads all invites for a given user from the "ChattInvites" table.
load_user_invites(User) ->
    KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "invite#" ++ binary_to_list(User)),
    {ok, Result} = erlcloud_ddb:query("ChattInvites", [{key_conditions, KeyCond}]),
    [list_to_binary(proplists:get_value("room", I)) || I <- Result#ddb_q.items].