-module(chatt_persist).
-export([
    init/0,
    save_room/3,
    load_rooms/0,
    save_message/2,
    % load_messages/2,
    % load_user_invites/1
    save_invite/2
]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ddb.hrl").

%% @doc Initializes the module.
init() ->
    Config = #aws_config{
        access_key_id = "fakeMyKeyId",
        secret_access_key = "fakeSecretAccessKey",
        aws_region = "us-east-1",
        ddb_scheme = "http://",
        % ddb_host = "localhost",
        ddb_host = "dynamodb_local",  % <-- dynamodb container name
        ddb_port = 8000
    },
    erlcloud_aws:configure(Config).

%% @doc Saves a chat room to the DynamoDB table "ChattRooms".
save_room(RoomName, IsPrivate, Creator) ->
    % RoomName and Creator are likely Erlang strings (lists of chars).
    % DynamoDB strings are binaries.
    RoomNameBinary = list_to_binary(RoomName),
    CreatorBinary = list_to_binary(Creator),

    Item = [
        % Primary Key: Always define its type correctly.
        % Assumed "PK" is a String in DynamoDB, so <<"room#", RoomNameBinary/binary>> is correct
        {<<"PK">>, <<"room#", RoomNameBinary/binary>>},

        % For 'private':
        % Option A: Try passing raw boolean (as per erlcloud's apparent design)
        % This is what caused your error.
        {<<"private">>, IsPrivate},

        % Option B: If A fails (which it did), force it to a binary string.
        % This is the most reliable workaround if DynamoDB or erlcloud's
        % internal handling of boolean is problematic for your setup.
        % {<<"private">>, atom_to_binary(IsPrivate, utf8)}, % Converts 'true' -> <<"true">>, 'false' -> <<"false">>

        {<<"creator">>, CreatorBinary}
    ],
    TableName = <<"ChattRooms">>,

    case erlcloud_ddb:put_item(TableName, Item) of
        {ok, _Result} ->
            io:format("Successfully added room ~s to DynamoDB~n", [binary_to_list(RoomNameBinary)]);
        {error, Reason} ->
            io:format("Error adding room ~s to DynamoDB: ~p~n", [binary_to_list(RoomNameBinary), Reason])
    end.

%% @doc Loads all chat rooms from the DynamoDB table "ChattRooms".
load_rooms() ->
    case erlcloud_ddb:scan("ChattRooms", []) of
        {ok, Resp} ->
            [parse_room(Item) || Item <- Resp#ddb_scan.items];
        {error, Reason} ->
            error_logger:error_msg("Failed to scan ChattRooms: ~p~n", [Reason]),
            []
    end.

parse_room(Item) ->
    {s, <<"room#", Rest/binary>>} = proplists:get_value("PK", Item),
    {bool, Private} = proplists:get_value("private", Item),
    {s, Creator} = proplists:get_value("creator", Item),
    #{name => Rest, private => Private, creator => Creator}.

%% @doc Saves a message to the DynamoDB table "ChattMessages".
save_message(Room, MessageBin) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = "msg#" ++ integer_to_list(Now),
    Item = [
        {"PK", {s, "room#" ++ binary_to_list(Room)}},
        {"SK", {s, Key}},
        {"message", {s, binary_to_list(MessageBin)}}
    ],
    erlcloud_ddb:put_item("ChattMessages", Item).

% %% @doc Loads the last N messages for a given room from the "ChattMessages" table.
% load_messages(Room, N) ->
%     KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "room#" ++ binary_to_list(Room)),
%     {ok, Result} = erlcloud_ddb:query("ChattMessages", [
%         {key_conditions, KeyCond},
%         {scan_index_forward, false},
%         {limit, N}
%     ]),
%     lists:reverse([list_to_binary(proplists:get_value("message", Msg)) || Msg <- Result#ddb_q.items]).

%% @doc Saves an invite for a user to a room in the "ChattInvites" table.
save_invite(User, Room) ->
    Item = [
        {"PK", {s, "invite#" ++ binary_to_list(User)}},
        {"room", {s, binary_to_list(Room)}}
    ],
    erlcloud_ddb:put_item("ChattInvites", Item).

%% @doc Loads all invites for a given user from the "ChattInvites" table.
% load_user_invites(User) ->
%     KeyCond = erlcloud_ddb:make_key_condition("PK", equals, "invite#" ++ binary_to_list(User)),
%     {ok, Result} = erlcloud_ddb:query("ChattInvites", [{key_conditions, KeyCond}]),
%     [list_to_binary(proplists:get_value("room", I)) || I <- Result#ddb_q.items].
