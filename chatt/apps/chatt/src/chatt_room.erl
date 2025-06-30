%%--------------------------------------------------------------------
%% @doc Chat room process that keeps track of connected users
%%      and broadcasts messages to all participants.
%%--------------------------------------------------------------------

-module(chatt_room).

%% We use gen_server for an OTP-compliant process
-behaviour(gen_server).

%% Public API exports
-export([start_link/0, register_user/2, unregister_user/1, handle_command/2, send_message/2, send_private/3]).

%% gen_server callback exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("chatt_room.hrl").

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% Starts the chat room process and registers it locally
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Username, Socket) ->
    gen_server:cast(?MODULE, {register_user, Username, Socket}).

unregister_user(Username) ->
    gen_server:cast(?MODULE, {unregister_user, Username}).

handle_command(User, CommandLine) ->
    gen_server:cast(?MODULE, {command, User, CommandLine}).

send_message(User, Msg) ->
    gen_server:cast(?MODULE, {send, User, Msg}).

send_private(Sender, Receiver, Message) ->
    gen_server:cast(?MODULE, {private_message, Sender, Receiver, Message}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% Called when the chat room process starts
init([]) ->
    %% State is a map of Username => Socket
    {ok, #state{}}.

%% Handle a user joining: add their socket to the user map
handle_cast({register_user, Username, Socket}, State) ->
    Users = State#state.users,
    NewUsers = Users#{Username => Socket},
    {noreply, State#state{users = NewUsers}};

handle_cast({unregister_user, Username}, State) ->
    RoomName = maps:get(Username, State#state.user_rooms, undefined),
    State1 = case RoomName of
        undefined -> State;
        Room ->
            RoomMembers = maps:get(Room, State#state.rooms),
            UpdatedRoom = sets:del_element(Username, RoomMembers),
            Rooms = State#state.rooms,
            UpdatedRooms = Rooms#{Room => UpdatedRoom},
            UpdatedURs = maps:remove(Username, State#state.user_rooms),
            State#state{rooms = UpdatedRooms, user_rooms = UpdatedURs}
    end,
    NewUsers = maps:remove(Username, State1#state.users),
    {noreply, State1#state{users = NewUsers}};

handle_cast({command, Username, CommandLine}, State) ->
    Words = string:tokens(CommandLine, " "),
    case Words of
        ["create", RoomName] ->
            case maps:is_key(RoomName, State#state.rooms) of
                true ->
                    reply(Username, ansi:red("Room already exists.\n"), State),
                    {noreply, State};
                false ->
                    OldRoomOpt = maps:get(Username, State#state.user_rooms, undefined),
                    CleanedState = remove_user_from_previous_room(Username, State),

                    Rooms = CleanedState#state.rooms,
                    NewRooms = Rooms#{RoomName => sets:add_element(Username, sets:new())},
                    UserRooms = CleanedState#state.user_rooms,
                    NewUserRooms = UserRooms#{Username => RoomName},
                    RoomCreators = CleanedState#state.room_creators,
                    NewCreators = RoomCreators#{RoomName => Username},

                    FinalState = CleanedState#state{
                        rooms = NewRooms,
                        user_rooms = NewUserRooms,
                        room_creators = NewCreators
                    },

                    case OldRoomOpt of
                        undefined -> ok;
                        OldRoomName -> reply(Username, ansi:green(io_lib:format("Left room <~s>; ", [OldRoomName])), FinalState)
                    end,
                    
                    reply(Username, ansi:green(io_lib:format("Room <~s> created and joined.\n", [RoomName])), FinalState),
                    {noreply, FinalState}
            end;

        ["create_private", RoomName] ->
            case maps:is_key(RoomName, State#state.rooms) of
                true ->
                    reply(Username, ansi:red("Room already exists.\n"), State),
                    {noreply, State};
                false ->
                    OldRoomOpt = maps:get(Username, State#state.user_rooms, undefined),
                    CleanedState = remove_user_from_previous_room(Username, State),

                    Rooms = CleanedState#state.rooms,
                    NewRooms = Rooms#{RoomName => sets:add_element(Username, sets:new())},
                    UserRooms = CleanedState#state.user_rooms,
                    NewUserRooms = UserRooms#{Username => RoomName},
                    RoomCreators = CleanedState#state.room_creators,
                    NewCreators = RoomCreators#{RoomName => Username},
                    NewPrivates = sets:add_element(RoomName, CleanedState#state.private_rooms),
                    RoomInvitations = CleanedState#state.room_invitations,
                    NewInvites = RoomInvitations#{RoomName => sets:new()},

                    FinalState = CleanedState#state{
                        rooms = NewRooms,
                        user_rooms = NewUserRooms,
                        room_creators = NewCreators,
                        private_rooms = NewPrivates,
                        room_invitations = NewInvites
                    },

                    case OldRoomOpt of
                        undefined -> ok;
                        OldRoomName -> reply(Username, ansi:green(io_lib:format("Left room <~s>; ", [OldRoomName])), FinalState)
                    end,

                    reply(Username, ansi:green(io_lib:format("Private room <~s> created and joined.\n", [RoomName])), FinalState),
                    {noreply, FinalState}
            end;

        ["invite", TargetUser] ->
            case maps:get(Username, State#state.user_rooms, undefined) of
                undefined ->
                    reply(Username, "You're not in a room.\n", State),
                    {noreply, State};
                RoomName ->
                    case sets:is_element(RoomName, State#state.private_rooms) of
                        false ->
                            reply(Username, ansi:red(io_lib:format("Room <~s> is not private.\n", [RoomName])), State),
                            {noreply, State};
                        true ->
                            case maps:get(RoomName, State#state.room_creators, undefined) of
                                Username ->
                                    %% Prevent inviting oneself
                                    case TargetUser =:= Username of
                                        true ->
                                            reply(Username, ansi:red("You cannot invite yourself.\n"), State),
                                            {noreply, State};
                                        false ->
                                            OldInvited = maps:get(RoomName, State#state.room_invitations, sets:new()),
                                            case sets:is_element(TargetUser, OldInvited) of
                                                true ->
                                                    reply(Username, ansi:red(io_lib:format("User `~s` is already invited to <~s>.\n", [TargetUser, RoomName])), State),
                                                    {noreply, State};
                                                false ->
                                                    NewInvited = sets:add_element(TargetUser, OldInvited),
                                                    RoomInvitations = State#state.room_invitations,
                                                    NewInvites = RoomInvitations#{RoomName => NewInvited},
                                                    NewState = State#state{room_invitations = NewInvites},
                                                    reply(Username, ansi:blue(io_lib:format("Invited `~s` to private room <~s>.\n", [TargetUser, RoomName])), NewState),
                                                    reply(TargetUser, ansi:blue(io_lib:format("You have been invited to join private room <~s> by `~s`.\n", [RoomName, Username])), NewState),
                                                    {noreply, NewState}
                                            end
                                    end;
                                _ ->
                                    reply(Username, ansi:red("Only the room creator can invite.\n"), State),
                                    {noreply, State}
                            end
                    end
            end;

        ["destroy", RoomName] ->
            case {maps:get(RoomName, State#state.rooms, undefined),
                maps:get(RoomName, State#state.room_creators, undefined)} of
                {undefined, _} ->
                    reply(Username, ansi:red(io_lib:format("Room <~s> does not exist.\n", [RoomName])), State),
                    {noreply, State};
                {_, Creator} when Creator =/= Username ->
                    reply(Username, ansi:red(io_lib:format("Only the creator `~s` can destroy room <~s>.\n", [Creator, RoomName])), State),
                    {noreply, State};
                {Members, Username} ->
                    %% Remove room
                    NewRooms = maps:remove(RoomName, State#state.rooms),
                    NewCreators = maps:remove(RoomName, State#state.room_creators),

                    %% Remove all users from user_rooms who were in this room
                    UpdatedUserRooms = maps:filter(
                        fun(_, Room) -> Room =/= RoomName end,
                        State#state.user_rooms
                    ),

                    %% Notify affected users
                    lists:foreach(
                        fun(User) ->
                            case maps:get(User, State#state.users, undefined) of
                                undefined -> ok;
                                Sock -> gen_tcp:send(Sock, lists:flatten(io_lib:format("Room <~s> was destroyed by creator `~s`.\n", [RoomName, Username])))
                            end
                        end,
                        sets:to_list(Members)
                    ),

                    FinalState = State#state{
                        rooms = NewRooms,
                        user_rooms = UpdatedUserRooms,
                        room_creators = NewCreators
                    },

                    reply(Username, ansi:green(io_lib:format("Destroyed room <~s>.\n", [RoomName])), FinalState),
                    {noreply, FinalState}
            end;

        ["join", RoomName] ->
            case maps:get(RoomName, State#state.rooms, undefined) of
                undefined ->
                    reply(Username, ansi:red(io_lib:format("Room <~s> not found.\n", [RoomName])), State),
                    {noreply, State};
                Members ->
                    IsPrivate = sets:is_element(RoomName, State#state.private_rooms),
                    Creator = maps:get(RoomName, State#state.room_creators, undefined),
                    InvitedUsers = maps:get(RoomName, State#state.room_invitations, sets:new()),
                    CanJoin = not IsPrivate orelse Username =:= Creator orelse sets:is_element(Username, InvitedUsers),

                    case CanJoin of
                        false ->
                            reply(Username, ansi:red(io_lib:format("Room <~s> is private. You are not invited.\n", [RoomName])), State),
                            {noreply, State};
                        true ->
                            OldRoomOpt = maps:get(Username, State#state.user_rooms, undefined),
                            CleanedState = remove_user_from_previous_room(Username, State),

                            NewMembers = sets:add_element(Username, Members),
                            Rooms = CleanedState#state.rooms,
                            NewRooms = Rooms#{RoomName => NewMembers},
                            UserRooms = CleanedState#state.user_rooms,
                            NewUserRooms = UserRooms#{Username => RoomName},

                            FinalState = CleanedState#state{
                                rooms = NewRooms,
                                user_rooms = NewUserRooms
                            },

                            case OldRoomOpt of
                                undefined -> ok;
                                OldRoomName -> reply(Username, ansi:green(io_lib:format("Left room <~s>; ", [OldRoomName])), FinalState)
                            end,

                            reply(Username, ansi:green(io_lib:format("Joined room <~s>.\n", [RoomName])), FinalState),
                            {noreply, FinalState}
                    end
            end;

        ["leave"] ->
            RoomName = maps:get(Username, State#state.user_rooms, undefined),
            case RoomName of
                undefined ->
                    reply(Username, ansi:red("You are not in a room.\n"), State),
                    {noreply, State};
                Room ->
                    RoomMembers = maps:get(Room, State#state.rooms),
                    UpdatedRoom = sets:del_element(Username, RoomMembers),
                    Rooms = State#state.rooms,
                    NewRooms = Rooms#{Room => UpdatedRoom},
                    NewURs = maps:remove(Username, State#state.user_rooms),
                    reply(Username, ansi:green(io_lib:format("Left room <~s>.\n", [RoomName])), State),
                    {noreply, State#state{rooms = NewRooms, user_rooms = NewURs}}
            end;

        ["rooms"] ->
            PrivateRoomMap = State#state.private_rooms,
            PrivateRoomNames = maps:keys(PrivateRoomMap),
            RoomCreators = State#state.room_creators,

            %% Filter public rooms: rooms that are NOT in private_rooms
            AllRoomNames = maps:keys(State#state.rooms),
            PublicRoomNames = [R || R <- AllRoomNames, not lists:member(R, PrivateRoomNames)],

            %% Private rooms visible to this user: either they are the creator or they were invited
            CreatorVisible =
                [R || R <- PrivateRoomNames,
                    maps:get(R, RoomCreators, undefined) =:= Username],

            InvitedVisible =
            [R || R <- PrivateRoomNames,
                case maps:get(R, State#state.room_invitations, undefined) of
                    undefined -> false;
                    Set ->
                        case is_map(Set) orelse is_list(Set) of
                            true -> sets:is_element(Username, Set);
                            false -> false
                        end
                end],


            VisiblePrivateRooms = lists:usort(CreatorVisible ++ InvitedVisible),

            %% Format: green for public, blue for private
            RoomStrsPublic = [ansi:green("<" ++ R ++ ">\n") || R <- PublicRoomNames],
            RoomStrsPrivate = [ansi:blue("<" ++ R ++ ">\n") || R <- VisiblePrivateRooms],

            AllRoomStrs = lists:flatten(RoomStrsPublic ++ RoomStrsPrivate),
            reply(Username, AllRoomStrs, State),
            {noreply, State};

        _ ->
            reply(Username, ansi:red("Unknown command, available commands are:\n- /rooms\n- /create\n- /create_private\n- /invite\n- /join\n- /leave\n- /destroy\n- @username\n"), State),
            {noreply, State}
    end;

handle_cast({send, Sender, Msg}, State) ->
    case maps:get(Sender, State#state.user_rooms, undefined) of
        undefined ->
            reply(Sender, ansi:red("You are not in a room.\n"), State),
            {noreply, State};
        Room ->
            Members = sets:to_list(maps:get(Room, State#state.rooms)),
            RoomTag = ansi:green(io_lib:format("<~s>", [Room])),
            Message = io_lib:format("~s ~s: ~s\n", [RoomTag, Sender, Msg]),

            lists:foreach(fun(User) ->
                case maps:get(User, State#state.users, undefined) of
                    undefined -> ok;
                    Sock -> gen_tcp:send(Sock, Message)
                end
            end, Members),

            {noreply, State}
    end;

handle_cast({private_message, Sender, Receiver, Message}, State) ->
    Users = State#state.users,

    %% Prevent sending messages to oneself
    case Sender =:= Receiver of
        true ->
            case maps:find(Sender, Users) of
                {ok, SenderSock} ->
                    gen_tcp:send(SenderSock, ansi:red("Use your mind to speak to yourself.\n"));
                error ->
                    ok
            end,
            {noreply, State};
        false ->
            case maps:find(Receiver, Users) of
                {ok, ReceiverSock} ->
                    Formatted = ansi:blue(io_lib:format("[PRIVATE] ~s: ~s~n", [Sender, Message])),
                    gen_tcp:send(ReceiverSock, lists:flatten(Formatted)),

                    %% Notify sender too
                    case maps:find(Sender, Users) of
                        {ok, SenderSock} ->
                            Confirmation = ansi:blue(io_lib:format("[to ~s] ~s~n", [Receiver, Message])),
                            gen_tcp:send(SenderSock, lists:flatten(Confirmation));
                        error ->
                            ok
                    end,
                    {noreply, State};
                error ->
                    %% Receiver not found — inform sender
                    case maps:find(Sender, Users) of
                        {ok, SenderSock} ->
                            Error_string = ansi:red(io_lib:format("User `~s` not found or offline.\n", [Receiver])),
                            gen_tcp:send(SenderSock, Error_string);
                        error ->
                            ok
                    end,
                    {noreply, State}
            end
    end;

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_, _, State) -> {reply, ok, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

reply(User, Msg, State) ->
    case maps:get(User, State#state.users, undefined) of
        undefined -> ok;
        Sock -> gen_tcp:send(Sock, list_to_binary(Msg))
    end.

remove_user_from_previous_room(Username, State) ->
    case maps:get(Username, State#state.user_rooms, undefined) of
        undefined ->
            State;
        OldRoom ->
            OldMembers = maps:get(OldRoom, State#state.rooms),
            CleanedMembers = sets:del_element(Username, OldMembers),
            Rooms = State#state.rooms,
            CleanedRooms = Rooms#{OldRoom => CleanedMembers},
            State#state{rooms = CleanedRooms}
    end.