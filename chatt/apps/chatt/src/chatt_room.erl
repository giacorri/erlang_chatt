%%--------------------------------------------------------------------
%% @doc Chat room process that keeps track of connected users
%%      and broadcasts messages to all participants.
%%--------------------------------------------------------------------

-module(chatt_room).

%% We use gen_server for an OTP-compliant process
-behaviour(gen_server).

%% Public API exports
-export([start_link/0, register_user/2, unregister_user/1, handle_command/2, send_message/2]).

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
                    reply(Username, "Room already exists.\n", State),
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
                        OldRoomName -> reply(Username, io_lib:format("Left room <~s>; ", [OldRoomName]), FinalState)
                    end,
                    
                    reply(Username, io_lib:format("Room <~s> created and joined.\n", [RoomName]), FinalState),
                    {noreply, FinalState}
            end;

        ["destroy", RoomName] ->
            case {maps:get(RoomName, State#state.rooms, undefined),
                maps:get(RoomName, State#state.room_creators, undefined)} of
                {undefined, _} ->
                    reply(Username, io_lib:format("Room <~s> does not exist.\n", [RoomName]), State),
                    {noreply, State};
                {_, Creator} when Creator =/= Username ->
                    reply(Username, io_lib:format("Only the creator `~s` can destroy room <~s>.\n", [Creator, RoomName]), State),
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

                    %% Notify affected users (optional)
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

                    reply(Username, io_lib:format("Destroyed room <~s>.\n", [RoomName]), FinalState),
                    {noreply, FinalState}
            end;

        ["join", RoomName] ->
            case maps:get(RoomName, State#state.rooms, undefined) of
                undefined ->
                    reply(Username, io_lib:format("Room <~s> not found.\n", [RoomName]), State),
                    {noreply, State};
                Members ->
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
                        OldRoomName -> reply(Username, io_lib:format("Left room <~s>; ", [OldRoomName]), FinalState)
                    end,

                    reply(Username, io_lib:format("Joined room <~s>.\n", [RoomName]), CleanedState),
                    {noreply, FinalState}
            end;

        ["leave"] ->
            RoomName = maps:get(Username, State#state.user_rooms, undefined),
            case RoomName of
                undefined ->
                    reply(Username, "You are not in a room.\n", State),
                    {noreply, State};
                Room ->
                    RoomMembers = maps:get(Room, State#state.rooms),
                    UpdatedRoom = sets:del_element(Username, RoomMembers),
                    Rooms = State#state.rooms,
                    NewRooms = Rooms#{Room => UpdatedRoom},
                    NewURs = maps:remove(Username, State#state.user_rooms),
                    reply(Username, io_lib:format("Left room <~s>.\n", [RoomName]), State),
                    {noreply, State#state{rooms = NewRooms, user_rooms = NewURs}}
            end;

        ["rooms"] ->
            Names = maps:keys(State#state.rooms),
            RoomStrs = lists:map(fun(R) -> "<" ++ R ++ ">\n" end, Names),
            reply(Username, lists:flatten(RoomStrs), State),
            {noreply, State};

        _ ->
            reply(Username, "Unknown command.\n", State),
            {noreply, State}
    end;

handle_cast({send, Sender, Msg}, State) ->
    case maps:get(Sender, State#state.user_rooms, undefined) of
        undefined ->
            reply(Sender, "You are not in a room.\n", State),
            {noreply, State};
        Room ->
            Members = sets:to_list(maps:get(Room, State#state.rooms)),
            lists:foreach(fun(User) ->
                if
                    User =/= Sender ->
                        case maps:get(User, State#state.users, undefined) of
                            undefined -> ok;
                            Sock -> gen_tcp:send(Sock, io_lib:format("<~s>~s: ~s\n", [Room, Sender, Msg]))
                        end;
                    true ->
                        ok
                end
            end, Members),
            {noreply, State}
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