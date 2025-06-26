% Define the module
-module(chatt_server).

% Export the public functions
-export([start_link/0, start/0, accept_loop/1, handle_client/1]).

% Define a macro for the TCP port
-define(PORT, 1234).

%%--------------------------------------------------------------------
%% start_link/0 - OTP-style start point for use under a supervisor
%%--------------------------------------------------------------------
start_link() ->
    % Spawn a linked process that runs start/0
    Pid = spawn_link(?MODULE, start, []),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% start/0 - Starts the TCP server
%%--------------------------------------------------------------------
start() ->
    % Set up a TCP listening socket
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [
        binary,              % Receive data as binary
        {packet, line},      % Line-based parsing
        {active, false},     % Passive mode - we call gen_tcp:recv manually
        {reuseaddr, true}    % Allows quick restart on the same port
    ]),
    chatt_room:start_link(),
    io:format("Server listening on port ~p~n", [?PORT]),
    accept_loop(ListenSocket).  % Begin accepting clients

%%--------------------------------------------------------------------
%% accept_loop/1 - Accepts clients in a loop
%%--------------------------------------------------------------------
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),  % Wait for a connection
    spawn(?MODULE, handle_client, [Socket]),      % Spawn new process for each client
    accept_loop(ListenSocket).                    % Keep accepting others

%%--------------------------------------------------------------------
%% handle_client/1 - Handles a single client
%%--------------------------------------------------------------------
handle_client(Socket) ->
    gen_tcp:send(Socket, "Enter your username:\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, UsernameBin} ->
            Username = string:trim(binary_to_list(UsernameBin)),
            chatt_room:register_user(Username, Socket),
            loop(Socket, Username);
        {error, closed} ->
            io:format("Client disconnected before sending username~n")
    end.

%%--------------------------------------------------------------------
%% loop/2 - Message loop for a connected client
%%--------------------------------------------------------------------

loop(Socket, Username) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Line = string:trim(binary_to_list(Data)),
            case Line of
                [$/ | Command] ->
                    chatt_room:handle_command(Username, Command),
                    loop(Socket, Username);
                _ ->
                    chatt_room:send_message(Username, Line),
                    loop(Socket, Username)
            end;
        {error, closed} ->
            chatt_room:unregister_user(Username),
            io:format("~s disconnected~n", [Username])
    end.


% handle_command(Socket, Username, CommandLine, State) ->
%     Words = string:tokens(CommandLine, " "),
%     case Words of
%         ["create", RoomName] ->
%             case maps:is_key(RoomName, State#state.rooms) of
%                 true ->
%                     gen_tcp:send(Socket, <<"Room already exists.\n">>),
%                     State;
%                 false ->
%                     Rooms = State#state.rooms,
%                     NewRooms = Rooms#{RoomName => sets:add_element(Username, sets:new())},
%                     UserRooms = State#state.user_rooms,
%                     NewUserRooms = UserRooms#{Username => RoomName},
%                     gen_tcp:send(Socket, <<"Room created and joined.\n">>),
%                     State#state{rooms = NewRooms, user_rooms = NewUserRooms}
%             end;

%         ["join", RoomName] ->
%             case maps:get(RoomName, State#state.rooms, undefined) of
%                 undefined ->
%                     gen_tcp:send(Socket, <<"Room not found.\n">>),
%                     State;
%                 Members ->
%                     UpdatedRoom = sets:add_element(Username, Members),
%                     Rooms = State#state.rooms,
%                     NewRooms = Rooms#{RoomName => UpdatedRoom},
%                     UserRooms = State#state.user_rooms,
%                     NewURs = UserRooms#{Username => RoomName},
%                     gen_tcp:send(Socket, <<"Joined room.\n">>),
%                     State#state{rooms = NewRooms, user_rooms = NewURs}
%             end;

%         ["leave"] ->
%             case maps:get(Username, State#state.user_rooms, undefined) of
%                 undefined ->
%                     gen_tcp:send(Socket, <<"You are not in a room.\n">>),
%                     State;
%                 RoomName ->
%                     Members = maps:get(RoomName, State#state.rooms),
%                     UpdatedRoom = sets:del_element(Username, Members),
%                     Rooms = State#state.rooms,
%                     NewRooms = Rooms#{RoomName => UpdatedRoom},
%                     NewURs = maps:remove(Username, State#state.user_rooms),
%                     gen_tcp:send(Socket, <<"Left room.\n">>),
%                     State#state{rooms = NewRooms, user_rooms = NewURs}
%             end;

%         ["rooms"] ->
%             RoomList = maps:keys(State#state.rooms),
%             ListText = lists:map(fun(R) -> io_lib:format("~s\n", [R]) end, RoomList),
%             gen_tcp:send(Socket, list_to_binary(lists:flatten(ListText))),
%             State;

%         _ ->
%             gen_tcp:send(Socket, <<"Unknown command.\n">>),
%             State
%     end.

% broadcast_message(Sender, Msg, State) ->
%     case maps:get(Sender, State#state.user_rooms, undefined) of
%         undefined ->
%             Socket = maps:get(Sender, State#state.users),
%             gen_tcp:send(Socket, <<"You are not in a room.\n">>),
%             State;
%         Room ->
%             Members = maps:get(Room, State#state.rooms),
%             lists:foreach(fun(User) ->
%                 case maps:get(User, State#state.users, undefined) of
%                     undefined -> ok;
%                     Sock -> gen_tcp:send(Sock, io_lib:format("~s says: ~s\n", [Sender, Msg]))
%                 end
%             end, sets:to_list(Members)),
%             State
%     end.
