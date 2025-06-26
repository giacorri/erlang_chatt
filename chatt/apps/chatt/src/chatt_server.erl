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
            Username = binary_to_list(string:trim(UsernameBin)),
            chatt_room:join(Username, Socket),
            gen_tcp:send(Socket, io_lib:format("Welcome, ~s! You joined the chat.\n", [Username])),
            loop(Socket, Username);  % Start message handling loop
        {error, closed} ->
            io:format("Client disconnected before sending username~n")
    end.

%%--------------------------------------------------------------------
%% loop/2 - Message loop for a connected client
%%--------------------------------------------------------------------
loop(Socket, Username) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("~s says: ~s", [Username, binary_to_list(Data)]),
            Message = binary_to_list(Data),
            chatt_room:broadcast(Username, Message),
            loop(Socket, Username);  % Keep receiving messages
        {error, closed} ->
            io:format("~s disconnected~n", [Username]),
            gen_tcp:close(Socket)
    end.
