% Define the module
-module(chatt_server).

% Export the public functions
-export([start_link/0, start/0, accept_loop/1, handle_client/1]).

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
    {ok, Port} = application:get_env(chatt, chatt_port),
    % Set up a TCP listening socket
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,              % Receive data as binary
        {packet, line},      % Line-based parsing
        {active, false},     % Passive mode - we call gen_tcp:recv manually
        {reuseaddr, true}    % Allows quick restart on the same port
    ]),
    chatt_room:start_link(),
    ansi:green(io:format("Server listening on port ~p~n", [Port])),
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
                [$@ | Rest] ->
                    case string:tokens(Rest, " ") of
                        [Receiver | MsgWords] ->
                            Msg = string:join(MsgWords, " "),
                            chatt_room:send_private(Username, Receiver, Msg),
                            loop(Socket, Username);
                        _ ->
                            Error_string = ansi:red("Invalid private message format. Use @username message\n"),
                            gen_tcp:send(Socket, Error_string),
                            loop(Socket, Username)
                    end;
                _ ->
                    chatt_room:send_message(Username, Line),
                    loop(Socket, Username)
            end;
        {error, closed} ->
            chatt_room:unregister_user(Username),
            io:format("~s disconnected~n", [Username])
    end.
