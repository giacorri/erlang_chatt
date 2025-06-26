%%--------------------------------------------------------------------
%% @doc Chat room process that keeps track of connected users
%%      and broadcasts messages to all participants.
%%--------------------------------------------------------------------

-module(chatt_room).

%% We use gen_server for an OTP-compliant process
-behaviour(gen_server).

%% Public API exports
-export([start_link/0, join/2, broadcast/2]).

%% gen_server callback exports
-export([
    init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

%% Starts the chat room process and registers it locally
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called by a client process to register (join) the chat room
%% Takes a Username (as string) and their Socket
join(Username, Socket) ->
    gen_server:cast(?MODULE, {join, Username, Socket}).

%% Called by a client process to broadcast a message to all others
broadcast(FromUser, Message) ->
    gen_server:cast(?MODULE, {broadcast, FromUser, Message}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

%% Called when the chat room process starts
init([]) ->
    %% State is a map of Username => Socket
    {ok, #{}}.

%% Handle a user joining: add their socket to the user map
handle_cast({join, Username, Socket}, State) ->
    io:format("~s joined the chat~n", [Username]),
    %% Add the new user to the state map
    {noreply, State#{Username => Socket}};

%% Handle a broadcast request from a user
handle_cast({broadcast, FromUser, Msg}, State) ->
    %% Send the message to all other users (excluding the sender)
    lists:foreach(
        fun({Username, Socket}) when Username =/= FromUser ->
                %% Format: "Alice says: Hello"
                gen_tcp:send(Socket, io_lib:format("~s says: ~s", [FromUser, Msg]));
           (_) ->
                %% If Username == FromUser, skip
                ok
        end,
        maps:to_list(State)
    ),
    {noreply, State}.

%% No synchronous calls currently used, so return ok
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% No specific info messages handled
handle_info(_Info, State) ->
    {noreply, State}.

%% Optional cleanup logic (not used here)
terminate(_Reason, _State) -> ok.

%% Handle version upgrade (not used here)
code_change(_OldVsn, State, _Extra) -> {ok, State}.
