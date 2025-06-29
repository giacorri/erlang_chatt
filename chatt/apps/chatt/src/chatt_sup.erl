%%%-------------------------------------------------------------------
%% @doc chatt top level supervisor.
%% @end
%%%-------------------------------------------------------------------

% Define the module name
-module(chatt_sup).

% Specify that this module implements the 'supervisor' behaviour
-behaviour(supervisor).

% Export the start_link/0 function so it can be called externally
-export([start_link/0]).

% Export the init/1 callback required by the supervisor behaviour
-export([init/1]).

% Define a macro for referring to this module (used in registered name)
-define(SERVER, ?MODULE).

% Starts the supervisor and registers it locally with the name ?SERVER (i.e., chatt_sup)
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% The init/1 function is called when the supervisor starts.
% It returns supervision flags and a list of child specs.

init([]) ->
    % Supervision strategy and restart parameters.
    SupFlags = #{
        strategy => one_for_all,  % If one child dies, all others are killed and restarted
        intensity => 0,           % Max restarts allowed
        period => 1               % In this period of time (seconds)
    },

    % List of children to supervise.
    ChildSpecs = [
        #{id => chatt_server,
        start => {chatt_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [chatt_server]},
        #{id => chatt_room,
        start => {chatt_room, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [chatt_room]}
    ],

    % Return the tuple expected by the supervisor
    {ok, {SupFlags, ChildSpecs}}.
