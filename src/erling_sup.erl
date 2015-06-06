-module(erling_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, IRCHost} = application:get_env(erling, host),
    {ok, IRCPort} = application:get_env(erling, port),
    {ok, IRCPass} = application:get_env(erling, password),
    {ok, IRCSSL} = application:get_env(erling, ssl),
    {ok, Channels} = application:get_env(erling, channels),
    {ok, Trigger} = application:get_env(erling, trigger),
    {ok, Nick} = application:get_env(erling, nick),
    {ok, Realname} = application:get_env(erling, realname),
    {ok, BoardURL} = application:get_env(erling, boardurl),
    {ok, DBHost} = application:get_env(erling, dbhost),
    {ok, DBPort} = application:get_env(erling, dbport),
    {ok, DBUser} = application:get_env(erling, dbuser),
    {ok, DBPass} = application:get_env(erling, dbpass),
    {ok, DBName} = application:get_env(erling, dbname),
    Ircbot = {irc, {
        irc, start_link, [[IRCHost, IRCPort, IRCPass, IRCSSL, Channels, Trigger, Nick, Realname]]},
        permanent, 2000, worker, dynamic
        },
    SQLdaemon = {pgsql, {
        pgsql, start_link, [[DBHost, DBPort, DBUser, DBPass, DBName, BoardURL]]},
        permanent, 2000, worker, dynamic
        },

    {ok, { {one_for_one, 5, 10}, [Ircbot, SQLdaemon]} }.

