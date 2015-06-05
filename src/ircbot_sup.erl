-module(ircbot_sup).

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
    {ok, IRCHost} = application:get_env(ircbot, host),
    {ok, IRCPort} = application:get_env(ircbot, port),
    {ok, IRCPass} = application:get_env(ircbot, password),
    {ok, IRCSSL} = application:get_env(ircbot, ssl),
    {ok, Channels} = application:get_env(ircbot, channels),
    {ok, Trigger} = application:get_env(ircbot, trigger),
    {ok, Nick} = application:get_env(ircbot, nick),
    {ok, Realname} = application:get_env(ircbot, realname),
    {ok, DBHost} = application:get_env(ircbot, dbhost),
    {ok, DBPort} = application:get_env(ircbot, dbport),
    {ok, DBUser} = application:get_env(ircbot, dbuser),
    {ok, DBPass} = application:get_env(ircbot, dbpass),
    {ok, DBName} = application:get_env(ircbot, dbname),
    Ircbot = {ircbot_server, {
        ircbot_server, start_link, [[IRCHost, IRCPort, IRCPass, IRCSSL, Channels, Trigger, Nick, Realname]]},
        permanent, 2000, worker, dynamic
        },
    SQLdaemon = {pgsql_server, {
        pgsql_server, start_link, [[DBHost, DBPort, DBUser, DBPass, DBName]]},
        permanent, 2000, worker, dynamic
        },

    {ok, { {one_for_one, 5, 10}, [Ircbot, SQLdaemon]} }.

