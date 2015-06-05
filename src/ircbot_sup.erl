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
    {ok, Host} = application:get_env(ircbot, host),
    {ok, Port} = application:get_env(ircbot, port),
    {ok, Password} = application:get_env(ircbot, password),
    {ok, SSL} = application:get_env(ircbot, ssl),
    {ok, Channels} = application:get_env(ircbot, channels),
    {ok, Trigger} = application:get_env(ircbot, trigger),
    {ok, Nick} = application:get_env(ircbot, nick),
    {ok, Realname} = application:get_env(ircbot, realname),
    Ircbot = {ircbot_server, {
        ircbot_server, start_link, [[Host, Port, Password, SSL, Channels, Trigger, Nick, Realname]]},
        permanent, 2000, worker, dynamic
        },

    {ok, { {one_for_one, 5, 10}, [Ircbot]} }.

