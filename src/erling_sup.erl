%%%-------------------------------------------------------------------
%%% @doc Top-level supervisor.
%%%
%%% Both children are `permanent' and now genuinely restartable: neither does
%%% blocking network work in `init/1' any more, so a restart is cheap and an
%%% unreachable peer no longer burns the supervisor's restart budget.
%%%
%%% `one_for_one': the two workers are independent. They talk to each other by
%%% registered name via `gen_server:cast', which is a no-op-ish drop if the
%%% peer is momentarily down, so neither needs the other restarted alongside
%%% it.
%%%
%%% (The previous version read `host'/`port'/`password'/`ssl' out of the
%%% application env, but erling.app.src defines those keys as
%%% `irchost'/`ircport'/`ircpass'/`ircssl' -- so `{ok, _} = get_env(...)'
%%% raised a badmatch and the application could never start. Configuration now
%%% goes through erling_config, which has one name for each setting.)
%%% @end
%%%-------------------------------------------------------------------
-module(erling_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    Children = [
        #{id => irc,
          start => {irc, start_link, [erling_config:irc()]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [irc]},
        #{id => pgsql,
          start => {pgsql, start_link, [erling_config:db()]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [pgsql]}
    ],
    {ok, {SupFlags, Children}}.
