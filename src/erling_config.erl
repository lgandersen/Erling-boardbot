%%%-------------------------------------------------------------------
%%% @doc Configuration, resolved once at startup.
%%%
%%% Every setting comes from an `ERLING_*' environment variable, falling back
%%% to the `env' defaults in `erling.app.src' when the variable is unset or
%%% empty. Nothing is hardcoded at a call site.
%%%
%%% Types are normalised here so the gen_servers never have to parse strings:
%%% hosts stay as charlists (that is what `gen_tcp'/`epgsql' want), ports are
%%% integers, `irc_ssl' is a boolean, and everything that ends up on the wire
%%% or gets compared against wire data (nick, trigger, channels) is a binary.
%%% @end
%%%-------------------------------------------------------------------
-module(erling_config).

-export([irc/0, db/0, get/1]).

-export_type([irc_config/0, db_config/0]).

-type irc_config() :: #{host := string(),
                        port := inet:port_number(),
                        password := none | string(),
                        ssl := boolean(),
                        channels := [binary()],
                        trigger := binary(),
                        nick := binary(),
                        realname := binary()}.

-type db_config() :: #{host := string(),
                       port := inet:port_number(),
                       username := string(),
                       password := string(),
                       database := string(),
                       boardurl := binary()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec irc() -> irc_config().
irc() ->
    #{host     => str("ERLING_IRC_HOST", irchost),
      port     => int("ERLING_IRC_PORT", ircport),
      password => password("ERLING_IRC_PASS", ircpass),
      ssl      => bool("ERLING_IRC_SSL", ircssl),
      channels => bin_list("ERLING_CHANNELS", channels),
      trigger  => bin("ERLING_TRIGGER", trigger),
      nick     => bin("ERLING_NICK", nick),
      realname => bin("ERLING_REALNAME", realname)}.

-spec db() -> db_config().
db() ->
    #{host     => str("ERLING_DB_HOST", dbhost),
      port     => int("ERLING_DB_PORT", dbport),
      username => str("ERLING_DB_USER", dbuser),
      password => str("ERLING_DB_PASS", dbpass),
      database => str("ERLING_DB_NAME", dbname),
      boardurl => bin("ERLING_BOARD_URL", boardurl)}.

%% @doc The raw application-env default for `Key', for tests and diagnostics.
-spec get(atom()) -> term().
get(Key) ->
    {ok, Value} = application:get_env(erling, Key),
    Value.

%%%-------------------------------------------------------------------
%%% Internal
%%%-------------------------------------------------------------------

%% An unset OR empty variable falls back to the default: an empty string in a
%% generated env file is a mistake, not a deliberate "" setting.
-spec raw(string()) -> undefined | string().
raw(Var) ->
    case os:getenv(Var) of
        false -> undefined;
        ""    -> undefined;
        Value -> Value
    end.

str(Var, Key) ->
    case raw(Var) of
        undefined -> to_list(?MODULE:get(Key));
        Value -> Value
    end.

bin(Var, Key) ->
    case raw(Var) of
        undefined -> to_bin(?MODULE:get(Key));
        Value -> list_to_binary(Value)
    end.

int(Var, Key) ->
    case raw(Var) of
        undefined -> ?MODULE:get(Key);
        Value ->
            case string:to_integer(string:trim(Value)) of
                {Int, ""} when is_integer(Int) -> Int;
                _ -> error({bad_integer_env, Var, Value})
            end
    end.

bool(Var, Key) ->
    case raw(Var) of
        undefined -> ?MODULE:get(Key);
        Value ->
            case string:lowercase(string:trim(Value)) of
                V when V =:= "true";  V =:= "1"; V =:= "yes" -> true;
                V when V =:= "false"; V =:= "0"; V =:= "no"  -> false;
                _ -> error({bad_boolean_env, Var, Value})
            end
    end.

%% "none" (any case) means "send no PASS", matching the app.src default.
password(Var, Key) ->
    case raw(Var) of
        undefined -> ?MODULE:get(Key);
        Value ->
            case string:lowercase(string:trim(Value)) of
                "none" -> none;
                _ -> Value
            end
    end.

%% Comma-separated, e.g. ERLING_CHANNELS="#brobye,#erlol". Blanks are dropped
%% so a trailing comma is harmless.
bin_list(Var, Key) ->
    case raw(Var) of
        undefined ->
            [to_bin(C) || C <- ?MODULE:get(Key)];
        Value ->
            Trimmed = [string:trim(T) || T <- string:lexemes(Value, ",")],
            [list_to_binary(C) || C <- Trimmed, C =/= ""]
    end.

to_list(Value) when is_binary(Value) -> binary_to_list(Value);
to_list(Value) when is_list(Value) -> Value;
to_list(Value) when is_atom(Value) -> atom_to_list(Value).

to_bin(Value) when is_binary(Value) -> Value;
to_bin(Value) when is_list(Value) -> list_to_binary(Value);
to_bin(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).
