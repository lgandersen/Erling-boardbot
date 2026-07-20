%%%-------------------------------------------------------------------
%%% @doc Board database gen_server.
%%%
%%% Two jobs:
%%%   1. Poll the `posts'/`splash' tables and relay anything new to IRC via
%%%      {@link irc:say/1}.
%%%   2. Insert posts submitted through IRC (`irc' casts `{post, ...}' here).
%%%
%%% Design notes vs. the original:
%%%   * `init/1' does NO blocking work -- it schedules a `connect' message and
%%%     returns, so the process is restartable and the supervisor never blocks
%%%     on an unreachable database. Connect/reconnect uses exponential backoff.
%%%   * The old `spawn_link(fun monitor_boardet/1)' -- an unsupervised infinite
%%%     recursion around `timer:sleep/1' -- is gone. Polling is a `poll' message
%%%     rescheduled with `erlang:send_after/3' inside the gen_server.
%%%   * `last_post'/`last_splash' are SEEDED from `max(id)' at connect time, so
%%%     a restart does not replay the whole board history. (The original never
%%%     seeded them, so the first `WHERE id > $1' ran against `undefined'.)
%%%   * The `fucked_decode/1' + vendored YAWS `url_decode/1' layer is deleted.
%%%     It undid legacy PHP urlencode + nl2br mangling; the board is now Flask
%%%     and stores clean UTF-8 plaintext (see boardet app.py / migrate.py), so
%%%     that layer was dead and corrupted any message containing `+' or `%'.
%%% @end
%%%-------------------------------------------------------------------
-module(pgsql).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% Poll interval and reconnect backoff bounds (milliseconds).
-define(POLL_INTERVAL, 10000).
-define(BACKOFF_MIN, 1000).
-define(BACKOFF_MAX, 60000).

-record(state, {config :: erling_config:db_config(),
                boardurl :: binary(),
                connection :: epgsql:connection() | undefined,
                last_post = 0 :: non_neg_integer(),
                last_splash = 0 :: non_neg_integer(),
                backoff = ?BACKOFF_MIN :: pos_integer()}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([start_link/1, post/2]).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-spec start_link(erling_config:db_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec post(binary(), binary()) -> ok.
post(Poster, Post) ->
    gen_server:cast(?SERVER, {post, Poster, Post}).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

-spec init(erling_config:db_config()) -> {ok, #state{}}.
init(#{boardurl := BoardURL} = Config) ->
    process_flag(trap_exit, true),
    self() ! connect,
    {ok, #state{config = Config, boardurl = BoardURL}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Not connected yet -> drop; the poster will get the "As you wish" ack from
%% IRC regardless, and losing a post on a DB outage is acceptable here.
handle_cast({post, _Poster, _Post}, #state{connection = undefined} = State) ->
    logger:warning("Dropping board post: database not connected"),
    {noreply, State};
handle_cast({post, Poster, Post}, State) ->
    {noreply, insert_post(Poster, Post, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    {noreply, do_connect(State)};

handle_info(poll, #state{connection = undefined} = State) ->
    %% Disconnected; the reconnect flow will resume polling.
    {noreply, State};
handle_info(poll, State) ->
    {noreply, poll(State)};

%% epgsql owns a linked connection process; if it dies we get an EXIT and
%% reconnect with backoff.
handle_info({'EXIT', Conn, Reason}, #state{connection = Conn} = State) ->
    logger:warning("Database connection died: ~p", [Reason]),
    {noreply, schedule_reconnect(State#state{connection = undefined})};
handle_info({'EXIT', _Other, _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection = undefined}) ->
    ok;
terminate(_Reason, #state{connection = Conn}) ->
    _ = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Connection management
%% ------------------------------------------------------------------

do_connect(#state{config = Config} = State) ->
    #{host := Host, username := User, password := Pass,
      database := DB} = Config,
    ConnHost = parse_host(Host),
    case epgsql:connect(ConnHost, User, Pass,
                        #{database => DB, timeout => 4000}) of
        {ok, Conn} ->
            logger:info("Database connected (~s@~s/~s)", [User, Host, DB]),
            %% Seed the high-water marks so a restart does not replay history.
            {LastPost, LastSplash} = current_max_ids(Conn),
            State1 = State#state{connection = Conn,
                                 last_post = LastPost,
                                 last_splash = LastSplash,
                                 backoff = ?BACKOFF_MIN},
            erlang:send_after(?POLL_INTERVAL, self(), poll),
            State1;
        {error, Reason} ->
            logger:warning("Database connect failed: ~p", [Reason]),
            schedule_reconnect(State)
    end.

%% epgsql wants an inet address tuple for a literal IP, or a hostname string.
parse_host(Host) ->
    case inet:parse_address(Host) of
        {ok, Addr} -> Addr;
        {error, einval} -> Host
    end.

schedule_reconnect(#state{backoff = Backoff} = State) ->
    erlang:send_after(Backoff, self(), connect),
    State#state{backoff = min(Backoff * 2, ?BACKOFF_MAX)}.

%% ------------------------------------------------------------------
%% Polling and inserting
%% ------------------------------------------------------------------

poll(#state{connection = Conn, boardurl = Url,
            last_post = LastPost, last_splash = LastSplash} = State) ->
    try
        {ok, _, Posts} =
            epgsql:equery(Conn, "SELECT name, post FROM posts WHERE id > $1;",
                          [LastPost]),
        {ok, _, Splashes} =
            epgsql:equery(Conn, "SELECT filepath FROM splash WHERE id > $1;",
                          [LastSplash]),
        {NewPost, NewSplash} = current_max_ids(Conn),
        print_splashes_to_irc(Splashes, Url),
        print_posts_to_irc(Posts),
        erlang:send_after(?POLL_INTERVAL, self(), poll),
        State#state{last_post = NewPost, last_splash = NewSplash}
    catch
        Class:Reason ->
            logger:warning("Board poll failed (~p:~p); reconnecting",
                           [Class, Reason]),
            _ = (try epgsql:close(Conn) catch _:_ -> ok end),
            schedule_reconnect(State#state{connection = undefined})
    end.

insert_post(Poster, Post, #state{connection = Conn} = State) ->
    Query = "INSERT INTO posts (name, post, time) VALUES ($1, $2, NOW()) "
            "RETURNING id;",
    case epgsql:equery(Conn, Query, [Poster, Post]) of
        {ok, _Count, _Cols, [{PostId}]} ->
            %% We just inserted it; advance the mark so the poll loop does not
            %% echo it straight back to the channel.
            State#state{last_post = PostId};
        Other ->
            logger:warning("Board insert failed: ~p", [Other]),
            State
    end.

current_max_ids(Conn) ->
    {ok, _, [{MaxPost}, {MaxSplash}]} =
        epgsql:equery(Conn,
            "SELECT max(id) FROM posts "
            "UNION ALL SELECT max(id) FROM splash;"),
    {null_to_zero(MaxPost), null_to_zero(MaxSplash)}.

null_to_zero(null) -> 0;
null_to_zero(N) when is_integer(N) -> N.

%% ------------------------------------------------------------------
%% Relaying to IRC
%% ------------------------------------------------------------------

print_splashes_to_irc(Splashes, Url) ->
    [irc:say(<<"\x02Nyt splash:\x0f ", Url/binary, Splash/binary>>)
     || {Splash} <- Splashes].

print_posts_to_irc(Posts) ->
    lists:foreach(fun print_post/1, Posts).

print_post({Name, Content}) ->
    irc:say(<<"\x02Nyt boardindlaeg\x0f fra \x02", Name/binary, "\x0f:">>),
    %% Content is clean UTF-8 with real newlines; one IRC line per line.
    [irc:say(Line) || Line <- binary:split(Content, <<"\n">>, [global]),
                      Line =/= <<>>].
