%%%-------------------------------------------------------------------
%%% @doc IRC client gen_server.
%%%
%%% Connects to an IRC server, joins channels, and relays between the channel
%%% and the board (posts a message on the `<trigger> post Author##Content'
%%% command; is poked by `pgsql' via {@link say/1} to announce new board
%%% posts/splashes).
%%%
%%% Design notes vs. the original:
%%%   * `init/1' does NO network work -- it just schedules a `connect' message
%%%     and returns, so the process is restartable and the supervisor's
%%%     `init' phase never blocks on an unreachable server.
%%%   * connecting, registering, joining and reconnect-with-backoff are all
%%%     driven by `handle_info' timers (`erlang:send_after/3').
%%%   * binaries throughout; the deprecated `string:tokens/2' / `string:strip/3'
%%%     are gone in favour of `string:lexemes/2' / `string:trim/1' / binary
%%%     splits.
%%% @end
%%%-------------------------------------------------------------------
-module(irc).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(CRLF, "\r\n").
-define(TCP_OPTIONS, [binary, {packet, line}, {active, true}]).

%% Delay between a successful registration and joining channels (gives the
%% server time to finish the registration/MOTD handshake). Matches the
%% original 10s sleep.
-define(JOIN_DELAY, 10000).

%% Reconnect backoff bounds (milliseconds).
-define(BACKOFF_MIN, 1000).
-define(BACKOFF_MAX, 60000).

-record(state, {config :: erling_config:irc_config(),
                conn   :: gen_tcp | ssl | undefined,
                sock   :: gen_tcp:socket() | ssl:sslsocket() | undefined,
                backoff = ?BACKOFF_MIN :: pos_integer()}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------
-export([start_link/1, say/1]).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Pure helpers exported for eunit.
-export([format/1, tokenize/1, get_sender_nick/1, where/3,
         split_author_content/1]).

-spec start_link(erling_config:irc_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Announce a line on every joined channel. Leading spaces are trimmed and
%% an empty message is dropped (an empty PRIVMSG is a protocol error).
-spec say(binary()) -> ok.
say(Msg) ->
    case string:trim(Msg, leading, " ") of
        <<>> -> ok;
        Trimmed -> gen_server:cast(?SERVER, {say, Trimmed})
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

-spec init(erling_config:irc_config()) -> {ok, #state{}}.
init(Config) ->
    process_flag(trap_exit, true),
    %% No blocking network work here: kick off the first connect attempt
    %% asynchronously and return immediately.
    self() ! connect,
    {ok, #state{config = Config}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({say, _Msg}, #state{sock = undefined} = State) ->
    %% Not connected yet; drop rather than crash. The board poll loop will
    %% re-announce anything it hasn't seen once we reconnect.
    {noreply, State};
handle_cast({say, Msg}, #state{config = #{channels := Channels}} = State) ->
    [ircsend({channel, Channel}, Msg, State) || Channel <- Channels],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    {noreply, do_connect(State)};

handle_info(join_channels, #state{sock = undefined} = State) ->
    %% Connection dropped before the join timer fired; ignore.
    {noreply, State};
handle_info(join_channels, #state{config = #{channels := Channels}} = State) ->
    [ircsend(join_channel, Channel, State) || Channel <- Channels],
    {noreply, State};

%% Inbound line. `{packet, line}' hands us one CRLF-terminated line at a time,
%% as {tcp,...} for gen_tcp and {ssl,...} for ssl -- hence the wildcard tag.
handle_info({Tag, _Sock, <<$:, Rest/binary>>}, State)
  when Tag =:= tcp; Tag =:= ssl ->
    parse_input(tokenize(Rest), State),
    {noreply, State};
handle_info({Tag, _Sock, <<"PING :", From/binary>>}, State)
  when Tag =:= tcp; Tag =:= ssl ->
    ircsend(<<"PONG :", (strip_crlf(From))/binary>>, State),
    {noreply, State};
handle_info({Tag, _Sock, _Data}, State)
  when Tag =:= tcp; Tag =:= ssl ->
    {noreply, State};

%% Connection lost -> reconnect with backoff.
handle_info({Tag, _Sock}, State)
  when Tag =:= tcp_closed; Tag =:= ssl_closed ->
    {noreply, schedule_reconnect(close_sock(State))};
handle_info({Tag, _Sock, Reason}, State)
  when Tag =:= tcp_error; Tag =:= ssl_error ->
    logger:warning("IRC socket error: ~p", [Reason]),
    {noreply, schedule_reconnect(close_sock(State))};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close_sock(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Connection management
%% ------------------------------------------------------------------

do_connect(#state{config = Config} = State) ->
    #{host := Host, port := Port, ssl := SSL,
      password := Password, nick := Nick, realname := Realname} = Config,
    Conn = case SSL of true -> ssl; false -> gen_tcp end,
    case connect(Conn, Host, Port, SSL) of
        {ok, Sock} ->
            logger:info("IRC connected to ~s:~p (~s)",
                        [Host, Port, case SSL of true -> "TLS"; false -> "plain" end]),
            State1 = State#state{conn = Conn, sock = Sock, backoff = ?BACKOFF_MIN},
            ircsend(password, Password, State1),
            ircsend(nick, Nick, State1),
            ircsend(user, Realname, State1),
            erlang:send_after(?JOIN_DELAY, self(), join_channels),
            State1;
        {error, Reason} ->
            logger:warning("IRC connect to ~s:~p failed: ~p", [Host, Port, Reason]),
            schedule_reconnect(State)
    end.

connect(ssl, Host, Port, true) ->
    ssl:connect(Host, Port, ?TCP_OPTIONS ++ ssl_opts(Host), 10000);
connect(gen_tcp, Host, Port, false) ->
    gen_tcp:connect(Host, Port, ?TCP_OPTIONS, 10000).

%% TLS with real verification -- never verify_none. A bind-mounted CA bundle
%% (ERLING_IRC_CACERTFILE) is used when present, otherwise the system trust
%% store; hostname checking is left on.
ssl_opts(Host) ->
    Base = [{verify, verify_peer},
            {depth, 5},
            {server_name_indication, ensure_list(Host)},
            {customize_hostname_check,
             [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}],
    case os:getenv("ERLING_IRC_CACERTFILE") of
        false -> [{cacerts, public_key:cacerts_get()} | Base];
        ""    -> [{cacerts, public_key:cacerts_get()} | Base];
        Path  -> [{cacertfile, Path} | Base]
    end.

schedule_reconnect(#state{backoff = Backoff} = State) ->
    erlang:send_after(Backoff, self(), connect),
    State#state{backoff = min(Backoff * 2, ?BACKOFF_MAX)}.

close_sock(#state{conn = undefined} = State) ->
    State;
close_sock(#state{conn = Conn, sock = Sock} = State) when Sock =/= undefined ->
    try Conn:close(Sock) catch _:_ -> ok end,
    State#state{conn = undefined, sock = undefined};
close_sock(State) ->
    State#state{conn = undefined, sock = undefined}.

%% ------------------------------------------------------------------
%% Sending
%% ------------------------------------------------------------------

%% Raw line: append CRLF and put it on the wire.
ircsend(Line, #state{conn = Conn, sock = Sock})
  when Conn =/= undefined, Sock =/= undefined ->
    Conn:send(Sock, <<(to_bin(Line))/binary, ?CRLF>>);
ircsend(_Line, _State) ->
    ok.  % not connected; drop

ircsend({channel, Channel}, Msg, State) ->
    ircsend(<<"PRIVMSG ", (to_bin(Channel))/binary, " :", (to_bin(Msg))/binary>>, State);
ircsend({private, Nick}, Msg, State) ->
    ircsend(<<"PRIVMSG ", (to_bin(Nick))/binary, " :", (to_bin(Msg))/binary>>, State);
ircsend({channel@nick, {Channel, Nick}}, Msg, State) ->
    ircsend(<<"PRIVMSG ", (to_bin(Channel))/binary, " :",
              (to_bin(Nick))/binary, ": ", (to_bin(Msg))/binary>>, State);
ircsend(nick, Nick, State) ->
    ircsend(<<"NICK ", (to_bin(Nick))/binary>>, State);
ircsend(join_channel, Channel, State) ->
    ircsend(<<"JOIN ", (to_bin(Channel))/binary>>, State);
ircsend(user, Realname, State) ->
    ircsend(<<"USER lol lool bla :", (to_bin(Realname))/binary>>, State);
ircsend(password, none, _State) ->
    ok;
ircsend(password, Password, State) ->
    ircsend(<<"PASS ", (to_bin(Password))/binary>>, State).

%% ------------------------------------------------------------------
%% Parsing (pure)
%% ------------------------------------------------------------------

%% @doc Split a raw IRC line body (the part after the leading `:') into
%% space-separated binary tokens. The trailing CRLF stays on the last token
%% and is stripped later by {@link format/1}.
-spec tokenize(binary()) -> [binary()].
tokenize(Line) ->
    string:lexemes(Line, " ").

%% @doc The nick portion of a `nick!user@host' hostmask.
-spec get_sender_nick(binary()) -> binary().
get_sender_nick(Hostmask) ->
    hd(binary:split(Hostmask, <<"!">>)).

%% @doc Where a PRIVMSG was addressed: to us directly, to a channel we are in,
%% or neither.
-spec where(binary(), [binary()], binary()) -> me | channel | neither.
where(SentTo, MyChannels, MyNick) ->
    if
        SentTo =:= MyNick -> me;
        true ->
            case lists:member(SentTo, MyChannels) of
                true -> channel;
                false -> neither
            end
    end.

%% @doc Turn the PRIVMSG trailing tokens into the message words: drop the
%% leading `:' from the first word and the trailing CRLF from the last.
-spec format([binary()]) -> [binary()].
format([]) ->
    [];
format([First | Rest]) ->
    Head = strip_leading_colon(First),
    case Rest of
        [] -> [strip_crlf(Head)];
        _  -> [Head | drop_last_crlf(Rest)]
    end.

drop_last_crlf(Words) ->
    {Init, [Last]} = lists:split(length(Words) - 1, Words),
    Init ++ [strip_crlf(Last)].

parse_input([Hostmask, <<"PRIVMSG">>, SentTo | Privmsg] = Tokens,
            #state{config = #{nick := MyNick, channels := MyChannels}} = State) ->
    SenderNick = get_sender_nick(Hostmask),
    case where(SentTo, MyChannels, MyNick) of
        channel ->
            log_tokens("Channel msg received: ~ts", Tokens),
            parse_msg_from_channel({channel@nick, {SentTo, SenderNick}},
                                   format(Privmsg), State);
        me ->
            log_tokens("Private msg received: ~ts", Tokens),
            parse_msg_from_user({private, SenderNick}, format(Privmsg), State);
        neither ->
            log_tokens("Could not place PRIVMSG: ~ts", Tokens)
    end;
parse_input(Tokens, _State) ->
    log_tokens("Non-PRIVMSG msg received: ~ts", Tokens).

parse_msg_from_channel(Target, [Trigger, <<"post">> | MsgTokens],
                       #state{config = #{trigger := Trigger}} = State) ->
    post_msg(Target, MsgTokens, State);
parse_msg_from_channel(Target, [Trigger, <<"hello">>],
                       #state{config = #{trigger := Trigger}} = State) ->
    ircsend(Target, <<"Hej med dig!">>, State);
parse_msg_from_channel(_Target, Privmsg, _State) ->
    log_tokens("This message was probably not for me: ~ts", Privmsg).

parse_msg_from_user(Target, [<<"post">> | MsgTokens], State) ->
    post_msg(Target, MsgTokens, State);
parse_msg_from_user(Target, [<<"hello">>], State) ->
    ircsend(Target, <<"Hej med dig!">>, State);
parse_msg_from_user(_Target, Privmsg, _State) ->
    log_tokens("This message was probably not for me: ~ts", Privmsg).

post_msg(Target, MsgTokens, State) ->
    Msg = join(MsgTokens, <<" ">>),
    case split_author_content(Msg) of
        {ok, Author, Content} ->
            pgsql:post(Author, Content),
            ircsend(Target, <<"As you wish..">>, State);
        error ->
            ircsend(Target, <<"Kunne ikke formatere din Board besked :<">>, State)
    end.

%% @doc Split a `Author##Content' board command on the first `##'. Surrounding
%% whitespace is trimmed. Returns `error' when there is no `##' or either side
%% is empty.
-spec split_author_content(binary()) -> {ok, binary(), binary()} | error.
split_author_content(Msg) ->
    case binary:split(string:trim(Msg), <<"##">>) of
        [Author0, Content0] ->
            Author = string:trim(Author0),
            Content = string:trim(Content0),
            case {Author, Content} of
                {<<>>, _} -> error;
                {_, <<>>} -> error;
                _ -> {ok, Author, Content}
            end;
        _ ->
            error
    end.

%% ------------------------------------------------------------------
%% Small utilities
%% ------------------------------------------------------------------

log_tokens(Template, Tokens) ->
    logger:info(Template, [join(Tokens, <<" ">>)]).

join(Bins, Sep) ->
    iolist_to_binary(lists:join(Sep, Bins)).

strip_leading_colon(<<$:, Rest/binary>>) -> Rest;
strip_leading_colon(Bin) -> Bin.

strip_crlf(Bin) ->
    string:trim(Bin, trailing, "\r\n").

to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(List) when is_list(List) -> unicode:characters_to_binary(List).

ensure_list(Bin) when is_binary(Bin) -> binary_to_list(Bin);
ensure_list(List) when is_list(List) -> List.
