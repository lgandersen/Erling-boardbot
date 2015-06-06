-module(irc).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(LINESEPERATOR, "\r\n").

-define(TCP_OPTIONS, [binary, {packet, line}, {reuseaddr, true}]).

-record(state, {connection, sock, host, port, password, ssl, channels, nick, realname, trigger}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, say/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([Host, Port, Password, SSL, Channels, Trigger, Nick, Realname]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port, Password, SSL, Channels, Trigger, Nick, Realname], []).

say(Msg) -> say_sanitized(Msg).

say_sanitized(<<>>) -> ok;
say_sanitized(<<" ", Msg/binary>>) -> say(Msg);
say_sanitized(Msg) -> gen_server:cast(?SERVER, {say, Msg}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port, Password, SSL, Channels, Trigger, Nick, Realname]) ->
    State_ = #state{host=Host, port=Port, password=Password, ssl=SSL, channels=Channels, nick=Nick, realname=Realname, trigger=Trigger},

    Connection = case SSL of
        true ->       
            ssl:start(),
            ssl;
        false ->
            gen_tcp;
        _ ->
            {error, "SSL parameter must be 'true' or 'false'."}         
    end,
    case Connection:connect(Host, Port, ?TCP_OPTIONS) of
        {ok, Sock} ->
            State = State_#state{connection=Connection, sock=Sock},
            ircsend(password, Password, State),
            ircsend(nick, Nick, State),
            ircsend(user, Realname, State),
            [ircsend(join_channel, Channel, State) || Channel <- Channels ];
        {error, Reason} ->
            State = State_,
            io:format("Connect error: ~s~n", [inet:format_error(Reason)])
    end,
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.


handle_cast({say, Msg}, State) ->
    [ircsend({channel, Channel}, Msg, State) || Channel <- State#state.channels],
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({_, _Sock, <<$:, Msg/binary>>}, State) ->
    Msg_tokens = string:tokens(binary_to_list(Msg), " "),
    parse_input(Msg_tokens, State),
    {noreply, State};
handle_info({_, _Sock, <<"PING :", From/binary>>}, State) ->
    ircsend(<<"PONG :", From/binary>>, State),
    {noreply, State};
handle_info({_, _Sock, _Data}, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Connection = State#state.connection,
    Connection:close(State#state.sock),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
log_msg(Msg) ->
    io:format(Msg).

log_msg(Msg_template, Msg_tokens) ->
    io:format(Msg_template, [string:join(Msg_tokens, " ")]).


ircsend(Msg, State) ->
    Sock = State#state.sock,
    Connection = State#state.connection,
    if
      is_binary(Msg) -> 
        Connection:send(Sock, <<Msg/binary, ?LINESEPERATOR>>);
      true -> % Assume it is a string
        Connection:send(Sock, string:concat(Msg, ?LINESEPERATOR))
    end.

ircsend({channel, Channel}, Msg, State) when is_binary(Msg)==true ->
    Channel_bin = list_to_binary(Channel),
    ircsend(<<"PRIVMSG " , Channel_bin/binary, " :", Msg/binary>>, State);
ircsend({channel, Channel}, Msg, State) ->
    ircsend("PRIVMSG " ++ Channel ++ " :" ++ Msg, State);
ircsend({private, Nick}, Msg, State) ->
    ircsend("PRIVMSG " ++ Nick ++ " :" ++ Msg, State);
ircsend({channel@nick, {Channel, Nick}}, Msg, State) ->
    ircsend("PRIVMSG " ++ Channel ++ " :" ++ Nick ++ ": " ++ Msg, State);

ircsend(nick, Nick, State) -> ircsend("NICK " ++ Nick, State);
ircsend(join_channel, Channel, State) -> ircsend("JOIN " ++ Channel, State);
ircsend(user, Realname, State) -> ircsend("USER lol lool bla :" ++ Realname, State);
ircsend(password, Password, State) ->
    case Password of
        none -> ok;
        _ -> ircsend("PASS " ++ Password, State)
    end.


get_sender_nick(Hostmask) ->
    [SenderNick | _] = string:tokens(Hostmask, "!"),
    SenderNick.

where(SentTo, MyChannels, MyNick) ->
    InChannel = in_channels(SentTo, MyChannels),
    if
      SentTo == MyNick -> me;
      InChannel -> channel;
      true -> neither
    end.


in_channels(Channel, Channels) ->
    lists:any(fun(Chan) -> Chan == Channel end, Channels).


% Remove colon at beginning of the msg and '\r\n' of last word
format([FirstWord_ | Rest]) ->
    FirstWord = string:substr(FirstWord_, 2), % Remove colon
    Remove_linebreak = fun(Word) -> string:substr(Word, 1, string:len(Word) - 2) end,
    case length(Rest) of
      0 ->
        Prepared_msg = [Remove_linebreak(FirstWord)];
      _ ->
        NewRest = lists:append(lists:droplast(Rest), [Remove_linebreak(lists:last(Rest))]),
        Prepared_msg = [FirstWord | NewRest]
    end,
    Prepared_msg.


parse_input([Hostmask, "PRIVMSG", SentTo | Privmsg] = Msg_tokens, #state{nick=MyNick, channels=MyChannels} = State) ->
    SenderNick = get_sender_nick(Hostmask),
    case where(SentTo, MyChannels, MyNick)  of
      channel ->
        log_msg("Channel msg received: ~p~n", Msg_tokens),
        parse_msg_from_channel({channel@nick, {SentTo, SenderNick}}, format(Privmsg), State);
      me ->
        log_msg("Private msg received: ~p~n", Msg_tokens),
        parse_msg_from_user({private, SenderNick}, format(Privmsg), State);
      neither ->
        log_msg("Could not parse PRIVMSG: ~p~n", Msg_tokens)
    end;
parse_input(Input, _State) ->
    log_msg("Non-PRIVMSG msg received: ~p~n", Input).


parse_msg_from_channel(Target, [Trigger, "post", PostName | Post_tokens], State) when Trigger == State#state.trigger ->
    post_msg(Target, PostName, Post_tokens, State);
parse_msg_from_channel(Target, [Trigger, "hello"], State) when Trigger == State#state.trigger ->
    ircsend(Target, "Hej med dig!", State);
parse_msg_from_channel(_, Privmsg, _) -> log_msg("This message was probably not for me: '~p'\n", Privmsg).

parse_msg_from_user(Target, ["post", PostName | Post_tokens], State) ->
    post_msg(Target, PostName, Post_tokens, State);
parse_msg_from_user(Target, ["hello"], State) ->
    ircsend(Target, "Hej med dig!", State);
parse_msg_from_user(_, Privmsg, _) -> log_msg("This message was probably not for me: '~p'\n", Privmsg).


post_msg(Target, PostName_, Msg_tokens, State) ->
    Msg = string:join(Msg_tokens, " "),
    PostName = string:strip(PostName_, both, $#),
    case length(PostName_) - length(PostName) == 4 of
      true ->
        pgsql:post(PostName, Msg),
        ircsend(Target, "Beskeden er sendt!", State);
      false -> ircsend(Target, "Kunne ikke formatere din Board besked.", State)
    end.
