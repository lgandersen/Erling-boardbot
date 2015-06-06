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
    ircsend(privmsg_channels, Msg, State),
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

ircsend(privmsg_channels, Msg, State) ->
    [ ircsend({privmsg_channel, Channel}, Msg, State) || Channel <- State#state.channels];
ircsend({privmsg_channel, Channel}, Msg, State) when is_binary(Msg)==true ->
    Channel_bin = list_to_binary(Channel),
    ircsend(<<"PRIVMSG " , Channel_bin/binary, " :", Msg/binary>>, State);
ircsend({privmsg_channel, Channel}, Msg, State) ->
    ircsend("PRIVMSG " ++ Channel ++ " :" ++ Msg, State);
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


is_channel(Channel, [Channel_next | Rest]) ->
    case Channel == Channel_next of
      true -> true;
      false -> is_channel(Channel, Rest)
    end;
is_channel(_Channel, []) -> false.


% Remove colon at beginning of the msg and '\r\n' of last word
prepare_privmsg([FirstWord_ | Rest]) ->
    FirstWord = string:substr(FirstWord_, 2), % Remove colon
    Remove_linebreak = fun(Word) -> string:substr(Word, 1, string:len(Word) - 2) end,
    case length(Rest) of
      0 ->
        Prepared_msg = [Remove_linebreak(FirstWord)];
      _ ->
        [LastWord | ReverseRest] = lists:reverse(Rest),
        NewRest = lists:reverse([Remove_linebreak(LastWord) | ReverseRest]),
        Prepared_msg = [FirstWord | NewRest ]
    end,
    Prepared_msg.


parse_input([Hostmask, "PRIVMSG", Receiver | Privmsg] = Msg_tokens, #state{nick=MyNick, channels=MyChannels} = State) ->
    case {is_channel(Receiver, MyChannels), Receiver == MyNick}  of
      {true, _} ->
        log_msg("Channel msg received: ~p~n", Msg_tokens),
        parse_privmsg({channel, Receiver}, prepare_privmsg(Privmsg), get_sender_nick(Hostmask), State);
      {_, true} ->
        log_msg("Private msg received: ~p~n", Msg_tokens),
        parse_privmsg(private, prepare_privmsg(Privmsg), get_sender_nick(Hostmask), State);
      _ ->
        log_msg("Could not parse PRIVMSG: ~p~n", Msg_tokens)
    end;
parse_input(Input, _State) ->
    log_msg("Non-PRIVMSG msg received: ~p~n", Input).


parse_privmsg({channel, Channel}, [Trigger, "post" | _Msgtokens_Rest], SenderNick, State) when Trigger == State#state.trigger ->
    log_msg("Post request in channel!\n");
parse_privmsg({channel, Channel}, [Trigger, "hello"], SenderNick, State) when Trigger == State#state.trigger ->
    ircsend("PRIVMSG " ++ Channel ++ " :" ++ SenderNick ++ ": Hej med dig.", State);
parse_privmsg(private, "post", SenderNick, State) ->
    log_msg("Private post request!\n");
parse_privmsg(private, ["hello"], SenderNick, State) ->
    ircsend("PRIVMSG " ++ SenderNick ++ " :Hej med dig.", State);
parse_privmsg(_, Privmsg, _Sender, State) ->
    log_msg("This message was probably not for me: '~p'\n", Privmsg).
