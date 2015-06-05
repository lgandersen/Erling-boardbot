-module(pgsql).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {host, port, username, password, database, connection, last_post}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([DBHost, DBPort, DBUser, DBPass, DBName]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBHost, DBPort, DBUser, DBPass, DBName], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DBHost, DBPort, DBUser, DBPass, DBName]) ->
    State = #state{host=DBHost, port=DBPort, username=DBUser, password=DBPass, database=DBName},
    {ok, C} = epgsql:connect(DBHost, DBUser, DBPass, [
        {database, DBName},
        {timeout, 4000}
        ]),
    monitor_posts(State#state{connection=C}),
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
    

terminate(_Reason, #state{connection=C} = _State) ->
    epgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

last_post(C) ->
    {ok, _, [{Last_post}]} = epgsql:squery(C, "SELECT max(id) FROM posts;"),
    Pk = list_to_integer(binary_to_list(Last_post)),
    Pk.

monitor_posts(State) ->
    timer:sleep(3000),
    Pk = last_post(State#state.connection),
    {ok, _, Posts} = epgsql:equery(State#state.connection, "SELECT name,post FROM posts WHERE id > $1;", [Pk]),
    Posts_parsed = parse_posts(Posts, []),
    print_posts_to_irc(Posts_parsed),
    monitor_posts(State#state{last_post=Pk}).


parse_posts([], Processed) ->
    lists:reverse(Processed);
parse_posts([{Name, Content} | Rest], Processed) ->
    Name_ = parse_fucked_encoding(Name),
    Content_ = parse_fucked_encoding(Content),
    parse_posts(Rest, [{Name_, Content_} | Processed]).


parse_fucked_encoding(Post_) ->
    Post_urldecoded = list_to_binary(url_decode(binary_to_list(Post_))),
    Replace = fun(Post, Patt, Replac) -> binary:replace(Post, Patt, Replac, [global]) end,
    Post_wtfdecoded = Replace(Replace(Post_urldecoded, <<"<br+/>\r">>, <<"">>), <<"+">>, <<" ">>),
    binary:split(Post_wtfdecoded, <<"\n">>, [global]).

print_posts_to_irc([{[Name], Contents} | Rest]) ->
    irc:say(<<"\x02Nyt boardindlaeg\x0f fra \x02" ,Name/binary ,"\x0f:\n">>),
    [ irc:say(Content_part) || Content_part <- Contents],
    print_posts_to_irc(Rest);
print_posts_to_irc([]) -> ok.
    

%% ------------------------------------------------------------------
%% Urldecoder code snippet taken from YAWS 
%% ------------------------------------------------------------------

url_decode(Path) ->
    {DecPath, QS} = url_decode(Path, []),
    DecPath1 = case file:native_name_encoding() of
                   latin1 ->
                       DecPath;
                   utf8 ->
                       case unicode:characters_to_list(list_to_binary(DecPath)) of
                           UTF8DecPath when is_list(UTF8DecPath) -> UTF8DecPath;
                           _ -> DecPath
                       end
               end,
    case QS of
        [] -> lists:flatten(DecPath1);
        _  -> lists:flatten([DecPath1, $?, QS])
    end.

url_decode([], Acc) ->
    {lists:reverse(Acc), []};
url_decode([$?|Tail], Acc) ->
    %% Don't decode the query string here, that is parsed separately.
    {lists:reverse(Acc), Tail};
url_decode([$%, Hi, Lo | Tail], Acc) ->
    Hex = erlang:list_to_integer([Hi, Lo], 16),
    url_decode(Tail, [Hex|Acc]);
url_decode([H|T], Acc) when is_integer(H) ->
    url_decode(T, [H|Acc]);
%% deep lists
url_decode([H|T], Acc) when is_list(H) ->
    case url_decode(H, Acc) of
        {P1, []} ->
            {P2, QS} = url_decode(T, []),
            {[P1,P2], QS};
        {P1, QS} ->
            {P1, QS++T}
    end.
