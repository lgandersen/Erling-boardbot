-module(pgsql).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {boardurl, host, port, username, password, database, connection, last_post, last_splash}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, post/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([DBHost, DBPort, DBUser, DBPass, DBName, BoardURL]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBHost, DBPort, DBUser, DBPass, DBName, BoardURL], []).

post(Poster, Post) ->
    gen_server:cast(?SERVER, {post, [Poster, Post]}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DBHost, DBPort, DBUser, DBPass, DBName, BoardURL]) ->
    State_ = #state{host=DBHost, port=DBPort, username=DBUser, password=DBPass, database=DBName, boardurl=list_to_binary(BoardURL)},
    case inet:parse_address(DBHost) of
      {ok, DBHost_parsed} ->
        ok;
      {error, einval} ->
        DBHost_parsed = DBHost
    end,
    {ok, C} = epgsql:connect(DBHost_parsed, DBUser, DBPass, [
        {database, DBName},
        {timeout, 4000}
        ]),
    State = State_#state{connection=C},
    spawn_link(fun() -> monitor_boardet(State) end),
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.


handle_cast({post, [Poster, Post]}, State) ->
    State_ = insert_post(Poster, Post, State),
    {noreply, State_};
handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.
    

terminate(_Reason, #state{connection=C} = _State) ->
    epgsql:close(C),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
insert_post(Poster, Post, State) ->
    Query = "INSERT INTO posts (name, post, time) VALUES ($1, $2, NOW()) RETURNING id;",
    {ok, _, _, [{PostId}]} = epgsql:equery(State#state.connection, Query, [Poster, Post]),
    State#state{last_post=PostId}.


last_post_and_splash(C) ->
    {ok, _, [{LastPostId}, {LastSplashId}]} = epgsql:equery(C, "select max(posts.id) from posts UNION ALL select max(splash.id) from splash;"),
    {LastPostId, LastSplashId}.


monitor_boardet(#state{connection=C, boardurl=Url} = State) ->
    timer:sleep(10000),
    {ok, _, Posts} = epgsql:equery(C, "SELECT name,post FROM posts WHERE id > $1;", [State#state.last_post]),
    {ok, _, Splashs} = epgsql:equery(C, "SELECT filepath FROM splash WHERE id > $1;", [State#state.last_splash]),
    {LastPostId, LastSplashId} = last_post_and_splash(C),
    print_splash_to_irc(Splashs, Url),
    print_posts_to_irc([{fucked_decode(Name), fucked_decode(Content)} || {Name, Content} <- Posts]),
    monitor_boardet(State#state{last_post=LastPostId, last_splash=LastSplashId}).


fucked_decode(Post_) ->
    Replace = fun(Post, Patt, Replac) -> binary:replace(Post, Patt, Replac, [global]) end,
    Post_urldecoded = list_to_binary(url_decode(binary_to_list(Post_))),
    Post_wtfdecoded = Replace(Replace(Post_urldecoded, <<"<br+/>\r">>, <<"">>), <<"+">>, <<" ">>),
    binary:split(Post_wtfdecoded, <<"\n">>, [global]).

print_splash_to_irc(Splashs, Url) ->
    [irc:say(<<"\x02Nyt splash:\x0f ", Url/binary, Splash/binary>>) || {Splash} <- Splashs].

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
