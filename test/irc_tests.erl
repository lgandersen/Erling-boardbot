%%%-------------------------------------------------------------------
%%% @doc EUnit tests for the pure IRC parsing logic in {@link irc}.
%%% These are the bits most likely to break silently on a refactor: the line
%%% tokenizer, the PRIVMSG `format/1' colon/CRLF trimming, the hostmask nick
%%% split, the channel/self/neither classifier, and the `Author##Content'
%%% board-command splitter.
%%% @end
%%%-------------------------------------------------------------------
-module(irc_tests).

-include_lib("eunit/include/eunit.hrl").

%% ---- tokenize/1 ----

%% tokenize does NOT strip the trailing CRLF -- that is format/1's job, so the
%% last token still carries it.
tokenize_splits_on_spaces_test() ->
    ?assertEqual([<<"nick!u@h">>, <<"PRIVMSG">>, <<"#chan">>, <<":hello\r\n">>],
                 irc:tokenize(<<"nick!u@h PRIVMSG #chan :hello\r\n">>)).

tokenize_collapses_runs_of_spaces_test() ->
    ?assertEqual([<<"a">>, <<"b">>],
                 irc:tokenize(<<"a    b">>)).

%% ---- get_sender_nick/1 ----

sender_nick_from_full_hostmask_test() ->
    ?assertEqual(<<"lasse">>, irc:get_sender_nick(<<"lasse!~u@example.org">>)).

sender_nick_without_bang_is_whole_test() ->
    ?assertEqual(<<"server.name">>, irc:get_sender_nick(<<"server.name">>)).

%% ---- where/3 ----

where_me_test() ->
    ?assertEqual(me, irc:where(<<"Erling">>, [<<"#brobye">>], <<"Erling">>)).

where_channel_test() ->
    ?assertEqual(channel,
                 irc:where(<<"#brobye">>, [<<"#brobye">>, <<"#erlol">>], <<"Erling">>)).

where_neither_test() ->
    ?assertEqual(neither,
                 irc:where(<<"#other">>, [<<"#brobye">>], <<"Erling">>)).

%% ---- format/1 ----

format_single_word_strips_colon_and_crlf_test() ->
    ?assertEqual([<<"hello">>], irc:format([<<":hello\r\n">>])).

format_multi_word_strips_leading_colon_and_trailing_crlf_test() ->
    ?assertEqual([<<"Erling:">>, <<"post">>, <<"Bob##Hi\r\nthere">>, <<"world">>],
                 irc:format([<<":Erling:">>, <<"post">>, <<"Bob##Hi\r\nthere">>,
                             <<"world\r\n">>])).

format_empty_test() ->
    ?assertEqual([], irc:format([])).

%% Realistic channel PRIVMSG trailing tokens -> the "hello" command words.
format_hello_command_test() ->
    ?assertEqual([<<"Erling:">>, <<"hello">>],
                 irc:format([<<":Erling:">>, <<"hello\r\n">>])).

%% ---- split_author_content/1 (post_msg core) ----

split_basic_test() ->
    ?assertEqual({ok, <<"Bob">>, <<"Hello world">>},
                 irc:split_author_content(<<"Bob##Hello world">>)).

split_trims_whitespace_test() ->
    ?assertEqual({ok, <<"Bob">>, <<"Hello">>},
                 irc:split_author_content(<<"  Bob  ##  Hello  ">>)).

%% Content may itself contain '##': only the FIRST '##' splits.
split_on_first_separator_test() ->
    ?assertEqual({ok, <<"Bob">>, <<"a ## b ## c">>},
                 irc:split_author_content(<<"Bob##a ## b ## c">>)).

%% Content containing '+' or '%' must survive verbatim (the bug the old
%% fucked_decode/url_decode layer introduced).
split_preserves_plus_and_percent_test() ->
    ?assertEqual({ok, <<"Bob">>, <<"1+1=2 100% sure">>},
                 irc:split_author_content(<<"Bob##1+1=2 100% sure">>)).

split_no_separator_is_error_test() ->
    ?assertEqual(error, irc:split_author_content(<<"just some text">>)).

split_empty_author_is_error_test() ->
    ?assertEqual(error, irc:split_author_content(<<"##content">>)).

split_empty_content_is_error_test() ->
    ?assertEqual(error, irc:split_author_content(<<"author##">>)).
