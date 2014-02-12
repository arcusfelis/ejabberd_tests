%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_pubsub* modules
%%% 
%%% Based on XEP-0060, version 1.13
%%% @end
%%%===================================================================

-module(pubsub_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_PUBSUB, <<"http://jabber.org/protocol/pubsub">>).
-define(NS_ATOM,   <<"http://www.w3.org/2005/Atom">>).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, not_friend_tests},
     {group, friend_tests}].

not_friend_tests() ->
    [publish_case,
     subscribe_not_authorized_case].

friend_tests() ->
    [subscribe_not_found_case,
     subscribe_case].

groups() ->
    [{not_friend_tests, [sequence], not_friend_tests()},
     {friend_tests,     [sequence], friend_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(friend_tests, Config0) ->
    Config1 = escalus:create_users(Config0),
    Config2 = escalus:make_everyone_friends(Config1),
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    Config2;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config),
    Config.

init_per_testcase(CaseName, Config) ->
    delete_offline_messages(Config),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    delete_offline_messages(Config),
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% pubsub test cases
%%%===================================================================

%% Example 1. Publisher Publishes a New Weblog Entry
publish_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, publish_soliloquy_entry_iq()),
        ResultIQ = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, ResultIQ)
    end).

%% Example 124. Request to create a node
%% Example 131. Service replies with success and generated NodeID
%% Example 32. Entity subscribes to a node
subscribe_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        Node     = <<"princely_musings">>,
        AliceJID = escalus_utils:get_short_jid(Alice),
        BobJID   = escalus_utils:get_short_jid(Bob),

        %% Owner creates a node
        escalus:send(Alice, create_node_iq(AliceJID, Node)),
        CreResIQ = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, CreResIQ),

        %% Listener subscribe to node
        escalus:send(Bob, subscribe_iq(AliceJID, Node, BobJID)),
        SubResIQ = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_iq_result, SubResIQ)
    end).

%% Example 45. Node does not exist
subscribe_not_found_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        Node     = <<"princely_musings">>,
        AliceJID = escalus_utils:get_short_jid(Alice),
        BobJID   = escalus_utils:get_short_jid(Bob),
        escalus:send(Bob, subscribe_iq(AliceJID, Node, BobJID)),
        ErrorIQ = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], ErrorIQ)
    end).

%% Example 92. Entity is not authorized to retrieve items (presence subscription required)
subscribe_not_authorized_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        Node     = <<"princely_musings">>,
        AliceJID = escalus_utils:get_short_jid(Alice),
        BobJID   = escalus_utils:get_short_jid(Bob),
        escalus:send(Bob, subscribe_iq(AliceJID, Node, BobJID)),
        ErrorIQ = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"auth">>, <<"not-authorized">>], ErrorIQ)
%       escalus:assert(is_error, [<<"auth">>, <<"presence-subscription-required">>], ErrorIQ)
    end).


%%%===================================================================
%%% helpers
%%%===================================================================

publish_soliloquy_entry_iq() ->
    publish_iq(<<"princely_musings">>, soliloquy_entry()).

publish_iq(Node, Entry) ->
    escalus_stanza:iq(<<"set">>, [publish_body(Node, Entry)]).

publish_body(Node, Entry) ->
    #xmlel{
        name = <<"pubsub">>,
        attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
        children = [publish_publish(Node, Entry)]}.

publish_publish(Node, Entry) ->
    #xmlel{
        name = <<"publish">>,
        attrs = [{<<"node">>, Node}],
        children = [publish_item(Entry)]}.

publish_item(Entry) ->
    #xmlel{
        name = <<"item">>,
        children = [Entry]}.

new_weblog_entry(Title, Summary, Link, Id, Published, Updated) ->
    #xmlel{
        name = <<"entry">>,
        attrs = [{<<"xmlns">>, ?NS_ATOM}],
        children = [
            xmlel(<<"title">>, Title),
            xmlel(<<"summary">>, Summary),
            link_elem(<<"link">>, Link),
            xmlel(<<"id">>, Id),
            xmlel(<<"published">>, Published),
            xmlel(<<"updated">>, Updated)
        ]}.

xmlel(Name, Text) ->
    #xmlel{
        name = Name,
        children = [#xmlcdata{content = Text}]}.

link_elem(Name, HRef) ->
    #xmlel{
        name = Name,
        attrs = [{<<"rel">>, <<"alternate">>},
                 {<<"type">>, <<"text/html">>},
                 {<<"href">>, HRef}]}.

soliloquy_entry() ->
    Title = <<"Soliloquy">>,
    Summary = <<"To be, or not to be: that is the question:\r\n"
                "Whether 'tis nobler in the mind to suffer\r\n"
                "The slings and arrows of outrageous fortune,\r\n"
                "Or to take arms against a sea of troubles,\r\n"
                "And by opposing end them?">>,
    Link = <<"http://denmark.lit/2003/12/13/atom03">>,
    Id = <<"tag:denmark.lit,2003:entry-32397">>,
    Published = <<"2003-12-13T18:30:02Z">>,
    Updated = <<"2003-12-13T18:30:02Z">>,
    new_weblog_entry(Title, Summary, Link, Id, Published, Updated).

create_node_iq(ToJID, Node) ->
    escalus_stanza:iq(ToJID, <<"set">>, [create_node_body(Node)]).

create_node_body(Node) ->
    #xmlel{
        name = <<"pubsub">>,
        attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
        children = [create_node_create(Node)]}.

create_node_create(Node) ->
    #xmlel{
        name = <<"create">>,
        attrs = [{<<"node">>, Node}]}.

subscribe_iq(ToJID, Node, SubscribedJID) ->
    Body = subscribe_body(Node, SubscribedJID),
    escalus_stanza:iq(ToJID, <<"set">>, [Body]).

subscribe_body(Node, SubscribedJID) ->
    #xmlel{
        name = <<"pubsub">>,
        attrs = [{<<"xmlns">>, ?NS_PUBSUB}],
        children = [subscribe_subscribe(Node, SubscribedJID)]}.

subscribe_subscribe(Node, SubscribedJID) ->
    #xmlel{
        name = <<"subscribe">>,
        attrs = [{<<"node">>, Node}, {<<"jid">>, SubscribedJID}]}.

delete_offline_messages(Config) ->
    SUs = serv_users(Config),
    %% It is not the best place to delete these messages.
    [delete_offline_messages(S, U) || {S, U} <- SUs],
    ok.

delete_offline_messages(Username, Server) ->
    catch escalus_ejabberd:rpc(mod_offline, remove_user, [Username, Server]),
    ok.

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.
