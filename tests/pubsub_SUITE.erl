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
-define(NS_PUBSUB_OWNER, <<"http://jabber.org/protocol/pubsub#owner">>).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, all_tests}].

all_tests() ->
    [publish_case,
     subscribe_not_authorized_case,
     subscribe_not_found_case,
     subscribe_case,
     subscribe_and_publish_case,
     get_default_node_options_case].

groups() ->
    [{all_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config) ->
    escalus:create_users(escalus:init_per_suite(Config)).

end_per_suite(Config) ->
    escalus:end_per_suite(escalus:delete_users(Config)).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName,
        pre_init_per_testcase(CaseName, Config)).

pre_init_per_testcase(publish_case, Config) ->
    clean(presence_unsubscribe(Config));
pre_init_per_testcase(subscribe_not_authorized_case, Config) ->
    clean(presence_unsubscribe(Config));
pre_init_per_testcase(get_default_node_options_case, Config) ->
    clean(presence_unsubscribe(Config));
pre_init_per_testcase(subscribe_not_found_case, Config) ->
    clean(presence_subscribe(presence_unsubscribe(Config)));
pre_init_per_testcase(subscribe_case, Config) ->
    clean(presence_subscribe(presence_unsubscribe(Config)));
pre_init_per_testcase(subscribe_and_publish_case, Config) ->
    clean(presence_subscribe(presence_unsubscribe(Config))).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

clean(Config) ->
    delete_pubsub_objects(Config),
    delete_offline_messages(Config),
    Config.

presence_subscribe(Config0) ->
    Config1 = escalus:make_everyone_friends(Config0),
    escalus_ejabberd:wait_for_session_count(Config1, 0),
    Config1.

presence_unsubscribe(Config) ->
    delete_rosters(Config),
    Config.

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

subscribe_and_publish_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        Node     = <<"princely_musings">>,
        AliceJID = escalus_utils:get_short_jid(Alice),
        BobJID   = escalus_utils:get_short_jid(Bob),

        %% Owner creates a node
        escalus:send(Alice, create_node_iq(AliceJID, Node)),
        escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),

        %% Listener subscribe to node
        escalus:send(Bob, subscribe_iq(AliceJID, Node, BobJID)),
        escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Bob)),

        escalus:send(Alice, publish_soliloquy_entry_iq()),
        escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),

        Event = escalus_client:wait_for_stanza(Bob, 500),
        escalus:assert(is_message, Event)
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
        %% Owner creates a node
        escalus:send(Alice, create_node_iq(AliceJID, Node)),
        escalus:assert(is_iq_result, escalus_client:wait_for_stanza(Alice)),
        %% Listener subscribe to node
        escalus:send(Bob, subscribe_iq(AliceJID, Node, BobJID)),
        ErrorIQ = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"auth">>, <<"not-authorized">>], ErrorIQ)
%       escalus:assert(is_error, [<<"auth">>, <<"presence-subscription-required">>], ErrorIQ)
    end).

%% Example 151. Entity requests default node configuration options
get_default_node_options_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, get_default_node_options_iq()),
        ResultIQ = escalus_client:wait_for_stanza(Alice),
        ct:pal("ResultIQ ~p", [ResultIQ]),
        escalus:assert(is_iq_result, ResultIQ)
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

get_default_node_options_iq() ->
    escalus_stanza:iq(<<"get">>, [get_default_node_options_body()]).

get_default_node_options_body() ->
    #xmlel{
        name = <<"pubsub">>,
        attrs = [{<<"xmlns">>, ?NS_PUBSUB_OWNER}],
        children = [#xmlel{name = <<"default">>}]}.

delete_offline_messages(Config) ->
    SUs = serv_users(Config),
    [delete_offline_messages(U, S) || {S, U} <- SUs],
    ok.

delete_pubsub_objects(Config) ->
    SUs = serv_users(Config),
    [delete_pubsub_objects(U, S) || {S, U} <- SUs],
    ok.

delete_rosters(Config) ->
    SUs = serv_users(Config),
    [delete_rosters(U, S) || {S, U} <- SUs],
    ok.

delete_offline_messages(Username, Server) ->
    remove_user(Username, Server, [mod_offline, mod_offline_odbc]).

delete_pubsub_objects(Username, Server) ->
    remove_user(Username, Server, [mod_pubsub, mod_pubsub_odbc]).

delete_rosters(Username, Server) ->
    remove_user(Username, Server, [mod_roster, mod_roster_odbc]).

remove_user(Username, Server, Modules) ->
    [(catch escalus_ejabberd:rpc(M, remove_user, [Username, Server]))
     || M <- Modules, LM <- loaded_modules(Server), M =:= LM],
    ok.

loaded_modules(Server) ->
    escalus_ejabberd:rpc(gen_mod, loaded_modules, [Server]).

serv_users(Config) ->
    [serv_user(Config, UserSpec)
     || {_, UserSpec} <- escalus_users:get_users(all)].

serv_user(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    {Server, Username}.
