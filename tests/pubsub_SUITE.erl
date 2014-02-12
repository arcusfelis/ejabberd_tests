%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_pubsub* modules
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
    [{group, pubsub_tests}].

all_tests() ->
    [new_entry_case].

groups() ->
    [{pubsub_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1).

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% pubsub tests
%%%===================================================================

new_entry_case(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, publish_soliloquy_entry_iq()),
        ResultIQ = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, ResultIQ)
    end).

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
