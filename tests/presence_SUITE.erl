%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(presence_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, presence},
     {group, presence_friends},
     {group, presence_priority},
     {group, roster},
     {group, subscribe_group}].

groups() ->
    [{presence, [sequence], [available,
                             available_direct,
                             additions,
                             invisible_presence]},
     {presence_friends, [sequence], [unavailable_multi,
                                     become_available_again]},
     {presence_priority, [sequence], [negative_priority_presence]},
     {roster, [sequence], [get_roster,
                           add_contact,
                           remove_contact]},
     {subscribe_group, [sequence], [subscribe,
                                    subscribe_decline,
                                    subscribe_relog,
                                    unsubscribe,
                                    remove_unsubscribe]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).


init_per_group(presence_friends, Config) ->
    Config1 = escalus:create_users(Config, {by_name, [alice, bob]}),
    escalus:make_everyone_friends(Config1);
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(add_contact, Config) ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(add_contact, Config);
end_per_testcase(subscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(subscribe_decline, Config) ->
    end_rosters_remove(Config);
end_per_testcase(unsubscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

end_rosters_remove(Config) ->
    [{_, UserSpec1}, {_, UserSpec2} | _] =
        escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec1),
    remove_roster(Config, UserSpec2),
    escalus:end_per_testcase(subscription, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

available(Config) ->
    escalus:story(Config, [1], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice))

        end).

available_direct(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"available">>)),
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received)

        end).

%% @doc Put additional fields priority and status message into presence
additions(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        Tags = escalus_stanza:tags([
            {<<"show">>, <<"dnd">>},
            {<<"priority">>, <<"1">>},
            {<<"status">>, <<"Short break">>}
        ]),
        Presence = escalus_stanza:presence_direct(bob, <<"available">>, Tags),
        escalus:send(Alice, Presence),

        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus:assert(is_presence_with_show, [<<"dnd">>], Received),
        escalus:assert(is_presence_with_status, [<<"Short break">>], Received),
        escalus:assert(is_presence_with_priority, [<<"1">>], Received)

        end).

negative_priority_presence(Config) ->
    escalus:story(Config, [2, 1], fun(Alice1, Alice2, Bob) ->

        %% Alice1 updates presense priority
        Tags = escalus_stanza:tags([
            {<<"priority">>, <<"-10">>}
        ]),
        Presence = escalus_stanza:presence(<<"available">>, Tags),
        escalus:send(Alice1, Presence),

        Received1 = escalus:wait_for_stanza(Alice1),
        Received2 = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_presence, Received1),
        escalus:assert(is_presence, Received2),
        escalus:assert(is_presence_with_priority, [<<"-10">>], Received1),
        escalus:assert(is_presence_with_priority, [<<"-10">>], Received2),

        %% Bob sends to the Alice's bare JID.
        escalus:send(Bob, escalus_stanza:chat_to_short_jid(Alice1, <<"Hi.">>)),

        %% Message is delivered to resource with maximum priority.
        Msg = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_chat_message, [<<"Hi.">>], Msg),

        %% If priority is negative, than the client does not want to receive
        %% any messages.
        timer:sleep(1000),

        %% Check Alice2 and Bob too
        escalus_assert:has_no_stanzas(Alice1),
        escalus_assert:has_no_stanzas(Alice2),
        escalus_assert:has_no_stanzas(Bob)

        end).

invisible_presence(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB1),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob becomes invisible
        escalus:send(Bob, escalus_stanza:presence(<<"invisible">>)),

        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice)),

        %% Return everything back
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice))

        end).

%% Alice sends "unavailable" from both devices
unavailable_multi(Config) ->
    escalus:story(Config, [2, 1], fun(Alice1, Alice2, Bob) ->
        %% First device becomes unavailable
        escalus:send(Alice1, escalus_stanza:presence(<<"unavailable">>)),
        %% Roster users and other devices receive a presence update
        P1 = escalus:wait_for_stanza(Alice2),
        P2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_stanza_from, [Alice1], P1),
        escalus:assert(is_stanza_from, [Alice1], P2),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], P1),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], P2),

        timer:sleep(1000),
        escalus_assert:has_no_stanzas(Alice1),
        escalus_assert:has_no_stanzas(Alice2),
        escalus_assert:has_no_stanzas(Bob),

        %% Second device also becomes unavailable
        escalus:send(Alice2, escalus_stanza:presence(<<"unavailable">>)),
        P3 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_stanza_from, [Alice2], P3),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], P3),

        timer:sleep(1000),
        escalus_assert:has_no_stanzas(Alice1),
        escalus_assert:has_no_stanzas(Alice2),
        escalus_assert:has_no_stanzas(Bob)

        end).

become_available_again(Config) ->
    escalus:story(Config, [2, 1], fun(Alice1, Alice2, Bob) ->
        %% PREPARATION
        %% Send unavailable, reproducing last state from unavailable_multi
        escalus:send(Alice1, escalus_stanza:presence(<<"unavailable">>)),
        escalus:wait_for_stanza(Alice2),
        escalus:send(Alice2, escalus_stanza:presence(<<"unavailable">>)),
        escalus:wait_for_stanza(Bob),
        escalus:wait_for_stanza(Bob),

        timer:sleep(1000),
        escalus_assert:has_no_stanzas(Alice1),
        escalus_assert:has_no_stanzas(Alice2),
        escalus_assert:has_no_stanzas(Bob),

        %% MAIN PART
        %% Becomes available again
        escalus:send(Alice1, escalus_stanza:presence(<<"available">>)),

        %% Alice receives her presence
        %% Alice receives roster presences (with delay element)
        %% The order of presences is random
        P1 = escalus:wait_for_stanza(Alice1),
        P2 = escalus:wait_for_stanza(Alice1),
        escalus:assert(is_stanza_from, [Alice1], P1),
        escalus:assert(is_presence_with_type, [<<"available">>], P1),
        escalus:assert(is_presence_with_type, [<<"available">>], P2),
        %% Check, than there are 2 stanzas: one from Alice1, another from Bob
        escalus:assert_many([is_stanza_from_fn(Alice1),
                             is_stanza_from_fn(Bob)], [P1, P2]),

        %% Bob receives Alice's presence update
        P3 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_stanza_from, [Alice1], P3),
        escalus:assert(is_presence_with_type, [<<"available">>], P3),

        timer:sleep(1000),
        escalus_assert:has_no_stanzas(Alice1),
        escalus_assert:has_no_stanzas(Alice2),
        escalus_assert:has_no_stanzas(Bob)
        end).

get_roster(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,_Bob) ->
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:is_roster_result(escalus:wait_for_stanza(Alice))

        end).

add_contact(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>], <<"Bobby">>),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received),

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [bob], Received2)

        end).

remove_contact(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [1], escalus:wait_for_stanza(Alice)),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [0], escalus:wait_for_stanza(Alice))

    end).


subscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB1),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))

        end).

subscribe_decline(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob refuses subscription
        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"unsubscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"unsubscribed">>)

    end).

subscribe_relog(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% New Bob resource connects, should receive subscription request again
        {ok, NewBob} = escalus_client:start_for(Config, bob, <<"newbob">>),
        escalus:send(NewBob,
            escalus_stanza:presence(<<"available">>)),

        escalus:assert(is_presence_with_type, [<<"available">>],
                       escalus:wait_for_stanza(Bob)),

        Stanzas = escalus:wait_for_stanzas(NewBob, 3),
        3 = length(Stanzas),

        escalus_new_assert:mix_match([
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(Bob, S)
                end,
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(NewBob, S)
                end,
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"subscribe">>, S)
                    andalso escalus_pred:is_stanza_from(alice, S)
                end
            ], Stanzas),

        escalus_client:stop(NewBob),

        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"unsubscribed">>))

        end).

unsubscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Alice sends unsubscribe
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"unsubscribe">>)),

        PushReqA2 = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReqA2),
        escalus:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe

        StanzasB = escalus:wait_for_stanzas(Bob, 2),

        check_subscription_stanzas(StanzasB, <<"unsubscribe">>),

        %% Alice receives unsubscribed
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))
    end).

remove_unsubscribe(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                            <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(alice, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = [escalus:wait_for_stanza(Alice),
                   escalus:wait_for_stanza(Alice)],

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),

        IsPresUnavailable =
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"unavailable">>, S)
                end,
        escalus:assert_many([is_roster_set, is_iq_result, IsPresUnavailable],
                            escalus:wait_for_stanzas(Alice, 3)),
        check_subscription_stanzas(escalus:wait_for_stanzas(Bob, 2),
                                   <<"unsubscribe">>)

    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
                                                          [<<"friends">>],
                                                          <<"Bobby">>)),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).

check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun(S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).

remove_roster(Config, UserSpec) ->
    [Username, Server, _Pass] = [escalus_ejabberd:unify_str_arg(Item) ||
                                 Item <- escalus_users:get_usp(Config, UserSpec)],
    Mods = escalus_ejabberd:rpc(gen_mod, loaded_modules, [Server]),
    case lists:member(mod_roster, Mods) of
        true ->
            {atomic, _} = escalus_ejabberd:rpc(mod_roster, remove_user, [Username, Server]);
        false ->
            case lists:member(mod_roster_odbc, Mods) of
                true ->
                    ok = escalus_ejabberd:rpc(mod_roster_odbc, remove_user, [Username, Server]);
                false ->
                    throw(roster_not_loaded)
            end
    end.

is_stanza_from_fn(From) ->
    fun(S) ->
        escalus_pred:is_stanza_from(From, S)
    end.
