%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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

-module(muc_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(MUC_HOST, <<"muc.localhost">>).
-define(MUC_CLIENT_HOST, <<"localhost/res1">>).
-define(PASSWORD, <<"password">>).
-define(SUBJECT, <<"subject">>).

-define(NS_MUC_REQUEST, <<"http://jabber.org/protocol/muc#request">>).
-define(NS_MUC_ROOMCONFIG, <<"http://jabber.org/protocol/muc#roomconfig">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          {group, disco},
          {group, moderator},
          {group, admin},
          {group, admin_membersonly},
          {group, occupant},
          {group, owner},
          {group, room_management}
         ].

groups() -> [
             {disco, [], [
                                  disco_service,
                                  disco_features,
                                  disco_rooms,
                                  disco_info,
                                  disco_items
                                 ]},
             {moderator, [], [
                                      moderator_subject,
                                      moderator_subject_unauthorized,
                                      moderator_kick,
                                      moderator_kick_with_reason,
                                      moderator_kick_unauthorized,
                                      moderator_voice,
                                      moderator_voice_with_reason,
                                      moderator_voice_unauthorized,
                                      moderator_voice_list,
                                      moderator_voice_approval,
                                      moderator_voice_forbidden,
                                      moderator_voice_not_occupant,
                                      moderator_voice_nonick
                                     ]},
             {admin, [], [
                                  admin_ban,
                                  admin_ban_with_reason,
                                  admin_ban_list,
                                  admin_invalid_affiliation,
                                  admin_invalid_jid,
                                  %% test should fail, temporarily changed
                                  admin_ban_higher_user,
                                  admin_membership,
                                  admin_membership_with_reason,
                                  admin_member_list,
                                  admin_moderator,
                                  admin_moderator_with_reason,
                                  admin_moderator_revoke_owner,
                                  admin_moderator_list,
                                  admin_invalid_role,
                                  admin_invalid_nick
                                 ]},
             {admin_membersonly, [], [
                                              admin_mo_revoke,
                                              admin_mo_invite,
                                              admin_mo_invite_with_reason,
                                              admin_mo_invite_mere
                                             ]},
             {occupant, [], [
%nick registration in a room is not implemented and will not be tested
                                     groupchat_user_enter,
                                     groupchat_user_enter_no_nickname,
                                     muc_user_enter,
                                     enter_non_anonymous_room,
                                     deny_access_to_password_protected_room,
                                     enter_password_protected_room,
                                     deny_accesss_to_memebers_only_room,
                                     deny_entry_to_a_banned_user,
                                     deny_entry_nick_conflict,
                                     enter_room_with_logging,
                                     deny_entry_user_limit_reached,
                                     send_history,
%                                     send_non_anonymous_history,
%                                     limit_history_chars,
%                                     limit_history_messages,
%                                     recent_history, %unfinished,
%                                     history_since,
%                                     no_history,
                                     subject,
                                     no_subject,
                                     send_to_all,
                                     send_and_receive_private_message,
                                     send_private_groupchat,
                                     change_nickname,
                                     deny_nickname_change_conflict,
                                     change_availability_status,
                                     mediated_invite,
                                     one2one_chat_to_muc,
                                     exit_room,
                                     exit_room_with_status
                                    ]},
             {owner, [], [
                                  room_creation_not_allowed,
                                  %% fails, see testcase
                                  cant_enter_locked_room,
                                  create_instant_room,
                                  destroy_locked_room,
                                  create_reserved_room,
                                  %% fails, see testcase
                                  reserved_room_cancel,
                                  reserved_room_unacceptable,
                                  reserved_room_configuration,
                                  owner_grant_revoke,
                                  owner_grant_revoke_with_reason,
                                  owner_list,
                                  owner_unauthorized,
                                  admin_grant_revoke,
                                  admin_grant_revoke_with_reason,
                                  admin_list,
                                  admin_unauthorized,
                                  destroy,
                                  destroy_unauthorized,
                                  config_denial,
                                  config_cancel,
                                  configure,
                                  configure_logging,
                                  %% fails, see testcase
                                  configure_anonymous
                                 ]},
             {room_management, [], [
                                            create_and_destroy_room
                                           ]}
            ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(moderator, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick,
        [{persistent, true}, {allow_change_subj, false}, {moderated, true},
         {members_by_default, false}]);

init_per_group(admin, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick, [{persistent, true}]);

init_per_group(admin_membersonly, Config) ->
    RoomName = <<"alicesroom">>,
    RoomNick = <<"alicesnick">>,
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, RoomName, RoomNick, [{persistent, true},
        {members_only, true}]);

init_per_group(disco, Config) ->
    Config1 = escalus:create_users(Config),
    [Alice | _] = ?config(escalus_users, Config1),
    start_room(Config1, Alice, <<"alicesroom">>, <<"aliceonchat">>,
        [{persistent, true}]);

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config).

end_per_group(moderator, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(admin, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(admin_membersonly, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(disco, Config) ->
    destroy_room(Config),
    escalus:delete_users(Config);

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName = configure_anonymous, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>,
        [{persistent, true}, {anonymous, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = configure_logging, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>,
        [{persistent, true}, {anonymous, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = configure, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = config_cancel, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = config_denial, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = destroy_unauthorized, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = destroy, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = admin_unauthorized, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = admin_list, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = admin_grant_revoke, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = admin_grant_revoke_with_reason, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = owner_unauthorized, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = owner_list, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = owner_grant_revoke, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = owner_grant_revoke_with_reason, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{persistent, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = groupchat_user_enter, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = groupchat_user_enter_no_nickname, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = muc_user_enter, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = enter_non_anonymous_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{anonymous, false}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = deny_access_to_password_protected_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    %{password_protected, Password}?
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{password_protected, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = enter_password_protected_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{password_protected, true}, {password, ?PASSWORD}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName = deny_accesss_to_memebers_only_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{members_only, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_to_a_banned_user, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_nick_conflict, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_entry_user_limit_reached, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{max_users, 1}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =enter_room_with_logging, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"aliceonchat">>, [{logging, true}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =send_history, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =send_non_anonymous_history, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
	Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, [{anonymous, false}]),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =limit_history_chars, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =limit_history_messages, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =recent_history, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =history_since, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =no_history, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =subject, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
	Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, [{subject, ?SUBJECT}]),
    escalus:init_per_testcase(CaseName, Config1);
										
init_per_testcase(CaseName =no_subject, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =send_to_all, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =send_and_receive_private_message, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =send_private_groupchat, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =change_nickname, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =deny_nickname_change_conflict, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =change_availability_status, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =mediated_invite, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

%init_per_testcase(CaseName =deny_entry_locked_room, Config) ->
%    escalus:init_per_testcase(CaseName, Config);

init_per_testcase(CaseName =registration_request, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =reserved_nickname_request, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =exit_room, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName =exit_room_with_status, Config) ->
    [Alice | _] = ?config(escalus_users, Config),
    Config1 = start_room(Config, Alice, <<"alicesroom">>, <<"alice">>, []),
    escalus:init_per_testcase(CaseName, Config1);

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName = configure_anonymous, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = configure_logging, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = configure, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = config_cancel, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = config_denial, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = destroy_unauthorized, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = destroy, Config) ->
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = admin_unauthorized, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = admin_list, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = admin_grant_revoke, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = admin_grant_revoke_with_reason, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = owner_unauthorized, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = owner_list, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = owner_grant_revoke, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = owner_grant_revoke_with_reason, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = groupchat_user_enter, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = groupchat_user_enter_no_nickname, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = muc_user_enter, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = deny_access_to_password_protected_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = enter_password_protected_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName = deny_accesss_to_memebers_only_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =deny_entry_to_a_banned_user, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =deny_entry_nick_conflict, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =deny_entry_user_limit_reached, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

%end_per_testcase(CaseName =deny_entry_locked_room, Config) ->
%    destroy_room(Config),
%    escalus:end_per_testcase(CaseName, Config),
%    timer:sleep(3000);

end_per_testcase(CaseName =enter_room_with_logging, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =send_and_receive_private_message, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =send_history, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =send_non_anonymous_history, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =limit_history_chars, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =limit_history_messages, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =recent_history, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =history_since, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =no_history, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);
										
end_per_testcase(CaseName =subject, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);
										
end_per_testcase(CaseName =no_subject, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);
										
end_per_testcase(CaseName =send_to_all, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =send_private_groupchat, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =change_nickname, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =deny_nickname_change_conflict, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =change_availability_status, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =mediated_invite, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =registration_request, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =reserved_nickname_request, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =exit_room, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName =exit_room_with_status, Config) ->
    destroy_room(Config),
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000);

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config),
    timer:sleep(3000).

%%--------------------------------------------------------------------
%%  Moderator use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#moderator
%%--------------------------------------------------------------------

%%  Examples 84-85
moderator_subject(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice sets room subject
        escalus:send(Alice,
            stanza_room_subject(?config(room,Config), <<"Lets have a chat!">>)),

        %% Alice receives subject change message
        Message = escalus:wait_for_stanza(Alice),
        true = is_subject_message(Message, <<"Lets have a chat!">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Message)
    end).

%%  Example 87
%%  This test doesn't fail anymore
%%  According to XEP error message should be from chatroom@service/nick,
%%  however ejabberd provides it from chatroom@service
moderator_subject_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice grants voice to Bob
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"participant">>}])),

        %% skip Bob's new presence
        escalus:wait_for_stanza(Bob),

        %% Bob tries to set the room subject
        escalus:send(Bob,
            stanza_room_subject(?config(room,Config), <<"Lets have a chat!">>)),

        %% Bob should receive an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Error)
    end).

%%  Examples 89-92
%%  Apparently user has to be in the room to kick someone, however XEP doesn't need that
moderator_kick(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice kicks Bob
        escalus:send(Alice, stanza_set_roles(
            ?config(room,Config), [{<<"bob">>,<<"none">>}])),

        %% Alice receives both iq result and Bob's unavailable presence
        Pred = fun(Stanza) ->
            is_unavailable_presence(Stanza, <<"307">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred],
          escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his presence
        escalus:assert(Pred, escalus:wait_for_stanza(Bob))
    end).

moderator_kick_with_reason(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice kicks Bob
        escalus:send(Alice, stanza_set_roles(
            ?config(room,Config), [{<<"bob">>,<<"none">>,<<"Some serious reason">>}])),

        %% Alice receives both iq result and Bob's unavailable presence
        Pred = fun(Stanza) ->
            has_reason(Stanza) andalso
            is_unavailable_presence(Stanza, <<"307">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred],
            escalus:wait_for_stanzas(Alice, 2)),
        %% Bob receives his presence
        Pres = escalus:wait_for_stanza(Bob),
        escalus:assert(Pred, Pres),
        true = has_reason(Pres)
    end).

%%  Example 93
moderator_kick_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob tries to kick Alice
        escalus:send(Bob, stanza_set_roles(
            ?config(room,Config), [{<<"alice">>,<<"none">>}])),

        %% Bob should get an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"cancel">>,<<"not-allowed">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

%%  Examples 94-101
moderator_voice(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice grants voice to Bob
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"participant">>}])),

        %% Alice receives success information and new Bob's presence
        Pred = fun(Stanza) ->
            is_presence_with_role(Stanza, <<"participant">>) andalso
            escalus_pred:is_stanza_from(
              room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        escalus:assert(Pred, escalus:wait_for_stanza(Bob)),

        %% Revoke Bob's voice
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"visitor">>}])),

        %% Alice receives success information and new Bob's presence
        Pred2 = fun(Stanza) ->
            is_presence_with_role(Stanza, <<"visitor">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred2],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        escalus:assert(Pred2, escalus:wait_for_stanza(Bob))
    end).

moderator_voice_with_reason(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Alice grants voice to Bob
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"participant">>,<<"Lets chat!">>}])),

        %% Alice receives success information and new Bob's presence
        Pred = fun(Stanza) ->
            has_reason(Stanza) andalso
            is_presence_with_role(Stanza, <<"participant">>) andalso
            escalus_pred:is_stanza_from(
              room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        Pres = escalus:wait_for_stanza(Bob),
        escalus:assert(Pred, Pres),
        true = has_reason(Pres),

        %% Revoke Bob's voice
        escalus:send(Alice, stanza_set_roles(?config(room,Config),
            [{<<"bob">>,<<"visitor">>,<<"No, let's not">>}])),

        %% Alice receives success information and new Bob's presence
        Pred2 = fun(Stanza) ->
            is_presence_with_role(Stanza, <<"visitor">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        escalus:assert_many([is_iq_result, Pred2],
            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob should receive his new presence
        Pres2 = escalus:wait_for_stanza(Bob),
        escalus:assert(Pred2, Pres2),
        true = has_reason(Pres2)
    end).
%%  Example 102, 107
moderator_voice_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob tries to revoke voice from Alice
        escalus:send(Bob, stanza_set_roles(?config(room,Config),
            [{<<"alice">>,<<"visitor">>}])),

        %% Bob should get an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

%%  Examples 103-106
%%  ejabberd behaves strange, responds that owner doesn't have moderator privileges
%%  if she isn't in the room
moderator_voice_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice requests voice list
        escalus:send(Alice,
          stanza_role_list_request(?config(room, Config), <<"participant">>)),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], List),
        %% List should be empty
        true = is_item_list_empty(List),

        %% Grant voice to Bob and Kate
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
              [{<<"bob">>, <<"participant">>}, {<<"kate">>,<<"participant">>}])),

        %% Alice receives confirmation and Bob's and Kate's new presences
        Preds = [fun(Stanza) ->
            is_presence_with_role(Stanza, <<"participant">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        fun(Stanza) ->
            is_presence_with_role(Stanza, <<"participant">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"kate">>), Stanza)
        end],
        escalus:assert_many([is_iq_result | Preds],
            escalus:wait_for_stanzas(Alice, 3)),

        %% Bob and Kates get their presences
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate, 2)),

        %% Alice requests voice list again
        escalus:send(Alice,
          stanza_role_list_request(?config(room, Config), <<"participant">>)),
        List2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List2),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], List2),
        %% Bob and Kate should be on it
        true = is_iq_with_jid(List2, Bob),
        true = is_iq_with_jid(List2, Kate),

        %% Revoke Bob's and Kate's voices
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
              [{<<"bob">>, <<"visitor">>}, {<<"kate">>,<<"visitor">>}])),

        %% Alice receives confirmation and Bob's and Kate's new presences
        Preds2 = [fun(Stanza) ->
            is_presence_with_role(Stanza, <<"visitor">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        fun(Stanza) ->
            is_presence_with_role(Stanza, <<"visitor">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"kate">>), Stanza)
        end],
        escalus:assert_many([is_iq_result | Preds2],
            escalus:wait_for_stanzas(Alice, 3)),

        %% Bob and Kates get their presences
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Bob, 2)),
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Kate, 2))
    end).

%%  This test doesn't fail anymore, moderator used to never get voice approval form
%%  This test works, but XEP doesn't specify what error messages should be here if something goes wrong...
%%  Examples 108-109
moderator_voice_approval(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob sends voice request
        escalus:send(Bob, stanza_voice_request_form(?config(room, Config))),

        %% Alice should get the request
        Form = escalus:wait_for_stanza(Alice),
        true = is_message_form(Form),

        Appr = stanza_voice_request_approval(?config(room, Config),
            escalus_utils:get_short_jid(Bob), <<"bob">>),
        escalus:send(Alice, Appr),

        %% Bob should get his new presence
        Pres = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Pres, <<"participant">>)

    end).

moderator_voice_forbidden(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob tries to send request approval
        Appr = stanza_voice_request_approval(?config(room, Config),
            escalus_utils:get_short_jid(Bob), <<"bob">>),
        escalus:send(Bob, Appr),

        %% Bob should get an error
        Err = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Err)
    end).

moderator_voice_not_occupant(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice tries to send request approval while she isn't in the room
        Appr = stanza_voice_request_approval(?config(room, Config),
            escalus_utils:get_short_jid(Bob), <<"bob">>),
        escalus:send(Alice, Appr),

        %% Alice should get an error
        Err = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Err)
    end).

moderator_voice_nonick(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presence
        escalus:wait_for_stanza(Alice),

        %% Bob sends voice request
        escalus:send(Bob, stanza_voice_request_form(?config(room, Config))),

        %% Alice should get the request
        Form = escalus:wait_for_stanza(Alice),
        true = is_message_form(Form),

        Appr = stanza_voice_request_approval_nonick(?config(room, Config),
            escalus_utils:get_short_jid(Bob)),
        escalus:send(Alice, Appr),

        %% Alice should get an error
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>],
            Error)

    end).
%%--------------------------------------------------------------------
%%  Admin use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#admin
%%--------------------------------------------------------------------

%%    Examples 110-114
admin_ban(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice bans Bob
        escalus:send(Alice, stanza_ban_user(Bob, ?config(room, Config))),
        
        %% Alice receives confirmation
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives outcast presence
        Outcast = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Outcast),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Outcast),
        true = is_presence_with_status_code(Outcast, <<"301">>),
        true = is_presence_with_affiliation(Outcast, <<"outcast">>),
        true = is_presence_with_role(Outcast, <<"none">>),

        %% Kate receives Bob's outcast presence
        BobOutcast = escalus:wait_for_stanza(Kate),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], BobOutcast),
        true = is_presence_with_affiliation(BobOutcast, <<"outcast">>),
        true = is_presence_with_role(BobOutcast, <<"none">>),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config), <<"bob">>)],
            BobOutcast)
        %% ejabberd doesn't send jid attribute in presence as in ex. 114
    end).

admin_ban_with_reason(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, _Bob, Kate) ->
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 2),

        %% Alice bans Kate
        escalus:send(Alice, stanza_ban_user(Kate, ?config(room, Config), <<"Be rational!">>)),

        %% Alice receives confirmation
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Kate receives outcast presence
        Outcast = escalus:wait_for_stanza(Kate),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Outcast),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"kate">>)], Outcast),
        true = is_presence_with_status_code(Outcast, <<"301">>),
        true = is_presence_with_affiliation(Outcast, <<"outcast">>),
        true = is_presence_with_role(Outcast, <<"none">>),
        true = has_reason(Outcast)

    end).

%%    Example 115
%%    Reponse 'from' field should be full JID, ejabberd provides chatroom JID
%%    However it's a bit strange as other cancel/not-allowed errors must be from the room JID
%%    Temporarily left room address check
admin_ban_higher_user(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Bob tries to ban Alice
        escalus:send(Bob, stanza_ban_user(Alice, ?config(room, Config))),

        %% Bob receives an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Error),
%%         escalus:assert(is_stanza_from,
%%             [escalus_utils:get_jid(Bob)], Error)
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Error)
    end).

%%    Examples 116-119
admin_ban_list(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice, Bob, Kate) ->
        %% Alice requests ban list
        escalus:send(Alice, stanza_ban_list_request(?config(room, Config))),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),

        %% Bob and Kate should be banned
        true = is_iq_with_affiliation(List, <<"outcast">>),
        true = is_iq_with_short_jid(List, Bob),
        true = is_iq_with_short_jid(List, Kate),

        %% Remove Bob's ban
        stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, []), ?config(room, Config)),
        Items = [{escalus_utils:get_short_jid(Bob),<<"none">>},
            {escalus_utils:get_short_jid(Kate), <<"none">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Request again
        escalus:send(Alice, stanza_ban_list_request(?config(room, Config))),
        List2 = escalus:wait_for_stanza(Alice),

        %% Noone should be banned
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))],
            List2),
        true = is_item_list_empty(List2)
    end).

admin_invalid_affiliation(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Alice tries to give invalid affiliation to Bob
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config),
            [{escalus_utils:get_short_jid(Bob), <<"some-random-affiliation">>}])),
        %% Alice receives error
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Error),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)

    end).

admin_invalid_jid(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Alice tries to give invalid affiliation to Bob
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config),
            [{<<"@mistyped-jid">>, <<"admin">>}])),
        %% Alice receives error
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Error),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)

    end).

%%  Examples 120-127
admin_membership(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice grants membership to Bob
        Items = [{escalus_utils:get_short_jid(Bob),<<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Alice revokes Bob's membership
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his loss of membership presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of membership presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)
    end).

admin_membership_with_reason(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice grants membership to Bob
        Items = [{escalus_utils:get_short_jid(Bob),<<"member">>,<<"Member?">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = has_reason(Bobs),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kates receives Bob's presence
        Kates = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),


        %% Alice revokes Bob's membership
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>, <<"Your call">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his loss of membership presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = has_reason(Bobs2),
        true = is_presence_with_affiliation(Bobs2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives his loss of membership presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates2),
        true = is_presence_with_affiliation(Kates2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).

%%  Examples 129-136
admin_member_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice requests member list
        escalus:send(Alice, stanza_affiliation_list_request(
            ?config(room, Config), <<"member">>)),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),

        %% List should be empty
        true = is_item_list_empty(List),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], List),

        %% Make Bob a member
        Items = [{escalus_utils:get_short_jid(Bob), <<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"member">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"member">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Request again
        escalus:send(Alice, stanza_affiliation_list_request(
              ?config(room, Config), <<"member">>)),
        List2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List2),

        %% Bob should be on the list
        true = is_iq_with_affiliation(List2, <<"member">>),
        true = is_iq_with_short_jid(List2, Bob),

        %% Revoke Bob's membership and make Kate a member
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>},
            {escalus_utils:get_short_jid(Kate), <<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room,Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob receives his and Kate's presence
        Preds = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"member">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"none">>)
            end
        ],
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),

        %% Kate receives her and Bob's presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate, 2))
  end).

%%  Examples 137-145
admin_moderator(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob moderator status
        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>,<<"moderator">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs, <<"moderator">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_role(Kates, <<"moderator">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke bob moderator status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
        end,

        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>, <<"participant">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his loss of moderator presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs2, <<"participant">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of moderator presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_role(Kates2, <<"participant">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).

admin_moderator_with_reason(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob moderator status
        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>,<<"moderator">>,<<"He should be fair">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs, <<"moderator">>),
        true = has_reason(Bobs),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates),
        true = is_presence_with_role(Kates, <<"moderator">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke bob moderator status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
        end,

        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>, <<"participant">>, <<"He wasn't">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his loss of moderator presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_role(Bobs2, <<"participant">>),
        true = has_reason(Bobs2),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of moderator presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_role(Kates2, <<"participant">>),
        true = has_reason(Kates2),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).
%%  Examples 145, 150
admin_moderator_revoke_owner(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),

        %% Alice tries to revoke moderator status from herself
        escalus:send(Alice, stanza_set_roles(
             ?config(room, Config), [{<<"alice">>, <<"participant">>}])),

        %% Should be an error
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

%%  Examples 146-150
admin_moderator_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Request moderator list
        escalus:send(Alice, stanza_role_list_request(
            ?config(room, Config), <<"moderator">>)),
        List = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List),
        %% Alice should be on it
        true = is_iq_with_jid(List, Alice),
        true = is_iq_with_role(List, <<"moderator">>),

        %% Grant Bob and Kate moderators role
        Preds = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"moderator">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"moderator">>)
            end
        ],
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
            [{<<"bob">>,<<"moderator">>},{<<"kate">>,<<"moderator">>}])),
        escalus:assert_many([is_iq_result | Preds], escalus:wait_for_stanzas(Alice,3)),

        %% Bob receives his and Kate's moderator presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob,2)),

        %% Kate receives her and Bob's moderator presence
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate,2)),

        %% Request again
        escalus:send(Alice, stanza_role_list_request(
            ?config(room, Config), <<"moderator">>)),
        List2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, List2),

        %% Alice, Bob and Kate should be on it
        true = is_iq_with_jid(List2, Alice),
        true = is_iq_with_jid(List2, Bob),
        true = is_iq_with_jid(List2, Kate),

        %% Revoke Bob's and Kate's roles
        Preds2 = [
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"kate">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
            end,
            fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_role(Stanza, <<"participant">>)
            end
        ],
        escalus:send(Alice, stanza_set_roles(?config(room, Config),
              [{<<"bob">>,<<"participant">>},{<<"kate">>,<<"participant">>}])),
        escalus:assert_many([is_iq_result|Preds2], escalus:wait_for_stanzas(Alice,3)),

        %% Bob receives his and Kate's participant presence
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Bob,2)),

        %% Kate receives her and Bob's participant presence
        escalus:assert_many(Preds2, escalus:wait_for_stanzas(Kate,2))
    end).

admin_invalid_role(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presences
        escalus:wait_for_stanza(Alice),

        %% Grant bob moderator status
        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"bob">>,<<"role-that-i-made-up">>}])),
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Error),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

admin_invalid_nick(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Skip Bob's presences
        escalus:wait_for_stanza(Alice),

        %% Grant bob moderator status
        escalus:send(Alice, stanza_set_roles(
            ?config(room, Config), [{<<"mistyped-nickname">>,<<"moderator">>}])),
        Error = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_error, Error),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

%%  Example 128
admin_mo_revoke(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Make Bob and Kate members
        Items = [{escalus_utils:get_short_jid(Bob), <<"member">>},
            {escalus_utils:get_short_jid(Kate), <<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob gets invitation
        is_invitation(escalus:wait_for_stanza(Bob)),
        %% Kate gets invitation
        is_invitation(escalus:wait_for_stanza(Kate)),

        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 3),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),

        %% Alice revokes Bob's membership
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Skip Bob's lost of membership presence (tested in the other case)
        escalus:wait_for_stanza(Bob),

        %% Kate receives Bob's loss of unavailable presence
        Kates = escalus:wait_for_stanza(Kate),
        true = is_membership_presence(Kates, <<"none">>, <<"none">>),
        true = is_unavailable_presence(Kates, <<"321">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config),<<"bob">>)], Kates)
    end).

%%  Example 134
%%  ejabberd doesn't send an invitation after adding user to a member list
admin_mo_invite(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Make Bob a member
        Items = [{escalus_utils:get_short_jid(Bob), <<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob should receive an invitation
        Inv = escalus:wait_for_stanza(Bob),
        is_invitation(Inv),
        escalus:assert(is_stanza_from, [room_address(?config(room,Config))], Inv),

        %% Alice revokes Bob's membership
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
    end).

admin_mo_invite_with_reason(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Make Bob a member
        Items = [{escalus_utils:get_short_jid(Bob), <<"member">>, <<"Just an invitation">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob should receive an invitation
        Inv = escalus:wait_for_stanza(Bob),
        is_invitation(Inv),
        true = invite_has_reason(Inv),
        escalus:assert(is_stanza_from, [room_address(?config(room,Config))], Inv),

        %% Alice revokes Bob's membership
        Items2 = [{escalus_utils:get_short_jid(Bob), <<"none">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room, Config), Items2)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))
    end).

%%  Example 135
%%  ejabberd returns cancel/not-allowed error while it should return auth/forbidden according to XEP
admin_mo_invite_mere(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Make Bob a member
        Items = [{escalus_utils:get_short_jid(Bob), <<"member">>}],
        escalus:send(Alice, stanza_set_affiliations(?config(room,Config), Items)),
        escalus:wait_for_stanza(Alice),

        %% Bob gets na invitation
        escalus:wait_for_stanza(Bob),

        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob tries to invite Kate
        escalus:send(Bob, stanza_mediated_invitation(?config(room,Config), Kate)),

        %% He should receive an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error)
    end).

%%--------------------------------------------------------------------
%%  Occupant use case tests
%%
%%  Tests the usecases described here :
%%  http://xmpp.org/extensions/xep-0045.html/#user
%%
%%  note: new user presence broadcast is unconfigurable and enabled for participants
%%  by default
%%
%%  convention - in this part of the suite user names are uses aas nicknames if a testcase does not require otherwise
%%  TO DO:
%%
%%  JID is not sent to participancts, use owner to throuhtly test some of the use cases
%%  is this behavious configurable?
%%--------------------------------------------------------------------
%Example 18
groupchat_user_enter(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice, Bob) ->
        Enter_room_stanza = stanza_groupchat_enter_room(?config(room, Config), escalus_utils:get_username(Bob)),
        escalus:send(Bob, Enter_room_stanza),
        Presence = escalus:wait_for_stanza(Bob),
        escalus_pred:is_presence(Presence),
		From = room_address(?config(room, Config), escalus_utils:get_username(Bob)), 
		From = exml_query:attr(Presence, <<"from">>)
	end).

%Example 19
%Fails - no error message sent from the server
groupchat_user_enter_no_nickname(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        escalus:send(Bob, stanza_groupchat_enter_room_no_nick(?config(room, Config))),
		escalus:assert(is_error, [<<"modify">>, <<"jid-malformed">>], escalus:wait_for_stanza(Bob)),
        escalus_assert:has_no_stanzas(Alice),   
        escalus_assert:has_no_stanzas(Bob)
    end).


% Examples 20, 21, 22
% Fails - no 110 status code in the self-presence messages
muc_user_enter(Config) ->
    escalus:story(Config, [1, 1, 1], fun(_Alice, Bob, Eve) ->
        %Bob enters the room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Eve))),
        Presence = escalus:wait_for_stanza(Bob),
		is_presence_with_affiliation(Presence, <<"none">>),
		is_self_presence(Bob, ?config(room, Config), Presence),
        escalus:wait_for_stanza(Bob),   %topic

        Presence2 = escalus:wait_for_stanza(Bob),
		is_presence_with_affiliation(Presence2, <<"none">>),
		is_presence_from(Eve, ?config(room, Config), Presence2),

        Presence3 = escalus:wait_for_stanza(Eve),
		is_presence_with_affiliation(Presence3, <<"none">>),
		is_presence_from(Bob, ?config(room, Config), Presence3),

        Presence4 = escalus:wait_for_stanza(Eve),
		is_presence_with_affiliation(Presence4, <<"none">>),
		is_self_presence(Eve, ?config(room, Config), Presence4),
        escalus:wait_for_stanza(Eve)   %topic
    end).

% Example 23 ,24
% No simple way to lock down the nicknames

% Example 25, 26
% fails - sends an additional message instead of including code 100 in the presence
enter_non_anonymous_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
		%should include code 100 in the presence messages to inform users alobut the room being non-annymous.
		%sends an additional simple message instead
        Presence = escalus:wait_for_stanza(Bob),
		is_self_presence(Bob, ?config(room, Config), Presence),
		has_status_codes(Presence, [<<"100">>]), 
        escalus:wait_for_stanza(Bob) %topic 
    end).

%TO DO:
% semi-anonymous rooms
% no option to turn a room into a semi-anonymous one
% (No examples, section 7.2.5)

%Example 27
deny_access_to_password_protected_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"auth">>, <<"not-authorized">>)
    end).

%Example 28
% Fails - no 110 status code in the self-presence messages
enter_password_protected_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_password_protected_room(?config(room, Config), escalus_utils:get_username(Bob), ?PASSWORD)),
        Presence = escalus:wait_for_stanza(Bob),
		is_self_presence(Bob, ?config(room, Config), Presence)
    end).

%Example 29
deny_accesss_to_memebers_only_room(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"auth">>, <<"registration-required">>)
    end).

%Example 30
deny_entry_to_a_banned_user(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,  Bob) ->
        %% Alice bans Bob
        escalus:send(Alice, stanza_ban_user(Bob, ?config(room, Config))),
        %% Alice receives confirmation
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"auth">>, <<"forbidden">>)
    end).

%Examlpe 31
deny_entry_nick_conflict(Config) -> 
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        Enter_room_stanza = stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob)), 
        escalus:send(Bob, Enter_room_stanza),
        escalus:wait_for_stanzas(Bob, 2),
        escalus:send(Eve, Enter_room_stanza),
        escalus_assert:is_error(escalus:wait_for_stanza(Eve), <<"cancel">>, <<"conflict">>)
    end).

%Example 32
%fails: wrong error type. should be: 'wait', received: 'cancel'
deny_entry_user_limit_reached(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,  Bob) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"wait">>, <<"service-unavailable">>)
    end).

%%Example 33 
%%requires creating a locked room in the init per testcase function somehow
%deny_entry_locked_room(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        escalus:send(Bob,stanza_muc_enter_room(<<"alicesroom">>, <<"aliceonchat">>)),
%        Message = escalus:wait_for_stanza(Bob),
%        error_logger:info_msg("Not a member error message: ~n~p~n", [Message]),
%        escalus_assert:is_error(Message, <<"cancal">>, <<"item-not-found">>)
%    end).

% Nonexistent rooms:
% If a user seeks to enter a non-existent room, servers behaviour is undefined.
% See the xep: http://xmpp.org/extensions/xep-0045.html/#enter-nonexistent
%

%Example 34
enter_room_with_logging(Config) ->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        %Bob enters the room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
		Presence = escalus:wait_for_stanza(Bob),
        is_self_presence(Bob, ?config(room, Config), Presence),
		has_status_codes(Presence, [<<"170">>])
    end).


%Example 35
send_history(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:wait_for_stanzas(Alice, 1),
		escalus:wait_for_stanzas(Bob, 1),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 1),
		escalus:wait_for_stanzas(Bob, 1),

		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), nick(Eve))),
		escalus:wait_for_stanza(Alice),	
		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
		is_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
		is_history_message_correct(?config(room, Config), nick(Bob),<<"groupchat">>,Msg2, escalus:wait_for_stanza(Eve)), 
		escalus:wait_for_stanza(Eve),	%topic
		escalus_assert:has_no_stanzas(Alice),
		escalus_assert:has_no_stanzas(Bob),
		escalus_assert:has_no_stanzas(Eve)
    end).


%Example 36
%Fails - sends an additional message instead of including code 100 in the presence
%also - From attribute in the delay element should contain the rooms address without the user name
%and the addresses element is not icluded (note: this feature is optional)
send_non_anonymous_history(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        		
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), nick(Eve))),
		escalus:wait_for_stanza(Alice),	
		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
		is_non_anonymous_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
		is_non_anonymous_history_message_correct(?config(room, Config), nick(Bob),<<"groupchat">>,Msg2, escalus:wait_for_stanza(Eve)), 
		escalus:wait_for_stanza(Eve),	%topic
		escalus_assert:has_no_stanzas(Alice),
		escalus_assert:has_no_stanzas(Bob),
		escalus_assert:has_no_stanzas(Eve)
    end).


%Example 37
%fails - the history setting is ignored
limit_history_chars(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

 		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room_history_setting(?config(room, Config), nick(Eve), <<"maxchars">>,<<"500">>)),
 		escalus:wait_for_stanza(Alice),	
 		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
 		is_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
 		escalus:wait_for_stanza(Eve),	%topic
 		escalus_assert:has_no_stanzas(Alice),
 		escalus_assert:has_no_stanzas(Bob),
 		escalus_assert:has_no_stanzas(Eve)
    end).

%Example 38
%fails - the history setting is ignored
limit_history_messages(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

 		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room_history_setting(?config(room, Config), nick(Eve), <<"maxstanzas">>,<<"1">>)),
 		escalus:wait_for_stanza(Alice),	
 		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
 		is_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
 		escalus:wait_for_stanza(Eve),	%topic
 		escalus_assert:has_no_stanzas(Alice),
 		escalus_assert:has_no_stanzas(Bob),
 		escalus_assert:has_no_stanzas(Eve)
    end).

%Example 39
%fails - the history setting is ignored
recent_history(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		timer:sleep(5000),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

 		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room_history_setting(?config(room, Config), nick(Eve), <<"seconds">>,<<"3">>)),
 		escalus:wait_for_stanza(Alice),	
 		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
 		is_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
 		escalus:wait_for_stanza(Eve),	%topic
 		escalus_assert:has_no_stanzas(Alice),
 		escalus_assert:has_no_stanzas(Bob),
 		escalus_assert:has_no_stanzas(Eve)
    end).

%Example 40
%should fail - unfinished, needs to be improved
history_since(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

 		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room_history_setting(?config(room, Config), nick(Eve), <<"since">>,<<"1970-01-01T00:00:00Z">>)),
 		escalus:wait_for_stanza(Alice),	
 		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
 		is_history_message_correct(?config(room, Config), nick(Alice),<<"groupchat">>,Msg, escalus:wait_for_stanza(Eve)), 
		is_history_message_correct(?config(room, Config), nick(Bob),<<"groupchat">>,Msg2, escalus:wait_for_stanza(Eve)), 
 		escalus:wait_for_stanza(Eve),	%topic
 		escalus_assert:has_no_stanzas(Alice),
 		escalus_assert:has_no_stanzas(Bob),
 		escalus_assert:has_no_stanzas(Eve)
    end).

%Example 41
%Fails - server should not send the history
no_history(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice , stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:wait_for_stanzas(Alice, 3),
        escalus:wait_for_stanzas(Bob, 3),
		Msg= <<"Hi, Bob!">>,
		Msg2= <<"Hi, Alice!">>,
		escalus:send(Alice,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
		escalus:send(Bob,escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg2)),
		escalus:wait_for_stanzas(Alice, 2),
		escalus:wait_for_stanzas(Bob, 2),

 		%Eve enters and receives the presences, the message history and finally the topic
        escalus:send(Eve, stanza_muc_enter_room_history_setting(?config(room, Config), nick(Eve), <<"maxchars">>,<<"0">>)),
 		escalus:wait_for_stanza(Alice),	
 		escalus:wait_for_stanza(Bob),	
        escalus:wait_for_stanzas(Eve, 3), %presences
 		escalus:wait_for_stanza(Eve),	%topic
		timer:wait(5000),
 		escalus_assert:has_no_stanzas(Alice),
 		escalus_assert:has_no_stanzas(Bob),
 		escalus_assert:has_no_stanzas(Eve)
    end).

%Example 42
subject(Config)->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
		escalus:wait_for_stanza(Bob),
		Subject = exml_query:path(escalus:wait_for_stanza(Bob), [{element, <<"subject">>}, cdata]),
		Subject==?SUBJECT
    end).

%Example 43
%Fails - the message should contain an empty subject element
no_subject(Config)->
    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
		escalus:wait_for_stanza(Bob),
		#xmlel{children = []} = exml_query:subelement(escalus:wait_for_stanza(Bob), <<"subject">>)
    end).

%Example 44, 45
send_to_all(Config) ->
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Eve))),
		escalus:wait_for_stanzas(Bob, 3),
		escalus:wait_for_stanzas(Eve, 3),

        Msg = <<"chat message">>,
        escalus:send(Eve, escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
        is_message_correct(?config(room, Config), escalus_utils:get_username(Eve), <<"groupchat">>, Msg, escalus:wait_for_stanza(Bob)),
        is_message_correct(?config(room, Config), escalus_utils:get_username(Eve), <<"groupchat">>, Msg, escalus:wait_for_stanza(Eve)),
        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Eve)
    end).


%Examples 46, 47
send_and_receive_private_message(Config) ->
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Eve))),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Eve, 3),
        		
        Msg = <<"chat message">>,
        ChatMessage = escalus_stanza:chat_to(room_address(?config(room, Config), escalus_utils:get_username(Eve)), Msg),
        escalus:send(Bob,ChatMessage),
        is_message_correct(?config(room, Config), escalus_utils:get_username(Bob), <<"chat">>, Msg, escalus:wait_for_stanza(Eve)),
        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Eve)
    end).

%Example 48
send_private_groupchat(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
		escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), nick(Eve))),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Eve, 3),

        Msg = <<"chat message">>,
        ChatMessage = escalus_stanza:groupchat_to(room_address(?config(room, Config), nick(Eve)), Msg),
        escalus:send(Bob,ChatMessage ),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"modify">>, <<"bad-request">>),

        escalus:send(Bob,escalus_stanza:chat_to(room_address(?config(room, Config), <<"non-existent">>), Msg)),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"cancel">>, <<"item-not-found">>),

        escalus:send(Alice,escalus_stanza:chat_to(room_address(?config(room, Config), nick(Bob)), Msg)),
        escalus_assert:is_error(escalus:wait_for_stanza(Alice), <<"modify">>, <<"not-acceptable">>),

        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Eve)
    end).

%Examples  49, 50
% Fails - no 110 status code 
change_nickname(Config) ->
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), <<"eve">>)),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Eve, 3),

        escalus:send(Bob, stanza_change_nick(?config(room, Config), <<"newbob">>)),
        Presence = escalus:wait_for_stanza(Bob),
		has_status_codes(Presence, [<<"110">>]),
        is_nick_unavailable_correct(?config(room, Config), <<"bob">>, <<"newbob">>, escalus:wait_for_stanza(Eve)),
        is_nick_unavailable_correct(?config(room, Config), <<"bob">>, <<"newbob">>, Presence),
        is_nick_update_correct(?config(room, Config), <<"newbob">>, escalus:wait_for_stanza(Eve)),
        Presence2 = escalus:wait_for_stanza(Bob),
        is_nick_update_correct(?config(room, Config), <<"newbob">>, Presence2),
		has_status_codes(Presence2, [<<"110">>])
    end).

%Example 51
%How to set up nickname change policy?

%Example 52
deny_nickname_change_conflict(Config) ->
    escalus:story(Config, [1, 1, 1], fun(_Alice,  Bob, Eve) ->
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), <<"eve">>)),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Eve, 3),
        escalus:send(Bob, stanza_change_nick(?config(room, Config), <<"eve">>)),
        escalus_assert:is_error(escalus:wait_for_stanza(Bob), <<"cancel">>, <<"conflict">>)
    end).

%Example 53
%how to lock down the roomnicks?

%Example 54, 55
%Full JID is not sent to participants, just to the owner. Assumess this to be the default configuration
change_availability_status(Config) ->
    escalus:story(Config, [1, 1], fun(Alice,  Bob) ->
		escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), nick(Bob))),
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Alice, 3),
        Status = <<"Bobs awesome new status">>,
        escalus:send(Bob, stanza_change_availability(Status, ?config(room, Config), nick(Bob))),
        is_availability_status_notification_correct(?config(room, Config), nick(Bob), Status,  escalus:wait_for_stanza(Bob)),
        Notification = escalus:wait_for_stanza(Alice),
        is_availability_status_notification_correct(?config(room, Config), nick(Bob), Status,  Notification),
        Jid = escalus_utils:get_jid(Bob),
        Jid = exml_query:path(Notification, [{element, <<"x">>}, {element, <<"item">>}, {attr, <<"jid">>}])
    end).


%Missing Direct Invitations (no examples)

%Example 56-59
mediated_invite(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), nick(Alice))),
        escalus:wait_for_stanzas(Alice, 2),
        escalus:send(Alice, stanza_mediated_invitation(?config(room, Config), Bob)),
        escalus:send(Alice, stanza_mediated_invitation(?config(room, Config), Eve)),
        %Bob ignores the invitation, Eve formally declines
		is_invitation(escalus:wait_for_stanza(Bob)),
		is_invitation(escalus:wait_for_stanza(Eve)),
		escalus:send(Eve, stanza_mediated_invitation_decline(?config(room, Config), Alice)),
		is_invitation_decline(escalus:wait_for_stanza(Alice))
    end).

%Example 60-65
%No <thread> tag, so right now this does not test any new functionality. The thread
%tag is recommended, but not required, so the test will not fail because of its absence
%Also - the examples contain neither configuration of the room not confirmation that it
%is supposed to be instant. Thit test assumes that an instant room should be created
one2one_chat_to_muc(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        Msg1 = escalus_stanza:chat_to(Bob,<<"Hi,Bob!">>),	
        Msg2 = escalus_stanza:chat_to(Alice,<<"Hi,Alice!">>),	
        escalus:send(Alice, Msg1),
        escalus:send(Bob, Msg2),
        escalus:wait_for_stanza(Alice),
        escalus:wait_for_stanza(Bob),

        %Alice creates a room
        Room = <<"alicesroom">>,
        escalus:send(Alice,stanza_muc_enter_room(Room, <<"alice">>)),
        was_room_created(escalus:wait_for_stanza(Alice)),

        R = escalus_stanza:setattr(stanza_instant_room(room_address(Room)),
                                   <<"from">>, escalus_utils:get_jid(Alice)),
        escalus:send(Alice, R),
        escalus:wait_for_stanza(Alice), %topic
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %Alice sends history to the room
        NewMsg1 = escalus_stanza:setattr(Msg1, <<"type">>, <<"groupchat">>),
        %print(stanza_to_room(NewMsg1,Room)),
        escalus:send(Alice, stanza_to_room(NewMsg1,Room)),
        NewMsg2 = escalus_stanza:setattr(Msg2, <<"type">>, <<"groupchat">>),
        escalus:send(Alice, stanza_to_room(NewMsg2,Room)),

        %Alice sends invitations
        %invitations should include contiue flag, but this makes no sense without the thread
        escalus:send(Alice, stanza_mediated_invitation(Room, Bob)),
        escalus:send(Alice, stanza_mediated_invitation(Room, Eve)),
        is_invitation(escalus:wait_for_stanza(Bob)),
        is_invitation(escalus:wait_for_stanza(Eve)),
        %Bob and Eve accept the invitations
        escalus:send(Bob, stanza_muc_enter_room(Room, <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2), %presences; bob receives the history before he receves Eves presence
        escalus:send(Eve, stanza_muc_enter_room(Room, <<"eve">>)),
        escalus:wait_for_stanzas(Eve, 3), %presences
        %Bob and Eve receive the history
        is_history_message_correct(Room, <<"alice">>,<<"groupchat">>,<<"Hi,Bob!">>, escalus:wait_for_stanza(Bob)),
        is_history_message_correct(Room, <<"alice">>,<<"groupchat">>,<<"Hi,Alice!">>, escalus:wait_for_stanza(Bob)),
        is_history_message_correct(Room, <<"alice">>,<<"groupchat">>,<<"Hi,Bob!">>, escalus:wait_for_stanza(Eve)),
        is_history_message_correct(Room, <<"alice">>,<<"groupchat">>,<<"Hi,Alice!">>, escalus:wait_for_stanza(Eve)),
        escalus:wait_for_stanzas(Alice, 2), %messages 
        escalus:wait_for_stanzas(Alice, 2), %presences
        escalus:wait_for_stanzas(Bob, 2),   %topic & Eves presence
        escalus:wait_for_stanzas(Eve, 1),   %topic
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Eve)
    end).


%%Examples 66-76
%%Registartion feature is not implemented
%%TODO: create a differend goruop for the registration test cases (they will fail)
%registration_request(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        escalus:send(Bob, stanza_to_room(escalus_stanza:iq_get(<<"jabber:iq:register">>, []), ?config(room, Config))),
%   	    print_next_message(Bob) 	
%    end).
%
%%Example 67
%registration_request_no_room(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        escalus:send(Bob, stanza_to_room(escalus_stanza:iq_get(<<"jabber:iq:register">>, []), <<"non-existent-room">>)),
%        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], escalus:wait_for_stanza(Bob))
%    end).
%
%stanza_reserved_nickname_request() ->
%     escalus_stanza:iq(<<"get">>, [#xmlel{
%        name = <<"query">>,
%        attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/disco#info">>}, {<<"node">>, <<"x-roomuser-item">>}],
%        children = []
%     }]).
%
%%Example 77-78
%%Not implemented - the 'node' element is ignored
%reserved_nickname_request(Config) ->
%    escalus:story(Config, [1, 1], fun(_Alice,  Bob) ->
%        %escalus:send(Bob, stanza_to_room(escalus_stanza:iq_get(<<"jabber:iq:register">>, []), ?config(room, Config))),
%  	    %print_next_message(Bob) 	
%        print(stanza_to_room(stanza_reserved_nickname_request(), ?config(room, Config))),
%        escalus:send(Bob, (stanza_to_room(stanza_reserved_nickname_request(), ?config(room, Config)))),
%        print_next_message(Bob)
%    end).

%Examlple 79
%Does not work - tested elsewhere (Examples 108 - 109)
%Unfinished - need to check if the form that shoudl be sent back (but isn't) is correct is correct


%Example 80-82
%No 110 status code in self-presence messages
%No Jid, not event when the owner leaves tehe room (normally the owner receives senders jid along with a message
exit_room(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Eve))),
		escalus:wait_for_stanzas(Alice, 4),
        escalus:wait_for_stanzas(Bob, 4),
        escalus:wait_for_stanzas(Eve, 4),
		escalus:send(Alice, stanza_to_room(escalus_stanza:presence(<<"unavailable">>), ?config(room, Config), escalus_utils:get_username(Alice))),
		Message = escalus:wait_for_stanza(Alice),
		has_status_codes(Message, [<<"110">>]),
		is_exit_message_correct(Alice, <<"owner">>, ?config(room, Config), Message),
		is_exit_message_correct(Alice, <<"owner">>, ?config(room, Config), escalus:wait_for_stanza(Bob)),
		is_exit_message_correct(Alice, <<"owner">>, ?config(room, Config), escalus:wait_for_stanza(Eve))
    end).



%Example 80-83
%No 110 status code in self-presence messages
exit_room_with_status(Config) ->
    escalus:story(Config, [1, 1, 1], fun(Alice,  Bob, Eve) ->
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Alice))),
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Bob))),
        escalus:send(Eve, stanza_muc_enter_room(?config(room, Config), escalus_utils:get_username(Eve))),
		escalus:wait_for_stanzas(Alice, 4),
        escalus:wait_for_stanzas(Bob, 4),
        escalus:wait_for_stanzas(Eve, 4),

		Status = <<"Alices exit status">>,
		StatusXml = #xmlel{name = <<"status">>, children = [#xmlcdata{content=Status}]},
		Presence = escalus_stanza:presence(<<"unavailable">>),
		Presence2 = Presence#xmlel{children=[StatusXml]},
		Stanza = stanza_to_room(Presence2,  ?config(room, Config), escalus_utils:get_username(Alice)),
		escalus:send(Alice, Stanza),
		Message = escalus:wait_for_stanza(Alice),
		has_status_codes(Message, [<<"110">>]),
		is_exit_message_with_status_correct(Alice, <<"owner">>, ?config(room, Config), Status,  Message),
		is_exit_message_with_status_correct(Alice, <<"owner">>, ?config(room, Config), Status, escalus:wait_for_stanza(Bob)),
		is_exit_message_with_status_correct(Alice, <<"owner">>, ?config(room, Config), Status, escalus:wait_for_stanza(Eve))
    end).

%%-------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

disco_service(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(has_service, [?MUC_HOST], Stanza),
        escalus:assert(is_stanza_from, [escalus_config:get_config(ejabberd_domain, Config)], Stanza)
    end).

disco_features(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_get_features()),
        Stanza = escalus:wait_for_stanza(Alice),
        has_features(Stanza),
        escalus:assert(is_stanza_from, [?MUC_HOST], Stanza)
    end).

disco_rooms(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_get_rooms()),
        %% we should have 1 room, created in init
        Stanza = escalus:wait_for_stanza(Alice),
        count_rooms(Stanza, 1),
        has_room(room_address(<<"alicesroom">>), Stanza),
        escalus:assert(is_stanza_from, [?MUC_HOST], Stanza)
    end).

disco_info(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus:send(Alice, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_INFO,[]), <<"alicesroom">>)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        has_feature(Stanza, <<"muc_persistent">>)
    end).

disco_items(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        escalus:send(Alice, stanza_join_room(<<"alicesroom">>, <<"nicenick">>)),
        _Stanza = escalus:wait_for_stanza(Alice),

        escalus:send(Bob, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_ITEMS,[]), <<"alicesroom">>)),
        Stanza2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_iq_result, Stanza2)
    end).

create_and_destroy_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Room1 = stanza_enter_room(<<"room1">>, <<"nick1">>),
        escalus:send(Alice, Room1),
        was_room_created(escalus:wait_for_stanza(Alice)),
        escalus:wait_for_stanza(Alice),

        DestroyRoom1 = stanza_destroy_room(<<"room1">>),
        escalus:send(Alice, DestroyRoom1),
        [Presence, Iq] = escalus:wait_for_stanzas(Alice, 2),
        was_room_destroyed(Iq),
        was_destroy_presented(Presence)
    end).

%% DOES NOT FAIL ANYMORE
%% Example 152. Service Informs User of Inability to Create a Room
%% As of writing this testcase (2012-07-24) it fails. Room is not created
%% as expected, but the returned error message is not the one specified by XEP.
%% ejabberd returns 'forbidden' while it ought to return 'not-allowed'.
room_creation_not_allowed(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        escalus_ejabberd:with_global_option({access,muc_create,global},
                                            [{deny,all}], fun() ->
            escalus:send(Alice, stanza_enter_room(<<"room1">>, <<"nick1">>)),
            escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                           escalus:wait_for_stanza(Alice))

        end)
    end).

%%  Fails.
cant_enter_locked_room(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->

        %% Create the room (should be locked on creation)
        escalus:send(Alice, stanza_muc_enter_room(<<"room1">>,
                                                  <<"alice-the-owner">>)),
		Presence = escalus:wait_for_stanza(Alice),
        was_room_created(Presence),

        %% Bob should not be able to join the room
        escalus:send(Bob, stanza_enter_room(<<"room1">>, <<"just-bob">>)),
        R = escalus:wait_for_stanza(Bob),
        %% sometime the predicate itself should be moved to escalus
        escalus:assert(fun ?MODULE:is_room_locked/1, R)
    end).

%% Example 155. Owner Requests Instant Room
create_instant_room(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->

        %% Create the room (should be locked on creation)
        Presence = stanza_muc_enter_room(<<"room1">>, <<"alice-the-owner">>),
        escalus:send(Alice, Presence),
        was_room_created(escalus:wait_for_stanza(Alice)),

        escalus:wait_for_stanza(Alice), % topic

        R = escalus_stanza:setattr(stanza_instant_room(<<"room1@muc.localhost">>),
                                   <<"from">>, escalus_utils:get_jid(Alice)),
        escalus:send(Alice, R),
        IQ = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, IQ),

        %% Bob should be able to join the room
        escalus:send(Bob, stanza_muc_enter_room(<<"room1">>, <<"bob">>)),
        escalus:wait_for_stanza(Alice), %Bobs presence
        %% Bob should receive (in that order): Alices presence, his presence and the topic

        Preds = [fun(Stanza) -> escalus_pred:is_presence(Stanza) andalso
            escalus_pred:is_stanza_from(<<"room1@muc.localhost/bob">>, Stanza)
        end,
        fun(Stanza) -> escalus_pred:is_presence(Stanza) andalso
            escalus_pred:is_stanza_from(<<"room1@muc.localhost/alice-the-owner">>, Stanza)
        end],
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),
        escalus:wait_for_stanza(Bob), %topic
        escalus_assert:has_no_stanzas(Bob),
        escalus_assert:has_no_stanzas(Alice)
    end).

destroy_locked_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        Room1 = stanza_muc_enter_room(<<"room1">>, <<"nick1">>),
        escalus:send(Alice, Room1),
        was_room_created(escalus:wait_for_stanza(Alice)),
        escalus:wait_for_stanza(Alice),

        DestroyRoom1 = stanza_destroy_room(<<"room1">>),
        escalus:send(Alice, DestroyRoom1),
        [Presence, Iq] = escalus:wait_for_stanzas(Alice, 2),
        was_room_destroyed(Iq),
        was_destroy_presented(Presence)
    end).

%%  Example 156
create_reserved_room(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Create the room (should be locked on creation)
        escalus:send(Alice, stanza_muc_enter_room(<<"room2">>,
                                                  <<"alice-the-owner">>)),
        was_room_created(escalus:wait_for_stanza(Alice)),

        escalus:wait_for_stanza(Alice),

        R = escalus_stanza:setattr(stanza_reserved_room(<<"room2@muc.localhost">>),
                                   <<"from">>, escalus_utils:get_jid(Alice)),
        escalus:send(Alice, R),
        S = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, S),
        true = is_form(S)

    end).

%%  Example 162
%%  This test fails, room should be destroyed after sending cancel message
reserved_room_cancel(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Do the disco, we should have no rooms
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice), 0),

        %% Create the room (should be locked on creation)
        escalus:send(Alice, stanza_muc_enter_room(<<"room3">>,
                                                  <<"alice-the-owner">>)),
        was_room_created(escalus:wait_for_stanza(Alice)),

        escalus:wait_for_stanza(Alice),

        R = escalus_stanza:setattr(stanza_reserved_room(<<"room3@muc.localhost">>),
                                   <<"from">>, escalus_utils:get_jid(Alice)),
        escalus:send(Alice, R),
        S = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, S),
        true = is_form(S),

        %% Send cancel request
        escalus:send(Alice, stanza_cancel(<<"room3">>)),

        %% Alice must receive presence
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
            escalus:wait_for_stanza(Alice)),

        %% Room should have been destroyed
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice), 0)
    end).

%%  Example 161
reserved_room_unacceptable(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Create the room (should be locked on creation)
        escalus:send(Alice, stanza_muc_enter_room(<<"room4">>,
                                                  <<"alice-the-owner">>)),
        was_room_created(escalus:wait_for_stanza(Alice)),

        escalus:wait_for_stanza(Alice),
        escalus:send(Alice, stanza_reserved_room(<<"room4@muc.localhost">>)),
        S = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, S),
        true = is_form(S),

        %% Configure room to be password protected, with empty secret
        Form = stanza_configuration_form(<<"room4">>, [
            {<<"muc#roomconfig_passwordprotectedroom">>, <<"1">>, <<"boolean">>},
            {<<"muc#roomconfig_roomsecret">>, <<>>, <<"text-single">>}]),
        escalus:send(Alice, Form),

        R = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], R),
        escalus:assert(is_stanza_from, [room_address(<<"room4">>)], R)

    end).

%%  Example 159
%%  Mysterious thing: when room is named "room5", creation confirmation doesn't return status code
reserved_room_configuration(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Create the room (should be locked on creation)
        escalus:send(Alice, stanza_muc_enter_room(<<"roomfive">>,
                                                  <<"alice-the-owner">>)),
        was_room_created(escalus:wait_for_stanza(Alice)),

        escalus:wait_for_stanza(Alice),
        escalus:send(Alice, stanza_reserved_room(<<"roomfive@muc.localhost">>)),
        S = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, S),
        true = is_form(S),

        %% Configure room to be moderated, public and persistent
        Form = stanza_configuration_form(<<"roomfive">>, [
            {<<"muc#roomconfig_publicroom">>, <<"1">>, <<"boolean">>},
            {<<"muc#roomconfig_moderatedroom">>, <<"1">>, <<"boolean">>},
            {<<"muc#roomconfig_persistentroom">>, <<"1">>, <<"boolean">>}]),
        escalus:send(Alice, Form),

        Result = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result),
        escalus:assert(is_stanza_from, [<<"roomfive@muc.localhost">>], Result),

        %% Check if it worked
        escalus:send(Alice, stanza_to_room(escalus_stanza:iq_get(?NS_DISCO_INFO,[]), <<"roomfive">>)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        has_feature(Stanza, <<"muc_persistent">>),
        has_feature(Stanza, <<"muc_moderated">>),
        has_feature(Stanza, <<"muc_public">>),

        %% Destroy the room to clean up
        escalus:send(Alice, stanza_destroy_room(<<"roomfive">>)),
        escalus:wait_for_stanzas(Alice, 2)
    end).

%%  Example 164
%%  This test doesn't fail anymore
%%  ejabberd used to return error with no type
config_denial(Config) ->
    escalus:story(Config, [1,1], fun(_Alice, Bob) ->
        %% Bob requests configuration form
        escalus:send(Bob, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Bob should get an error
        Res = escalus:wait_for_stanza(Bob),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Res),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res)
    end).

%%  Example 166
config_cancel(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice requests configuration form
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res = escalus:wait_for_stanza(Alice),
        true = is_form(Res),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res),

        %% Alice cancels form
        escalus:send(Alice, stanza_cancel(?config(room, Config))),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice))

    end).

%%  Ejabberd doesn't let set admins nor owners in configuration form so testcases for ex. 167-170 would be useless

configure(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice requests configuration form
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res = escalus:wait_for_stanza(Alice),
        true = is_form(Res),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res),

        %% Configure room to be moderated, public and persistent
        Form = stanza_configuration_form(?config(room, Config), [
            {<<"muc#roomconfig_publicroom">>, <<"1">>, <<"boolean">>},
            {<<"muc#roomconfig_moderatedroom">>, <<"1">>, <<"boolean">>},
            {<<"muc#roomconfig_persistentroom">>, <<"1">>, <<"boolean">>}]),
        escalus:send(Alice, Form),

        Result = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Result),

        %% Check if it worked
        escalus:send(Alice, stanza_to_room(escalus_stanza:iq_get(
            ?NS_DISCO_INFO,[]), ?config(room, Config))),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Stanza),
        has_feature(Stanza, <<"muc_persistent">>),
        has_feature(Stanza, <<"muc_moderated">>),
        has_feature(Stanza, <<"muc_public">>)
    end).

%%  Example 171
%%  This test needs enabled mod_muc_log module and {access_log, muc_create} in options
%%  This test fails, ejabberd doesn't seem to send status code when room privacy changes
configure_logging(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Bob, 3),
        escalus:wait_for_stanzas(Kate, 3),

        %% Alice requests configuration form
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res = escalus:wait_for_stanza(Alice),
        true = is_form(Res),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res),

         %% Configure room with logging enabled
        Form = stanza_configuration_form(?config(room, Config), [
            {<<"muc#roomconfig_enablelogging">>, <<"1">>, <<"boolean">>}]),
        escalus:send(Alice, Form),

        Result = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Result),

        Res2 = escalus:wait_for_stanza(Bob),
        true = is_message_with_status_code(Res2, <<"170">>),
        true = is_groupchat_message(Res2),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res2),
        
        escalus:wait_for_stanza(Kate),

        %% Alice requests configuration form again
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res3 = escalus:wait_for_stanza(Alice),
        true = is_form(Res3),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res3),
        
        %% Simple message exchange
        Msg = <<"chat message">>,
        escalus:send(Bob, escalus_stanza:groupchat_to(room_address(?config(room, Config)), Msg)),
        is_message_correct(?config(room, Config), escalus_utils:get_username(Bob), <<"groupchat">>, Msg, escalus:wait_for_stanza(Bob)),
        is_message_correct(?config(room, Config), escalus_utils:get_username(Bob), <<"groupchat">>, Msg, escalus:wait_for_stanza(Kate)),

        %% Disable logging
        Form2 = stanza_configuration_form(?config(room, Config), [
            {<<"muc#roomconfig_enablelogging">>, <<"0">>, <<"boolean">>}]),
        escalus:send(Alice, Form2),

        Result2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result2),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Result2),

        Res4 = escalus:wait_for_stanza(Bob),
        true = is_message_with_status_code(Res4, <<"171">>),
        true = is_groupchat_message(Res4),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res4),
        
        escalus:wait_for_stanza(Kate)
    end).

%%  Example 171
%%  This test fails, ejabberd apparently doesn't send room status update after privacy-related update
configure_anonymous(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Alice requests configuration form
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res = escalus:wait_for_stanza(Alice),
        true = is_form(Res),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res),


        %% Configure room as non-anonymous
        Form = stanza_configuration_form(?config(room, Config), [
            {<<"muc#roomconfig_whois">>, <<"anyone">>, <<"list-single">>}]),
        escalus:send(Alice, Form),

        Result = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Result),

        Res2 = escalus:wait_for_stanza(Bob),
        true = is_message_with_status_code(Res2, <<"172">>),
        true = is_groupchat_message(Res2),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res2),

        %% Alice requests configuration form again
        escalus:send(Alice, stanza_to_room(
            escalus_stanza:iq_get(?NS_MUC_OWNER,[]), ?config(room, Config))),

        %% Alice receives form
        Res3 = escalus:wait_for_stanza(Alice),
        true = is_form(Res3),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res3),

        %% Configure room as semi-anonymous
        Form2 = stanza_configuration_form(?config(room, Config), [
            {<<"muc#roomconfig_whois">>, <<"moderators">>, <<"list-single">>}]),
        escalus:send(Alice, Form2),

        Result2 = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Result2),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config))], Result2),

        Res4 = escalus:wait_for_stanza(Bob),
        true = is_message_with_status_code(Res4, <<"173">>),
        true = is_groupchat_message(Res4),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Res4)
    end).

%%  Examples 172-180
owner_grant_revoke(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob owner status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob),<<"owner">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"owner">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"owner">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke alice owner status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"alice">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"admin">>)
        end,

        escalus:send(Bob, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Alice), <<"admin">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Bob, 2)),

        %% Alice receives her loss of ownership presence
        Alices = escalus:wait_for_stanza(Alice),
        true = is_presence_with_affiliation(Alices, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Alices),

        %% Kate receives Alice's loss of ownership presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates2, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Kates2)

    end).

owner_grant_revoke_with_reason(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob owner status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob),<<"owner">>,<<"I trust him">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = has_reason(Bobs),
        true = is_presence_with_affiliation(Bobs, <<"owner">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates),
        true = is_presence_with_affiliation(Kates, <<"owner">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke alice owner status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"alice">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"admin">>)
        end,

        escalus:send(Bob, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Alice),
                    <<"admin">>, <<"Assassination!">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Bob, 2)),

        %% Alice receives her loss of ownership presence
        Alices = escalus:wait_for_stanza(Alice),
        true = has_reason(Alices),
        true = is_presence_with_affiliation(Alices, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Alices),

        %% Kate receives Alice's loss of ownership presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates2),
        true = is_presence_with_affiliation(Kates2, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"alice">>)], Kates2)

    end).
%%  Examples 181-185
%%  Behaves strange when we try to revoke the only owner together with
%%  granting someone else
owner_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice requests owner list
        escalus:send(Alice, stanza_affiliation_list_request(
            ?config(room, Config), <<"owner">>)),
        List = escalus:wait_for_stanza(Alice),

        %% Alice should be on it
        escalus:assert(is_iq_result, List),
        true = is_iq_with_affiliation(List, <<"owner">>),
        true = is_iq_with_short_jid(List, Alice),

        %% Grant Bob and Kate owners status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Kate),<<"owner">>},
                 {escalus_utils:get_short_jid(Bob), <<"owner">>}])),
        escalus:assert_many([is_iq_result, is_presence, is_presence],
            escalus:wait_for_stanzas(Alice, 3)),

        %% Bob receives his and Kate's notice
        Preds = [fun(Stanza) ->
            is_presence_with_affiliation(Stanza, <<"owner">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        fun(Stanza) ->
            is_presence_with_affiliation(Stanza, <<"owner">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"kate">>), Stanza)
        end],
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),

        %% Kate receives her and Bob's notice
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate, 2))
    end).

%%  Example 184
%%  Test doesn't fail anymore, ejabberd used to return cancel/not-allowed error while it should
%%  return auth/forbidden according to XEP
owner_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(_Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob tries to modify owner list
        escalus:send(Bob, stanza_set_affiliations(
            ?config(room, Config),
            [{escalus_utils:get_short_jid(Bob), <<"owner">>}])),
        %% Should get an error
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>],
            escalus:wait_for_stanza(Bob))

    end).

%%  Examples 186-195
admin_grant_revoke(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob owner status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob),<<"admin">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs, <<"admin">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke Bob admin status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"none">>)
        end,

        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob), <<"none">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his loss of admin presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = is_presence_with_affiliation(Bobs2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of admin presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = is_presence_with_affiliation(Kates2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).

admin_grant_revoke_with_reason(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Grant bob admin status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob),<<"admin">>,<<"He should be helpful">>}])),
        escalus:assert_many([is_iq_result, is_presence], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his notice
        Bobs = escalus:wait_for_stanza(Bob),
        true = has_reason(Bobs),
        true = is_presence_with_affiliation(Bobs, <<"admin">>),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Bobs),

        %% Kate receives Bob's notice
        Kates = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates),
        true = is_presence_with_affiliation(Kates, <<"admin">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room, Config), <<"bob">>)], Kates),

        %% Revoke Bob admin status
        Pred = fun(Stanza) ->
                escalus_pred:is_stanza_from(
                  room_address(?config(room, Config), <<"bob">>),Stanza) andalso
                is_presence_with_affiliation(Stanza, <<"none">>)
        end,

        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Bob),
                    <<"none">>, <<"Well, he wasn't">>}])),
        escalus:assert_many([is_iq_result, Pred], escalus:wait_for_stanzas(Alice, 2)),

        %% Bob receives his loss of admin presence
        Bobs2 = escalus:wait_for_stanza(Bob),
        true = has_reason(Bobs2),
        true = is_presence_with_affiliation(Bobs2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Bobs2),

        %% Kate receives Bob's loss of admin presence
        Kates2 = escalus:wait_for_stanza(Kate),
        true = has_reason(Kates2),
        true = is_presence_with_affiliation(Kates2, <<"none">>),
        escalus:assert(is_stanza_from,
            [room_address(?config(room,Config), <<"bob">>)], Kates2)

    end).
%%  Examples 196-200
admin_list(Config) ->
    escalus:story(Config, [1,1,1], fun(Alice, Bob, Kate) ->
        %% Alice joins room
        escalus:send(Alice, stanza_muc_enter_room(?config(room, Config), <<"alice">>)),
        escalus:wait_for_stanzas(Alice, 2),
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 3),
        %% Kate joins room
        escalus:send(Kate, stanza_muc_enter_room(?config(room, Config), <<"kate">>)),
        escalus:wait_for_stanzas(Kate, 4),
        %% Skip Kate's presence
        escalus:wait_for_stanza(Bob),
        %% Skip Kate's and Bob's presences
        escalus:wait_for_stanzas(Alice, 3),

        %% Alice requests owner list
        escalus:send(Alice, stanza_affiliation_list_request(
            ?config(room, Config), <<"admin">>)),
        List = escalus:wait_for_stanza(Alice),
        %% Noone should be on it
        true = is_item_list_empty(List),

        %% Grant Bob and Kate admins status
        escalus:send(Alice, stanza_set_affiliations(
            ?config(room, Config),
                [{escalus_utils:get_short_jid(Kate),<<"admin">>},
                 {escalus_utils:get_short_jid(Bob), <<"admin">>}])),
        escalus:assert_many([is_iq_result, is_presence, is_presence],
            escalus:wait_for_stanzas(Alice, 3)),

        %% Bob receives his and Kate's notice
        Preds = [fun(Stanza) ->
            is_presence_with_affiliation(Stanza, <<"admin">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"bob">>), Stanza)
        end,
        fun(Stanza) ->
            is_presence_with_affiliation(Stanza, <<"admin">>) andalso
            escalus_pred:is_stanza_from(
                room_address(?config(room, Config), <<"kate">>), Stanza)
        end],
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Bob, 2)),

        %% Kate receives her and Bob's notice
        escalus:assert_many(Preds, escalus:wait_for_stanzas(Kate, 2))
    end).

%%  Example 199
%%  Test does not fail anymoure, ejabberd used to return cancel/not-allowed error while it should
%%  return auth/forbidden according to XEP
admin_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(_Alice, Bob) ->
        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Bob tries to modify admin list
        escalus:send(Bob, stanza_set_affiliations(
            ?config(room, Config),
            [{escalus_utils:get_short_jid(Bob), <<"admin">>}])),
        Error = escalus:wait_for_stanza(Bob),
        %% Should get an error
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>],
            Error)

    end).

%%  Examples 201-203
destroy(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Run disco, we should have 1 room
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice),1),

        %% Bob joins room
        escalus:send(Bob, stanza_muc_enter_room(?config(room, Config), <<"bob">>)),
        escalus:wait_for_stanzas(Bob, 2),

        %% Alice requests room destruction
        escalus:send(Alice, stanza_destroy_room(?config(room, Config))),

        %% Alice gets confirmation
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        %% Bob gets unavailable presence
        Presence = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"unavailable">>], Presence),
        escalus:assert(is_stanza_from,
          [room_address(?config(room, Config), <<"bob">>)], Presence),


        %% Run disco again, we should have no rooms
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice),0)
    end).

%%  Example 204
%%  Test doesn't fail anymore
%%  Ejabberd used to return forbidden error without a type attribute
destroy_unauthorized(Config) ->
    escalus:story(Config, [1,1], fun(Alice, Bob) ->
        %% Run disco, we should have 1 room
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice),1),

        %% Bob tries to destroy Alice's room
        escalus:send(Bob, stanza_destroy_room(?config(room, Config))),

        %% Bob gets an error
        Error = escalus:wait_for_stanza(Bob),
        escalus:assert(is_stanza_from, [room_address(?config(room, Config))], Error),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Error),

        %% Run disco again, we still should have 1 room
        escalus:send(Alice, stanza_get_rooms()),
        count_rooms(escalus:wait_for_stanza(Alice),1)
    end).
%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

nick(User) -> escalus_utils:get_username(User).

is_history_message_correct(Room, SenderNick,Type,  Text, ReceivedMessage) ->
    %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_message(ReceivedMessage),
	exml_query:path(ReceivedMessage, [{element, <<"delay">>}, {attr, <<"stamp">>}]),
	FromDelay = room_address(Room),
	FromDelay = exml_query:path(ReceivedMessage, [{element, <<"delay">>}, {attr, <<"from">>}]),
    From = room_address(Room, SenderNick),
    From = exml_query:attr(ReceivedMessage, <<"from">>),
    Type = exml_query:attr(ReceivedMessage, <<"type">>),
	Content = exml_query:path(ReceivedMessage, [{element, <<"body">>}, cdata]),
	Text = Content.

is_non_anonymous_history_message_correct(Room, SenderNick,Type,  Text, ReceivedMessage) ->
    %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_message(ReceivedMessage),
	exml_query:path(ReceivedMessage, [{element, <<"delay">>}, {attr, <<"stamp">>}]),
	FromDelay = room_address(Room),
	FromDelay = exml_query:path(ReceivedMessage, [{element, <<"delay">>}, {attr, <<"from">>}]),
    From = room_address(Room, SenderNick),
    From = exml_query:attr(ReceivedMessage, <<"from">>),
    Type = exml_query:attr(ReceivedMessage, <<"type">>),
	Content = exml_query:path(ReceivedMessage, [{element, <<"body">>}, cdata]),
	Text = Content,
	<<"http://jabber.org/protocol/address">> = exml:path(ReceivedMessage, [{element, <<"addresses">>}, {attr, <<"xmlns">>}]),
	<<"oform">> = exml:path(ReceivedMessage, [{element, <<"addresses">>},{element, <<"address">>}, {attr, <<"type">>}]),
	JID = escalus_utils:get_jid(SenderNick),
	JID= exml:path(ReceivedMessage, [{element, <<"addresses">>},{element, <<"address">>}, {attr, <<"jid">>}]).

is_self_presence(User, Room, Presence) ->
		has_status_codes(Presence, [<<"110">>]),
        escalus_pred:is_presence(Presence),
		From = room_address(Room, escalus_utils:get_username(User)),
        From = exml_query:attr(Presence, <<"from">>).

is_presence_from(User, Room, Presence) ->
        escalus_pred:is_presence(Presence),
		From = room_address(Room, escalus_utils:get_username(User)),
        From = exml_query:attr(Presence, <<"from">>).


%does not check the jid - the user might not be entitled to receive it.
is_availability_status_notification_correct(Room, SenderNick, NewStatus, ReceivedMessage) ->
    %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_presence(ReceivedMessage),
    From = room_address(Room, SenderNick),
    From  = exml_query:attr(ReceivedMessage, <<"from">>),
    NewStatus =  exml_query:path(ReceivedMessage, [{element, <<"status">>}, cdata]),
    <<"xa">> = exml_query:path(ReceivedMessage, [{element, <<"show">>}, cdata]).

is_item_list_empty(#xmlel{children = [Query]}) ->
    Query#xmlel.children == [].

is_message_correct(Room, SenderNick, Type, Text, ReceivedMessage) ->
    %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_message(ReceivedMessage),
    From = room_address(Room, SenderNick),
    From  = exml_query:attr(ReceivedMessage, <<"from">>),
    Type  = exml_query:attr(ReceivedMessage, <<"type">>),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content=Text}]},
    Body = exml_query:subelement(ReceivedMessage, <<"body">>).

is_exit_message_correct(LeavingUser,Affiliation,Room, Message) ->
	escalus_pred:is_presence_with_type(<<"unavailable">>,Message),
	is_presence_with_affiliation(Message,Affiliation), 
    From = room_address(Room, escalus_utils:get_username(LeavingUser)),
    From  = exml_query:attr(Message, <<"from">>).

is_exit_message_with_status_correct(LeavingUser,Affiliation,Room,Status,  Message) ->
	escalus_pred:is_presence_with_type(<<"unavailable">>,Message),
	is_presence_with_affiliation(Message,Affiliation), 
    From = room_address(Room, escalus_utils:get_username(LeavingUser)),
    From  = exml_query:attr(Message, <<"from">>),
	Status = exml_query:path(Message, [{element,<<"status">>}, cdata]).

is_nick_unavailable_correct(Room, OldNick, NewNick, ReceivedMessage) ->
     %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_message(ReceivedMessage),
    From = room_address(Room, OldNick),
    From = exml_query:attr(ReceivedMessage, <<"from">>),
    has_status_codes(ReceivedMessage, [<<"303">>]),
    NewNick = exml_query:path(ReceivedMessage, [{element, <<"x">>}, {element, <<"item">>},{attr, <<"nick">>}]).

is_nick_update_correct(Room,NewNick, ReceivedMessage) ->
    %error_logger:info_msg("tested message: ~n~p~n", [ReceivedMessage]),
    escalus_pred:is_message(ReceivedMessage),
    From = room_address(Room,NewNick),
    From  = exml_query:attr(ReceivedMessage, <<"from">>).

print_next_message(User) ->
    error_logger:info_msg("~p messaege, ~n~p~n", [User, escalus:wait_for_stanza(User)]).

print(Element) ->
    error_logger:info_msg("~n~p~n", [Element]).

generate_rpc_jid({_,User}) ->
    {username, Username} = lists:keyfind(username, 1, User),
    {server, Server} = lists:keyfind(server, 1, User),
    %% esl-ejabberd uses different record to store jids
     %JID = <<Username/binary, "@", Server/binary, "/rpc">>,
     %{jid, JID, Username, Server, <<"rpc">>}.
    {jid, Username, Server, <<"rpc">>, Username, Server, <<"rpc">>}.

%Groupchat 1.0 protocol
stanza_groupchat_enter_room(Room, Nick) ->
    stanza_to_room(escalus_stanza:presence(<<"available">>), Room, Nick).


stanza_groupchat_enter_room_no_nick(Room) ->
    stanza_to_room(escalus_stanza:presence(<<"available">>), Room).


%Basic MUC protocol
stanza_muc_enter_room(Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        Room, Nick).

stanza_muc_enter_password_protected_room(Room, Nick, Password) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}],
                                             children=[#xmlel{name = <<"password">>, children = [#xmlcdata{content=[Password]}]} ]}]),
        Room, Nick).

stanza_change_nick(Room, NewNick) ->
    stanza_to_room(escalus_stanza:presence(<<"available">>), Room, NewNick).

start_room(Config, User, Room, Nick, Opts) ->
    From = generate_rpc_jid(User),
    escalus_ejabberd:rpc(mod_muc, create_instant_room,
        [<<"localhost">>, Room, From, Nick,
            Opts]),
    [{nick, Nick}, {room, Room} | Config].

destroy_room(Config) ->
    case escalus_ejabberd:rpc(ets, lookup, [muc_online_room,
        {?config(room, Config), <<"muc.localhost">>}]) of
        [{_,_,Pid}|_] -> gen_fsm:send_all_state_event(Pid, destroy);
        _ -> ok
    end.

room_address(Room) ->
    <<Room/binary, "@", ?MUC_HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@", ?MUC_HOST/binary, "/", Nick/binary>>.

%%--------------------------------------------------------------------
%% Helpers (stanzas)
%%--------------------------------------------------------------------

stanza_message_to_room(Room, Payload) ->
    stanza_to_room(#xmlel{name = <<"message">>, children = Payload}, Room).

stanza_change_availability(NewStatus, Room, Nick) ->
    stanza_to_room(
        escalus_stanza:presence( <<"available">>,
                                [
                                #xmlel{ name = <<"show">>, children=[ #xmlcdata{content=[<<"xa">>]}]},
                                #xmlel{ name = <<"status">>, children=[ #xmlcdata{content=[NewStatus]}]}
                                ]),
        Room, Nick).

stanza_muc_enter_room_history_setting(Room, Nick, Setting, Value) ->
    stanza_to_room(
        escalus_stanza:presence(  <<"available">>,
                                [#xmlel{ name = <<"x">>, 
                    						  attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}],
											  children = [#xmlel{name= <<"history">>, attrs=[{Setting, Value}]}]}]),
        Room, Nick).

stanza_room_subject(Room, Subject) ->
    stanza_to_room(#xmlel{name = <<"message">>,
        attrs = [{<<"type">>,<<"groupchat">>}],
        children = [#xmlel{
            name = <<"subject">>,
            children = [exml:escape_cdata(Subject)]
        }]
    }, Room).

stanza_mediated_invitation(Room, Invited) ->
    Payload = [ #xmlel{name = <<"invite">>,
        attrs = [{<<"to">>, escalus_utils:get_short_jid(Invited)}]} ],
    stanza_to_room(#xmlel{name = <<"message">>,
        children = [ #xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
            children = Payload }
        ]}, Room).

stanza_mediated_invitation_decline(Room,Sender) ->
    Payload = [ #xmlel{name = <<"decline">>,
        attrs = [{<<"to">>, escalus_utils:get_short_jid(Sender)}]} ],
    stanza_to_room(#xmlel{name = <<"message">>,
        children = [ #xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
            children = Payload }
        ]}, Room).

stanza_set_roles(Room, List) ->
    Payload = lists:map(fun({Nick, Role}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"nick">>, Nick}, {<<"role">>, Role}]};
    ({Nick, Role, Reason}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"nick">>, Nick}, {<<"role">>, Role}],
        children = [#xmlel{
            name = <<"reason">>,
            children = #xmlcdata{content = Reason}}
        ]}
    end, List),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_set_affiliations(Room, List) ->
    Payload = lists:map(fun({JID, Affiliation}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}]};
    ({JID, Affiliation, Reason}) ->
        #xmlel{name = <<"item">>,
        attrs = [{<<"jid">>, JID}, {<<"affiliation">>, Affiliation}],
        children = [#xmlel{
            name = <<"reason">>,
            children = #xmlcdata{content = Reason}}
        ]}
    end, List),
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_ADMIN, Payload), Room).

stanza_role_list_request(Room, Role) ->
    Payload = [ #xmlel{name = <<"item">>,
        attrs = [{<<"role">>, Role}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_affiliation_list_request(Room, Affiliation) ->
    Payload = [ #xmlel{name = <<"item">>,
        attrs = [{<<"affiliation">>, Affiliation}]} ],
    stanza_to_room(escalus_stanza:iq_get(?NS_MUC_ADMIN, Payload), Room).

stanza_ban_list_request(Room) ->
    stanza_affiliation_list_request(Room, <<"outcast">>).

stanza_ban_user(User, Room) ->
  stanza_set_affiliations(Room, [{escalus_utils:get_short_jid(User), <<"outcast">>}]).

stanza_ban_user(User, Room, Reason) ->
  stanza_set_affiliations(Room, [{escalus_utils:get_short_jid(User), <<"outcast">>, Reason}]).

stanza_join_room(Room, Nick) ->
    stanza_to_room(#xmlel{name = <<"presence">>, children =
        #xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>,<<"http://jabber.org/protocol/muc">>}]
        }
    },Room, Nick).

stanza_voice_request_form(Room) ->
    Payload = [ form_field({<<"muc#role">>, <<"participant">>, <<"text-single">>}) ],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_voice_request_approval(Room, JID, Nick) ->
    Items = [{<<"muc#role">>, <<"participant">>, <<"text-single">>},
        {<<"muc#jid">>, JID, <<"jid-single">>},
        {<<"muc#roomnick">>, Nick, <<"text-single">>},
        {<<"muc#request_allow">>, <<"true">>, <<"boolean">>}],
    Payload = [ form_field(El) || El <- Items],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_voice_request_approval_nonick(Room, JID) ->
    Items = [{<<"muc#role">>, <<"participant">>, <<"text-single">>},
        {<<"muc#jid">>, JID, <<"jid-single">>},
        {<<"muc#request_allow">>, <<"true">>, <<"boolean">>}],
    Payload = [ form_field(El) || El <- Items],
    stanza_message_to_room(Room, stanza_form(Payload, ?NS_MUC_REQUEST)).

stanza_configuration_form(Room, Params) ->
    DefaultParams = [],
    FinalParams = lists:foldl(
        fun({Key,_Val,_Type},Acc) ->
            lists:keydelete(Key,1,Acc)
        end,
        DefaultParams, Params) ++ Params,
    Payload = [ form_field(FieldData) || FieldData <- FinalParams ],
    stanza_to_room(escalus_stanza:iq_set(
          ?NS_MUC_OWNER, stanza_form(Payload, ?NS_MUC_ROOMCONFIG)), Room).

stanza_cancel(Room) ->
    Payload = #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>,<<"jabber:x:data">>}, {<<"type">>,<<"cancel">>}]
    },
    stanza_to_room(escalus_stanza:iq_set(
          ?NS_MUC_OWNER, Payload), Room).

stanza_form(Payload, Type) ->
    #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>,<<"jabber:x:data">>}, {<<"type">>,<<"submit">>}],
        children = [form_field({<<"FORM_TYPE">>, Type, <<"hidden">>}) | Payload]
    }.

form_field({Var, Value, Type}) ->
    #xmlel{ name  = <<"field">>,
                 attrs = [{<<"type">>, Type},{<<"var">>, Var}],
                 children  = [#xmlel{name = <<"value">>,
                                          children = [#xmlcdata{content = Value}] }] }.

stanza_instant_room(Room) ->
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_DATA_FORMS},
                                             {<<"type">>, <<"submit">>}]},
    escalus_stanza:to(escalus_stanza:iq_set(?NS_MUC_OWNER, [X]), Room).

stanza_reserved_room(Room) ->
    escalus_stanza:to(escalus_stanza:iq_get(?NS_MUC_OWNER, []), Room).

stanza_destroy_room(Room) ->
    Payload = [ #xmlel{name = <<"destroy">>} ],
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, Payload), Room).

stanza_enter_room(Room, Nick) ->
    stanza_to_room(#xmlel{name = <<"presence">>}, Room, Nick).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_get_rooms() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%   id='zb8q41f4'
    %%   to='chat.shakespeare.lit'
    %%   type='get'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        ?MUC_HOST).

stanza_get_features() ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='lx09df27'
    %%     to='chat.shakespeare.lit'
    %%     type='get'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_INFO, []), <<"to">>,
        ?MUC_HOST).

stanza_get_services(Config) ->
    %% <iq from='hag66@shakespeare.lit/pda'
    %%     id='h7ns81g'
    %%     to='shakespeare.lit'
    %%     type='get'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'/>
    %% </iq>
    escalus_stanza:setattr(escalus_stanza:iq_get(?NS_DISCO_ITEMS, []), <<"to">>,
        escalus_config:get_config(ejabberd_domain, Config)).

%%--------------------------------------------------------------------
%% Helpers (assertions)
%%--------------------------------------------------------------------

invite_has_reason(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"reason">>}, cdata]) =/= undefined.

has_reason(Stanza) ->
    exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"item">>},
        {element, <<"reason">>}]) =/= undefined.

is_message_form(Stanza) ->
    exml_query:path(Stanza,[{element,<<"x">>},
        {attr, <<"xmlns">>}]) =:= ?NS_DATA_FORMS.

is_form(Stanza) ->
    exml_query:path(Stanza,[{element, <<"query">>}, {element,<<"x">>},
        {attr, <<"xmlns">>}]) =:= ?NS_DATA_FORMS.

is_groupchat_message(Stanza) ->
    escalus_pred:is_message(Stanza) andalso
    escalus_pred:has_type(<<"groupchat">>, Stanza).

is_subject_message(Stanza) ->
    is_groupchat_message(Stanza) andalso
    exml_query:subelement(Stanza, <<"subject">>) /= undefined.

is_subject_message(Stanza, Subject) ->
    is_groupchat_message(Stanza) andalso
    exml_query:path(Stanza, [{element,<<"subject">>},cdata]) == Subject.

is_unavailable_presence(Stanza, Status) ->
    escalus_pred:is_presence_with_type(<<"unavailable">>,Stanza) andalso
    is_presence_with_status_code(Stanza, Status).

is_membership_presence(Stanza, Affiliation, Role) ->
    is_presence_with_affiliation(Stanza, Affiliation) andalso
    is_presence_with_role(Stanza, Role).

is_invitation(Stanza) ->
    escalus:assert(is_message, Stanza),
    #xmlel{} = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"invite">>}]).

is_invitation_decline(Stanza) ->
    escalus:assert(is_message, Stanza),
    #xmlel{} = exml_query:path(Stanza, [{element, <<"x">>}, {element, <<"decline">>}]).

is_presence_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"x">>), Role).

is_iq_with_role(Stanza, Role) ->
    is_with_role(exml_query:subelement(Stanza, <<"query">>), Role).

is_with_role(Stanza, Role) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    lists:any(fun(Item) ->
        exml_query:attr(Item, <<"role">>) =:= Role
    end, Items).

is_presence_with_nick(Stanza, Nick) ->
    escalus_pred:is_presence(Stanza) andalso
    exml_query:path(Stanza,[{element, <<"x">>},
        {element, <<"item">>}, {attribute, <<"nick">>}]) == Nick.

is_presence_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"x">>), Affiliation).

is_iq_with_affiliation(Stanza, Affiliation) ->
    is_affiliation(exml_query:subelement(Stanza, <<"query">>), Affiliation).

is_affiliation(Stanza, Affiliation) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    lists:any(fun(Item) ->
        exml_query:attr(Item, <<"affiliation">>) =:= Affiliation
    end, Items).

is_presence_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_presence_with_full_jid(Stanza, User) ->
    is_full_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_iq_with_jid(Stanza, User) ->
    is_jid(exml_query:subelement(Stanza, <<"query">>), User).

is_full_jid(Stanza, User) ->
    Item = exml_query:subelement(Stanza, <<"item">>),
    JID = escalus_utils:get_jid(User),
    JID = exml_query:attr(Item, <<"jid">>).

is_jid(Stanza, User) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    JID = escalus_utils:get_jid(User),
    lists:any(fun(Item) -> exml_query:attr(Item, <<"jid">>) =:= JID end, Items).

is_presence_with_short_jid(Stanza, User) ->
    is_short_jid(exml_query:subelement(Stanza, <<"x">>), User).

is_iq_with_short_jid(Stanza, User) ->
    is_short_jid(exml_query:subelement(Stanza, <<"query">>), User).

is_short_jid(Stanza, User) ->
    Items = exml_query:subelements(Stanza, <<"item">>),
    JID = escalus_utils:get_short_jid(User),
    lists:any(fun(Item) -> exml_query:attr(Item, <<"jid">>) =:= JID end, Items).

is_presence_with_status_code(Presence, Code) ->
    escalus:assert(is_presence, Presence),
    Code == exml_query:path(Presence, [{element, <<"x">>}, {element, <<"status">>},
        {attr, <<"code">>}]).

is_message_with_status_code(Message, Code) ->
    escalus_pred:is_message(Message) andalso
    Code == exml_query:path(Message, [{element, <<"x">>}, {element, <<"status">>},
        {attr, <<"code">>}]).

has_status_codes(Stanza, CodeList) ->
    StatusList = exml_query:subelements(exml_query:subelement(Stanza, <<"x">>), <<"status">>),
    StanzaCodes = lists:map(fun(Status) ->
                    exml_query:attr(Status, <<"code">>)
        end, StatusList),
    true = lists:all(fun (Code) ->
                        lists:member(Code, StanzaCodes)
            end, CodeList).


has_feature(Stanza, Feature) ->
    Features = exml_query:paths(Stanza, [{element, <<"query">>},
                                         {element, <<"feature">>}]),
    true = lists:any(fun(Item) ->
                        exml_query:attr(Item, <<"var">>) == Feature
                     end,
                     Features).

was_destroy_presented(#xmlel{children = [Items]} = Presence) ->
    #xmlel{} = exml_query:subelement(Items, <<"destroy">>),
    <<"unavailable">> = exml_query:attr(Presence, <<"type">>).

was_room_destroyed(Query) ->
    <<"result">> = exml_query:attr(Query, <<"type">>).

was_room_created(Stanza = #xmlel{children = [X]}) ->
    has_status_codes(Stanza, [<<"201">>, <<"110">>]),
    <<"owner">> = exml_query:path(X, [{element, <<"item">>},
                                      {attr, <<"affiliation">>}]),
    <<"moderator">> = exml_query:path(X, [{element, <<"item">>},
                                          {attr, <<"role">>}]).

has_room(JID, #xmlel{children = [ #xmlel{children = Rooms} ]}) ->
    %% <iq from='chat.shakespeare.lit'
    %%   id='zb8q41f4'
    %%   to='hag66@shakespeare.lit/pda'
    %%   type='result'>
    %% <query xmlns='http://jabber.org/protocol/disco#items'>
    %%    <item jid='heath@chat.shakespeare.lit'
    %%         name='A Lonely Heath'/>
    %%    <item jid='coven@chat.shakespeare.lit'
    %%         name='A Dark Cave'/>
    %%    <item jid='forres@chat.shakespeare.lit'
    %%         name='The Palace'/>
    %%     <item jid='inverness@chat.shakespeare.lit'
    %%         name='Macbeth&apos;s Castle'/>
    %%   </query>
    %% </iq>

    RoomPred = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == JID
    end,
    true = lists:any(RoomPred, Rooms).

count_rooms(#xmlel{children = [ #xmlel{children = Rooms} ]}, N) ->
    N = length(Rooms).

has_features(#xmlel{children = [ Query ]}) ->
    %%<iq from='chat.shakespeare.lit'
    %%  id='lx09df27'
    %%  to='hag66@shakespeare.lit/pda'
    %%  type='result'>
    %%  <query xmlns='http://jabber.org/protocol/disco#info'>
    %%    <identity
    %%      category='conference'
    %%      name='Shakespearean Chat Service'
    %%      type='text'/>
    %%      <feature var='http://jabber.org/protocol/muc'/>
    %%  </query>
    %%</iq>

    Identity = exml_query:subelement(Query, <<"identity">>),
    <<"conference">> = exml_query:attr(Identity, <<"category">>),
    #xmlel{name = _Name, attrs = _Attrs, children = _Body} = exml_query:subelement(Query, <<"feature">>).

has_muc(#xmlel{children = [ #xmlel{children = Services} ]}) ->
    %% should be along the lines of (taken straight from the XEP):
    %% <iq from='shakespeare.lit'
    %%     id='h7ns81g'
    %%     to='hag66@shakespeare.lit/pda'
    %%     type='result'>
    %%   <query xmlns='http://jabber.org/protocol/disco#items'>
    %%     <item jid='chat.shakespeare.lit'
    %%           name='Chatroom Service'/>
    %%   </query>
    %% </iq>

    %% is like this:
    %% {xmlel,<<"iq">>,
    %%     [{<<"from">>,<<"localhost">>},
    %%         {<<"to">>,<<"alice@localhost/res1">>},
    %%         {<<"id">>,<<"a5eb1dc70826598893b15f1936b18a34">>},
    %%         {<<"type">>,<<"result">>}],
    %%     [{xmlel,<<"query">>,
    %%             [{<<"xmlns">>,
    %%                     <<"http://jabber.org/protocol/disco#items">>}],
    %%             [{xmlel,<<"item">>,
    %%                     [{<<"jid">>,<<"vjud.localhost">>}],
    %%                     []},
    %%                 {xmlel,<<"item">>,
    %%                     [{<<"jid">>,<<"pubsub.localhost">>}],
    %%                     []},
    %%                 {xmlel,<<"item">>,
    %%                     [{<<"jid">>,<<"muc.localhost">>}],
    %%                     []},
    %%                 {xmlel,<<"item">>,
    %%                     [{<<"jid">>,<<"irc.localhost">>}],
    %%                     []}]}]}
    %% how to obtaing output like the above? simply put this in the test case:
    %% S = escalus:wait_for_stanza(Alice),
    %% error_logger:info_msg("~p~n", [S]),
    IsMUC = fun(Item) ->
        exml_query:attr(Item, <<"jid">>) == ?MUC_HOST
    end,
    lists:any(IsMUC, Services).

is_room_locked(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    escalus_pred:is_error(<<"cancel">>, <<"item-not-found">>, Stanza).
