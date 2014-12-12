%%% @doc Common Test Example Common Test Hook module.
-module(ct_travis_hook).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3]).

-export([terminate/1]).

-record(state, { log_fd, test_n, group_test_n }).

%% @doc Return a unique id for this CTH.
id(Opts) ->
    ct_travis_hook.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state. 
init(Id, Opts) ->
    ct:pal("Init ct_travis_hook", []),
    {ok, FD} = file:open("/tmp/ct_travis_hook.log", [append, write]),
    {ok, #state{log_fd=FD}}.

%% @doc Called before init_per_suite is called. 
pre_init_per_suite(Suite,Config,State) ->
    State2 = reset_test_n(State),
    {Config, fold_start(suite, Suite, State2)}.

%% @doc Called after init_per_suite.
post_init_per_suite(Suite,Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite. 
pre_end_per_suite(Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite. 
post_end_per_suite(Suite,Config,Return,State) ->
    {Return, fold_end(suite, Suite, Return, State)}.

%% @doc Called before each init_per_group.
pre_init_per_group(Group,Config,State) ->
    State2 = set_group_test_n(State),
    State3 = fold_start(group, Group, State2),
    {Config, State3}.

%% @doc Called after each init_per_group.
post_init_per_group(Group,Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group. 
pre_end_per_group(Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group. 
post_end_per_group(Group,Config,Return,State) ->
    {Return, fold_end(group, Group, Return, State)}.

%% @doc Called before each test case.
pre_init_per_testcase(TC,Config,State) ->
    {Config, fold_start(test, TC, State)}.

%% @doc Called after each test case.
post_end_per_testcase(TC,Config,Return,State) ->
    State2 = fold_end(test, TC, Return, State),
    State3 = inc_test_n(State2),
    {Return, State3}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(TC, Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.  
on_tc_skip(TC, Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(State) ->
    ok.

%% --------------------------------------------------------------------
%% Private

%% Same as:
%% echo -en "travis_fold:start:Name\\r"
%% echo "Name"
fold_start(Type, Name, State=#state{log_fd=FD}) ->
    STestN = get_and_print_n(Type, State),
    SType = print_type(Type),
    io:format(FD, "travis_fold:start:~s~p.~s\r~p~n",
              [SType, Name, STestN, Name]),
    State.

%% Same as:
%% echo -en "travis_fold:end:Name\\r"
fold_end(Type, Name, Return, State=#state{log_fd=FD}) ->
    STestN = get_and_print_n(Type, State),
    SType = print_type(Type),
    SReturn = print_return(Return),
    io:format(FD, "travis_fold:end:~s~p.~s\r~s",
              [SType, Name, STestN, SReturn]),
    State.

print_type(suite) -> "s.";
print_type(group) -> "g.";
print_type(test)  -> "t.".

print_return({skip, Reason}) ->
    io_lib:format("Skipped ~p~n", [Reason]);
print_return({fail, Reason}) ->
    io_lib:format("Failed ~p~n", [Reason]);
print_return(_) ->
    "".

inc_test_n(State=#state{test_n=TestN}) ->
    State#state{test_n=TestN+1}.

reset_test_n(State=#state{test_n=TestN}) ->
    State#state{test_n=1}.

set_group_test_n(State=#state{test_n=TestN}) ->
    State#state{group_test_n=TestN}.

get_and_print_n(Type, State) ->
    TestN = get_test_n(Type, State),
    print_test_n(Type, TestN).

get_test_n(test, State=#state{test_n=TestN}) ->
    TestN;
get_test_n(group, State=#state{group_test_n=TestN}) ->
    TestN;
get_test_n(suite, State=#state{}) ->
    1.

print_test_n(suite, _TestN) ->
    ""; % do not print for suites, because TestN is always 1
print_test_n(_Type, TestN) ->
    integer_to_list(TestN).
