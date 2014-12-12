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

-record(state, { log_fd }).

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
    {Config, fold_start(suite, Suite, State)}.

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
    {Config, fold_start(group, Group, State)}.

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
    {Return, fold_end(test, TC, Return, State)}.

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
    io:format(FD, "travis_fold:start:~s~p\r~p~n", [print_type(Type), Name, Name]),
    State.

%% Same as:
%% echo -en "travis_fold:end:Name\\r"
fold_end(Type, Name, Return, State=#state{log_fd=FD}) ->
    io:format(FD, "travis_fold:end:~s~p\r~s", [print_type(Type), Name, print_return(Return)]),
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
