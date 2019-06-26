%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_cron module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cron_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("names.hrl").
-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CRON_PERIOD, 1000).
-define(JOB_TIMEOUT, 5 * ?CRON_PERIOD div 2 ).

-define(CALLED_MODULE, some_module).
-define(CALLED_FUNCTION, some_function).
-define(CALLED_FUNCTION2, other_function).

-define(SIMPLE_ACTION, fun() -> ?CALLED_MODULE:?CALLED_FUNCTION() end).

%%%===================================================================
%%% Test functions
%%%===================================================================

onepanel_cron_test_() ->
    {foreach, fun prepare/0, fun stop/1, [
        fun job_is_executed/0,
        fun condition_prevents_execution/0,
        fun multiple_jobs_are_executed/0,
        fun job_is_repeated/0,
        fun job_is_removed/0,
        {timeout, 30, fun running_job_is_skipped/0},
        {timeout, 30, fun long_job_is_aborted/0}
    ]}.


job_is_executed() ->
    Period = 1,
    onepanel_cron:add_job(some_job, ?SIMPLE_ACTION, Period),

    timer:sleep(?CRON_PERIOD),

    ?assertEqual(1, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')).


condition_prevents_execution() ->
    Period = timer:hours(1),
    Condition = fun() -> false end,
    onepanel_cron:add_job(some_job, ?SIMPLE_ACTION, Period, Condition),

    timer:sleep(?CRON_PERIOD),

    ?assertEqual(0, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')).


job_is_repeated() ->
    Period = ?CRON_PERIOD div 10,
    onepanel_cron:add_job(some_job, ?SIMPLE_ACTION, Period),

    timer:sleep(?CRON_PERIOD * 3),

    ?assertEqual(3, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')).


multiple_jobs_are_executed() ->
    Action1 = fun() -> ?CALLED_MODULE:?CALLED_FUNCTION() end,
    Action2 = fun() -> ?CALLED_MODULE:?CALLED_FUNCTION2() end,
    Period1 = 100,
    Period2 = 3 * ?CRON_PERIOD div 2,
    onepanel_cron:add_job(some_job1, Action1, Period1),
    onepanel_cron:add_job(some_job2, Action2, Period2),

    timer:sleep(?CRON_PERIOD * 3 + 100),
    ?assertEqual(3, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')),
    ?assertEqual(1, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION2, '_')).


job_is_removed() ->
    JobName = some_job,
    Period = ?CRON_PERIOD div 10,
    onepanel_cron:add_job(JobName, ?SIMPLE_ACTION, Period),
    timer:sleep(?CRON_PERIOD),

    onepanel_cron:remove_job(JobName),
    timer:sleep(?CRON_PERIOD),

    ?assertEqual(1, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')).


running_job_is_skipped() ->
    Action = fun() -> ?CALLED_MODULE:?CALLED_FUNCTION(), timer:sleep(?CRON_PERIOD * 3) end,
    % would be run every tick if wasn't skipped
    Period = ?CRON_PERIOD div 10,
    onepanel_cron:add_job(some_job, Action, Period),
    timer:sleep(?CRON_PERIOD * 2),

    ?assertEqual(1, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')).


long_job_is_aborted() ->
    Delay = ?JOB_TIMEOUT + 1000,
    Action = fun() ->
        ?CALLED_MODULE:?CALLED_FUNCTION(),
        timer:sleep(Delay),
        ?CALLED_MODULE:?CALLED_FUNCTION2()
    end,
    Period = 1,

    onepanel_cron:add_job(some_job, Action, Period),
    timer:sleep(?CRON_PERIOD + Delay + 1000),

    % second execution starts after abort, is aborted before call to function2
    ?assertEqual(2, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION, '_')),
    ?assertEqual(0, meck:num_calls(?CALLED_MODULE, ?CALLED_FUNCTION2, '_')).


%%%===================================================================
%%% Test fixtures
%%%===================================================================

prepare() ->
    onepanel_env:set(cron_period, ?CRON_PERIOD),
    onepanel_env:set(cron_job_timeout, ?JOB_TIMEOUT),

    {ok, Pid} = onepanel_cron:start_link(),
    % prevent termination of the test process
    erlang:unlink(Pid),

    meck:new([?CALLED_MODULE], [non_strict]),
    meck:expect(?CALLED_MODULE, ?CALLED_FUNCTION, fun() -> ok end),
    meck:expect(?CALLED_MODULE, ?CALLED_FUNCTION2, fun() -> ok end),

    #{pid => Pid}.

stop(#{pid := Pid}) ->
    meck:unload(),
    exit(Pid, kill),
    % ensure process dies before next test run
    % to prevent already_started errors
    wait_to_die(Pid).


-spec wait_to_die(pid()) -> ok.
wait_to_die(Pid) ->
    case erlang:is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(100),
            wait_to_die(Pid)
    end.

-endif.
