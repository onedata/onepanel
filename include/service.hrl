%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains service records definitions.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_SERVICE_HRL).
-define(ONEPANEL_SERVICE_HRL, 1).

-define(IS_SERVICE_NAME(_Arg), is_atom(_Arg)).

-record(step, {
    hosts :: undefined | [service:host()],
    selection = all :: all | any | first | rest,
    service :: service:name() | undefined,
    module :: module(),
    function :: atom(),
    args :: [term()] | undefined,
    ctx :: service:step_ctx() | undefined,
    condition = true :: service:condition(),
    verify_hosts :: boolean() | undefined,
    attempts = 1 :: pos_integer(),
    retry_delay = onepanel_env:get(service_step_retry_delay) :: non_neg_integer()
}).

-record(steps, {
    service :: service:name() | undefined,
    action :: service:action(),
    ctx :: service:step_ctx() | undefined,
    condition = true :: service:condition(),
    verify_hosts :: boolean() | undefined
}).


%%%===================================================================
%%% Records for tracking action execution
%%%===================================================================

-record(action_steps_count, {
    service :: service:name(),
    action :: service:action(),
    count :: non_neg_integer()
}).

-record(action_begin, {
    service :: service:name(),
    action :: service:action()
}).

-record(action_end, {
    service :: service:name(),
    action :: service:action(),
    result :: ok | {error, term()}
}).

-record(step_begin, {
    module :: module(),
    function :: atom()
}).

-record(step_end, {
    module :: module(),
    function :: atom(),
    good_bad_results :: service_executor:hosts_results()
}).


-endif.
