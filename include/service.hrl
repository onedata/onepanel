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
    service :: service:name(),
    module :: module(),
    function :: atom(),
    args :: term(),
    ctx :: undefined | service:ctx(),
    condition = fun(_) -> true end :: service:condition(),
    verify_hosts :: undefined | boolean(),
    attempts = 1 :: pos_integer(),
    retry_delay = onepanel_env:get(service_step_retry_delay) :: non_neg_integer()
}).

-record(steps, {
    service :: service:name(),
    action :: service:action(),
    ctx :: undefined | service:ctx(),
    condition = fun(_) -> true end :: service:condition(),
    verify_hosts :: undefined | boolean()
}).

-endif.
