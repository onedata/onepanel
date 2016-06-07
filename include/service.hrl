%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-author("Krzysztof Trzepla").

-ifndef(ONEPANEL_SERVICE_HRL).
-define(ONEPANEL_SERVICE_HRL, 1).

-record(step, {
    hosts :: [service:host()] | fetch,
    service :: service:name(),
    module :: module(),
    function :: atom(),
    ctx :: service:ctx(),
    condition = fun() -> true end :: fun(() -> boolean()),
    ignore_errors :: boolean()
}).

-record(steps, {
    service :: service:name(),
    action :: service:action(),
    ctx :: service:ctx(),
    ignore_errors :: boolean()
}).

-endif.