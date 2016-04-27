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

-ifndef(ONEPANEL_HRL).
-define(ONEPANEL_HRL, 1).

-define(APP_NAME, onepanel).
-define(RPC_TIMEOUT, timer:seconds(
    application:get_env(?APP_NAME, rpc_timeout_seconds, 10))).

-endif.