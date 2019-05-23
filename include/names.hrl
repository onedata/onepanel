%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains common names macros.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_NAMES_HRL).
-define(ONEPANEL_NAMES_HRL, 1).

-define(APP_NAME, onepanel).
-define(SERVICE_EXECUTOR_NAME, service_executor).
-define(SERVICE_WATCHER_NAME, service_watcher).

-define(SERVICE_PANEL, ?APP_NAME).
-define(SERVICE_CB, couchbase).
-define(SERVICE_CM, cluster_manager).
-define(SERVICE_CW, cluster_worker).
-define(SERVICE_LE, letsencrypt).
-define(SERVICE_OP, oneprovider).
-define(SERVICE_OPW, op_worker).
-define(SERVICE_OZ, onezone).
-define(SERVICE_OZW, oz_worker).

-endif.