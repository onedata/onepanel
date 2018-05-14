%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains macros for deployment steps.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(onepanel_deployment_HRL).
-define(onepanel_deployment_HRL, 1).

% services are assigned to nodes
-define(PROGRESS_CLUSTER, service_nodes).
-define(PROGRESS_LETSENCRYPT, letsencrypt).
-define(PROGRESS_READY, ready).

-endif.
