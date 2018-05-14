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
-ifndef(DEPLOYMENT_PROGRESS_HRL).
-define(DEPLOYMENT_PROGRESS_HRL, 1).

% services are assigned to nodes
-define(PROGRESS_CLUSTER, service_nodes).
-define(PROGRESS_CLUSTER_IPS, cluster_ips).
-define(PROGRESS_LETSENCRYPT_CONFIG, letsencrypt).
-define(PROGRESS_READY, ready).

-endif.
