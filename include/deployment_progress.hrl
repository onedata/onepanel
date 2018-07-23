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

% user accepted determined IPs (or batch config was used)
-define(PROGRESS_CLUSTER_IPS, cluster_ips).

% user decided if Let's Encrypt should be enabled
-define(PROGRESS_LETSENCRYPT_CONFIG, letsencrypt).

% user acknowledged the DNS check resutls or they turned out OK
% during batch deployment
-define(DNS_CHECK_ACKNOWLEDGED, dns_check_acknowledged).

% all steps of configuration have been performed
-define(PROGRESS_READY, ready).

-endif.
