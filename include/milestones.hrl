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
-ifndef(ONEPANEL_MILESTONES_HRL).
-define(ONEPANEL_MILESTONES_HRL, 1).

% services are assigned to nodes
-define(MILESTONE_CLUSTER, service_nodes).
-define(MILESTONE_LETSENCRYPT, letsencrypt).
-define(MILESTONE_READY, ready).

-endif.
