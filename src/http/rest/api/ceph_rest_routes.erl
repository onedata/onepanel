%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for ceph.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_rest_routes).
-author("Wojciech Geisler").

-include("http/rest.hrl").

%% API
-export([routes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples consisting of a path, a handler module and
%% an initial request state.
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), #rest_req{}}].
routes() ->
    [
        %% Add managers to ceph cluster
        {<<"/provider/ceph/managers">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = managers,
                scope = private
            },
            %% Object with a list of Ceph manager configurations.
            data_spec = (rest_model:ceph_managers_model())
        }},

        %% Add monitors to Ceph cluster
        {<<"/provider/ceph/monitors">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = monitors,
                scope = private
            },
            %% List of Ceph monitor specifications.
            data_spec = (rest_model:ceph_monitors_model())
        }},

        %% Add OSDs to Ceph cluster
        {<<"/provider/ceph/osds">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = osds,
                scope = private
            },
            %% List of OSD specifications.
            data_spec = (rest_model:ceph_osds_model())
        }},

        %% Configure Ceph cluster
        {<<"/provider/ceph">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = cluster,
                scope = private
            },
            %% The Ceph cluster specification.
            data_spec = (rest_model:ceph_cluster_model())
        }},

        %% Get block devices for Ceph OSD
        {<<"/provider/ceph/preflight/block_devices">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = block_devices,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Host for which block devices should be returned.
                host => string
            }
        }},

        %% Get Ceph manager
        {<<"/provider/ceph/managers/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {manager, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph managers
        {<<"/provider/ceph/managers">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = managers,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph monitor
        {<<"/provider/ceph/monitors/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {monitor, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph monitors
        {<<"/provider/ceph/monitors">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = monitors,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph OSD
        {<<"/provider/ceph/osds/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {osd, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storage space usage details for specific OSD
        {<<"/provider/ceph/osds/:id/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {osd_usage, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph OSDs list
        {<<"/provider/ceph/osds">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = osds,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get global Ceph params
        {<<"/provider/ceph">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = global_params,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get details of a Ceph pool
        {<<"/provider/ceph/pools/:name">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool, ?BINDING(name)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storage space usage details for specific pool
        {<<"/provider/ceph/pools/:name/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool_usage, ?BINDING(name)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Ceph pools
        {<<"/provider/ceph/pools">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = pools,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph cluster health
        {<<"/provider/ceph/status">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = status,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ceph storage space usage.
        {<<"/provider/ceph/usage">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = usage,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify pool params
        {<<"/provider/ceph/pools/:name">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_ceph,
                id = undefined,
                aspect = {pool, ?BINDING(name)},
                scope = private
            },
            data_spec = (rest_model:ceph_pool_model())
        }}

    ].
