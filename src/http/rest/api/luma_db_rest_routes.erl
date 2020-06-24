%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for luma_db.
%%% @end
%%%--------------------------------------------------------------------
-module(luma_db_rest_routes).
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
        %% Clear LUMA DB
        {<<"/provider/storages/:id/luma/db">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = luma_db,
                scope = private
            }
        }},

        %% Lookup mapping of ACL group
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/acl_group_to_onedata_group/:groupname">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_acl_group_to_onedata_group_mapping, ?BINDING(groupname)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup mapping of ACL user
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/acl_user_to_onedata_user/:username">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_acl_user_to_onedata_user_mapping, ?BINDING(username)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get LUMA DB configuration
        {<<"/provider/storages/:id/luma/config">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = luma_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup default posix credentials
        {<<"/provider/storages/:id/luma/db/storage_access/posix_compatible/default_credentials/:space_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_default_posix_credentials, ?BINDING(space_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup default display credentials
        {<<"/provider/storages/:id/luma/db/display_credentials/all/default/:space_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_display_credentials, ?BINDING(space_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup Onedata user to credentials mapping
        {<<"/provider/storages/:id/luma/db/storage_access/all/onedata_user_to_credentials/:onedata_user_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_onedata_user_to_credentials_mapping, ?BINDING(onedata_user_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup mapping of UID
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/uid_to_onedata_user/:uid">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_uid_to_onedata_user_mapping, ?BINDING(uid)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Remove mapping of ACL group
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/acl_group_to_onedata_group/:groupname">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_acl_group_to_onedata_group_mapping, ?BINDING(groupname)},
                scope = private
            }
        }},

        %% Remove mapping of ACL user
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/acl_user_to_onedata_user/:username">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_acl_user_to_onedata_user_mapping, ?BINDING(username)},
                scope = private
            }
        }},

        %% Remove default posix credentials
        {<<"/provider/storages/:id/luma/db/storage_access/posix_compatible/default_credentials/:space_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_default_posix_credentials, ?BINDING(space_id)},
                scope = private
            }
        }},

        %% Remove default display credentials
        {<<"/provider/storages/:id/luma/db/display_credentials/all/default/:space_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_display_credentials, ?BINDING(space_id)},
                scope = private
            }
        }},

        %% Remove Onedata user to credentials mapping
        {<<"/provider/storages/:id/luma/db/storage_access/all/onedata_user_to_credentials/:onedata_user_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_onedata_user_to_credentials_mapping, ?BINDING(onedata_user_id)},
                scope = private
            }
        }},

        %% Remove mapping of UID
        {<<"/provider/storages/:id/luma/db/storage_import/posix_compatible/uid_to_onedata_user/:uid">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {luma_uid_to_onedata_user_mapping, ?BINDING(uid)},
                scope = private
            }
        }}

    ].
