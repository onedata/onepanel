%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for luma_db_local_feed.
%%% @end
%%%--------------------------------------------------------------------
-module(luma_db_local_feed_rest_routes).
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
        %% Insert Onedata user to credentials mapping into local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/all/onedata_user_to_credentials">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = local_feed_luma_onedata_user_to_credentials_mapping,
                scope = private
            },
            produces = [<<"application/json">>],
            %% New user mapping
            data_spec = (rest_model:luma_user_mapping_model())
        }},

        %% Lookup mapping of ACL group in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/:groupname">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_group_to_onedata_group_mapping, ?BINDING(groupname)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup mapping of ACL user in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/:username">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_user_to_onedata_user_mapping, ?BINDING(username)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup default posix credentials in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/posix_compatible/default_credentials/:space_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_default_posix_credentials, ?BINDING(space_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup default display credentials in local feed
        {<<"/provider/storages/:id/luma/local_feed/display_credentials/all/default/:space_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_display_credentials, ?BINDING(space_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup Onedata user to credentials mapping in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/all/onedata_user_to_credentials/:onedata_user_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_onedata_user_to_credentials_mapping, ?BINDING(onedata_user_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Lookup mapping of UID in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/:uid">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_uid_to_onedata_user_mapping, ?BINDING(uid)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Update Onedata user to credentials mapping in local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/all/onedata_user_to_credentials/:onedata_user_id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_onedata_user_to_credentials_mapping, ?BINDING(onedata_user_id)},
                scope = private
            },
            produces = [<<"application/json">>],
            %% New user mapping
            data_spec = (rest_model:luma_storage_user_model())
        }},

        %% Remove mapping of ACL group from local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/:groupname">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_group_to_onedata_group_mapping, ?BINDING(groupname)},
                scope = private
            }
        }},

        %% Remove mapping of ACL user from local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/:username">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_user_to_onedata_user_mapping, ?BINDING(username)},
                scope = private
            }
        }},

        %% Remove default posix credentials from local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/posix_compatible/default_credentials/:space_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_default_posix_credentials, ?BINDING(space_id)},
                scope = private
            }
        }},

        %% Remove default display credentials from local feed
        {<<"/provider/storages/:id/luma/local_feed/display_credentials/all/default/:space_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_display_credentials, ?BINDING(space_id)},
                scope = private
            }
        }},

        %% Remove Onedata user to credentials mapping from local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/all/onedata_user_to_credentials/:onedata_user_id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_onedata_user_to_credentials_mapping, ?BINDING(onedata_user_id)},
                scope = private
            }
        }},

        %% Remove mapping of UID from local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/:uid">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_uid_to_onedata_user_mapping, ?BINDING(uid)},
                scope = private
            }
        }},

        %% Insert mapping of ACL group into local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_group_to_onedata_group/:groupname">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_group_to_onedata_group_mapping, ?BINDING(groupname)},
                scope = private
            },
            %% Credentials identifying group in the Onedata system.
            data_spec = (rest_model:luma_onedata_group_model())
        }},

        %% Insert mapping of ACL user into local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/acl_user_to_onedata_user/:username">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_acl_user_to_onedata_user_mapping, ?BINDING(username)},
                scope = private
            },
            %% Credentials identifying user in the Onedata system.
            data_spec = (rest_model:luma_onedata_user_model())
        }},

        %% Insert default posix credentials into local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_access/posix_compatible/default_credentials/:space_id">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_default_posix_credentials, ?BINDING(space_id)},
                scope = private
            },
            %% New default storage credentials for the space support.
            data_spec = (rest_model:posix_compatible_credentials_model())
        }},

        %% Insert default display credentials into local feed
        {<<"/provider/storages/:id/luma/local_feed/display_credentials/all/default/:space_id">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_display_credentials, ?BINDING(space_id)},
                scope = private
            },
            %% New default display credentials for the space support.
            data_spec = (rest_model:posix_compatible_credentials_model())
        }},

        %% Insert mapping of UID into local feed
        {<<"/provider/storages/:id/luma/local_feed/storage_sync/posix_compatible/uid_to_onedata_user/:uid">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = {local_feed_luma_uid_to_onedata_user_mapping, ?BINDING(uid)},
                scope = private
            },
            %% Credentials identifying user in the Onedata system.
            data_spec = (rest_model:luma_onedata_user_model())
        }}

    ].
