%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains macros and records for user authentication.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_AUTHENTICATION_HRL).
-define(ONEPANEL_AUTHENTICATION_HRL, 1).

-include_lib("ctool/include/oz/oz_users.hrl").

-define(ONEPANEL_TOKEN_SEPARATOR, ":").
-define(ONEPANEL_USER_AUTH_TOKEN_PREFIX, "onepanel").
-define(ONEPANEL_INVITE_TOKEN_PREFIX, "onepanelInvite").

-define(AUTHORIZATION_NONCE_LEN, 44).   % 4*ceil(onepanel_utils:?UUID_LEN/3)

%% Usernames which can be used with the emergency passphrase for convenience
%% when using clients which always expect basic auth to have two parts.
-define(LOCAL_USERNAME, <<"onepanel">>).
-define(LOCAL_SESSION_USERNAME, <<"__onepanel">>).

-record(client, {
    %% Roles:
    %% guest - unauthenticated client, on endpoints with noauth enabled
    %% member - Onezone user belonging to the cluster, governed by privileges
    %% peer - Onepanel node authorized with #authorization_nonce{}
    %% root - client authenticated with the emergency passphrase
    role = guest :: guest | member | peer | root,
    user :: undefined | #user_details{},
    zone_credentials = none :: rest_handler:zone_credentials(),
    auth = aai:nobody_auth() :: aai:auth(),
    privileges :: undefined | [privileges:cluster_privilege()]
}).


-endif.