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
-define(ONEPANEL_TOKEN_PREFIX, "onepanel").


-record(client, {
    %% Roles:
    %% guest - unauthenticated client, on endpoints which allow that, or user with role 'regular'
    %% member - Onezone user belonging to the cluster, governed by privileges
    %% root - user with role 'admin' (in the future: authenticated with root password)
    role :: guest | member | root,
    user :: undefined | #user_details{},
    zone_auth = none :: rest_handler:zone_auth(),
    privileges :: undefined | [privileges:cluster_privilege()]
}).


-endif.