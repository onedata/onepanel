%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header macros and records for user authentication.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_AUTHENTICATION_HRL).
-define(ONEPANEL_AUTHENTICATION_HRL, 1).

-include_lib("ctool/include/oz/oz_users.hrl").

-define(ONEPANEL_TOKEN_SEPARATOR, ":").
-define(ONEPANEL_TOKEN_PREFIX, "onepanel").

-define(NOAUTH_ROLE, guest).

-record(client, {
    %% Roles:
    %% user - normal Onezone user, governed by privileges
    %% root - authenticated with the root (Onepanel) password
    %% guest - unauthenticated client, on endpoints which allow that
    %% admin, regular - legacy roles of the local users
    role :: ?NOAUTH_ROLE | user | root | onepanel_user:role(),
    user :: undefined | #user_details{},
    zone_auth :: undefined | rest_handler:zone_auth(),
    privileges :: undefined | [privileges:cluster_privilege()]
}).


-endif.