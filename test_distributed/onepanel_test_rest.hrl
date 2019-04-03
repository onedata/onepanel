%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for REST API tests.
%%% @end
%%%--------------------------------------------------------------------
-author("Wojciech Geisler").

-ifndef(ONEPANEL_TEST_REST_HRL).
-define(ONEPANEL_TEST_REST_HRL, 1).


-define(OZ_USER_NAME, <<"joe">>).

-define(ADMIN_USER_NAME, <<"admin1">>).
-define(ADMIN_USER_PASSWORD, <<"Admin1Password">>).

-define(REG_USER_NAME, <<"user1">>).
-define(REG_USER_PASSWORD, <<"User1Password">>).


% authentication granting cluster root rights
-define(ROOT_AUTHS(HostOrConfig), [
    {?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD},
    onepanel_test_rest:obtain_local_token(HostOrConfig, ?ADMIN_USER_NAME, ?ADMIN_USER_PASSWORD)
]).

-define(OZ_AUTHS(HostOrConfig, Privileges), [
    onepanel_test_rest:oz_token_auth(<<"privileged">>, Privileges)
]).

-define(OZ_OR_ROOT_AUTHS(HostOrConfig, Privileges),
    ?OZ_AUTHS(HostOrConfig, Privileges) ++ ?ROOT_AUTHS(HostOrConfig)).

% authentication of a local onepanel user (role = regular)
-define(REGULAR_AUTHS(HostOrConfig), [
    {?REG_USER_NAME, ?REG_USER_PASSWORD},
    onepanel_test_rest:obtain_local_token(HostOrConfig, ?REG_USER_NAME, ?REG_USER_PASSWORD)
]).

-define(GUEST_AUTHS(), [none]).

-define(ALL_AUTHS(HostOrConfig),
    ?GUEST_AUTHS() ++ ?REGULAR_AUTHS(HostOrConfig) ++
        ?OZ_AUTHS(HostOrConfig, []) ++ ?OZ_OR_ROOT_AUTHS(HostOrConfig, privileges:cluster_admin())
).


-endif.
