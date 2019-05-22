%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Macros for generating user credentials in REST tests.
%%% @end
%%%--------------------------------------------------------------------
-author("Wojciech Geisler").

-ifndef(ONEPANEL_TEST_REST_HRL).
-define(ONEPANEL_TEST_REST_HRL, 1).


-define(OZ_USER_NAME, <<"joe">>).

-define(EMERGENCY_PASSPHRASE, <<"emergencyPassphrase">>).


% Basic auth and auth by session token
-define(LOCAL_AUTHS(HostOrConfig, BasicOrPassphrase), [
    BasicOrPassphrase,
    onepanel_test_rest:obtain_local_token(HostOrConfig, BasicOrPassphrase)
]).

-define(OZ_AUTHS(HostOrConfig, Privileges), [
    onepanel_test_rest:oz_token_auth(<<"privileged">>, Privileges)
]).


-define(ROOT_AUTHS(HostOrConfig),
    ?LOCAL_AUTHS(HostOrConfig, ?EMERGENCY_PASSPHRASE)).

-define(OZ_OR_ROOT_AUTHS(HostOrConfig, Privileges),
    ?OZ_AUTHS(HostOrConfig, Privileges) ++ ?ROOT_AUTHS(HostOrConfig)).

-define(INCORRECT_AUTHS(), [
    <<"badPassphrase">>,
    {<<"badUsername">>, <<"badPassphrase">>},
    {token, <<"badToken">>}
]).

-define(NONE_AUTHS(), [none]).

-define(ALL_AUTHS(HostOrConfig),
    ?NONE_AUTHS() ++
        ?OZ_AUTHS(HostOrConfig, []) ++
        ?OZ_OR_ROOT_AUTHS(HostOrConfig, privileges:cluster_admin())
).


-endif.
