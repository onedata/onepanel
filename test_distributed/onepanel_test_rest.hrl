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

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

-define(OZ_USER_NAME, <<"joe">>).
-define(EMERGENCY_PASSPHRASE, <<"emergencyPassphrase">>).

% Basic auth and auth by session token
-define(LOCAL_AUTHS(HostOrConfig, BasicOrPassphrase), [
    BasicOrPassphrase,
    onepanel_test_rest:obtain_local_token(HostOrConfig, BasicOrPassphrase)
]).

-define(PEER_AUTHS(Host), [
    onepanel_test_rest:obtain_invite_token(Host)
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


-define(eachHost(Config, Fun), lists:foreach(Fun, ?config(all_hosts, Config))).

-define(eachEndpoint(Config, Fun, EndpointsWithMethods),
    lists:foreach(fun({__Host, __Endpoint, __Method}) ->
        try
            Fun(__Host, __Endpoint, __Method)
        catch
            error:__Reason ->
                ct:pal("Failed on: ~ts ~ts (host ~ts)", [__Method, __Endpoint, __Host]),
                erlang:error(__Reason)
        end
    end, [
        {__Host, __Endpoint, __Method} ||
        {__Endpoint, __Method} <- EndpointsWithMethods,
        __Host <- ?config(all_hosts, Config)
    ])
).


% TaskId should be "a double-quote string literal"
-define(assertAsyncTask(TaskId, Response),
    ?assertMatch({
        ok, ?HTTP_202_ACCEPTED,
        #{?HDR_LOCATION := <<"/api/v3/onepanel/tasks/", TaskId>>},
        <<"{\"taskId\":\"", TaskId, "\"}">>
    }, Response)
).

-endif.
