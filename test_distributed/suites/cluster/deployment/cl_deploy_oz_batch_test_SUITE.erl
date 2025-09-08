%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone batch deployment.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_deploy_oz_batch_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    deploy_using_batch_config_test/1
]).

all() -> [
    deploy_using_batch_config_test
].

-define(OZ_NAME, <<"zone">>).

-define(ADMIN_USERNAME, <<"admin">>).
-define(NON_ADMIN_USERNAME, <<"joe">>).
-define(NON_ADMIN_PASSWORD, <<"password">>).

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).


%%%===================================================================
%%% Tests
%%%===================================================================


deploy_using_batch_config_test(Config) ->
    OzPanelNodes = [OzPanelNode1, _] = ?config(oz_panel_nodes, Config),
    [OzIpHost1, OzIpHost2] = lists:map(fun ip_test_utils:get_node_ip/1, OzPanelNodes),
    [OzHost1, OzHost2] = hosts:from_nodes(OzPanelNodes),
    OzDomain = dns_test_utils:get_k8s_service_domain(OzPanelNode1),

    panel_test_rpc:set_emergency_passphrase(OzPanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    cluster_management_test_utils:assert_onedata_service_domain(zone, undefined),

    deploy(OzPanelNode1, OzIpHost1, #{
        <<"cluster">> => #{
            <<"nodes">> => #{
                <<"node-1">> => #{
                    <<"hostname">> => str_utils:to_binary(OzHost1),
                    <<"externalIp">> => ip_test_utils:encode_ip(OzIpHost1)
                },
                <<"node-2">> => #{
                    <<"hostname">> => str_utils:to_binary(OzHost2),
                    <<"externalIp">> => ip_test_utils:encode_ip(OzIpHost2)
                }
            },
            <<"managers">> => #{
                <<"mainNode">> => <<"node-1">>,
                <<"nodes">> => [<<"node-1">>]
            },
            % Workers are not placed on all nodes to assert e.g. proper onedata_service env setup
            % (2 panel nodes and only 1 worker node)
            <<"workers">> => #{
                <<"nodes">> => [<<"node-1">>]
            },
            <<"databases">> => #{
                <<"nodes">> => [<<"node-2">>]
            }
        },
        <<"onezone">> => #{
            <<"name">> => ?OZ_NAME,
            % Submit uppercased domain to later assert it was downcased
            <<"domainName">> => string:uppercase(OzDomain),
            <<"users">> => [
                #{
                    <<"username">> => ?NON_ADMIN_USERNAME,
                    <<"password">> => ?NON_ADMIN_PASSWORD,
                    <<"groups">> => [<<"admins">>]
                }
            ]
        }
    }),
    cluster_management_test_utils:refresh_oct(Config),

    ?assertMatch(
        #{<<"onezone">> := #{<<"domainName">> := OzDomain, <<"name">> := ?OZ_NAME, <<"configured">> := true}},
        get_zone_configuration()
    ),
    cluster_management_test_utils:assert_onedata_service_domain(zone, OzDomain),
    assert_users_created(),

    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz_2nodes_not_deployed",
        posthook = fun(NewConfig) ->
            % Requests should be made without cert verification as provider
            % domain is set/changed during deployment
            panel_test_rest:set_insecure_flag(),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
deploy(OzPanelNode, OzPanelHostIp, BatchConfig) ->
    OzRequestOpts = #{
        auth => root,
        % zone is not deployed yet and as such requests must be made using host ip
        hostname => ip_test_utils:encode_ip(OzPanelHostIp)
    },
    {ok, ?HTTP_202_ACCEPTED, _, Resp} = panel_test_rest:post(
        OzPanelNode, <<"/zone/configuration">>, OzRequestOpts#{json => BatchConfig}
    ),
    TaskId = maps:get(<<"taskId">>, Resp),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, #{<<"status">> := <<"ok">>}},
        panel_test_rest:get(OzPanelNode, <<"/tasks/", TaskId/binary>>, OzRequestOpts),
        ?AWAIT_DEPLOYMENT_READY_ATTEMPTS
    ).


%% @private
get_zone_configuration() ->
    {ok, _, _, Details} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(zone, <<"/zone/configuration">>, #{auth => root})
    ),
    Details.


%% @private
assert_users_created() ->
    % Only default admin and users listed in batch config should exist
    ?assertEqual(2, length(ozw_test_rpc:list_users())),

    ozw_test_rpc:are_basic_credentials_valid(?ADMIN_USERNAME, ?ONENV_EMERGENCY_PASSPHRASE),
    ozw_test_rpc:are_basic_credentials_valid(?NON_ADMIN_USERNAME, ?NON_ADMIN_PASSWORD),

    % and both of them should belong to the admins group
    ?assertEqual(
        lists:sort(ozw_test_rpc:list_users()),
        lists:sort(ozw_test_rpc:get_group_users(<<"admins">>))
    ).
