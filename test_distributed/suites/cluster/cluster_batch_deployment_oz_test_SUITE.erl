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
-module(cluster_batch_deployment_oz_test_SUITE).
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
-define(JOE_USERNAME, <<"joe">>).
-define(JOE_PASSWORD, <<"password">>).

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).


%%%===================================================================
%%% Tests
%%%===================================================================


deploy_using_batch_config_test(Config) ->
    [OzPanelNode] = ?config(oz_panel_nodes, Config),
    OzPanelHostIp = ip_test_utils:get_node_ip(OzPanelNode),
    [OzHost] = hosts:from_nodes([OzPanelNode]),
    OzDomain = get_zone_domain(OzPanelNode),

    panel_test_rpc:set_emergency_passphrase(OzPanelNode, ?ONENV_EMERGENCY_PASSPHRASE),

    deploy(Config, OzPanelNode, OzPanelHostIp, #{
        <<"cluster">> => #{
            <<"nodes">> => #{
                <<"node-1">> => #{
                    <<"hostname">> => str_utils:to_binary(OzHost),
                    <<"externalIp">> => ip_test_utils:encode_ip(OzPanelHostIp)
                }
            },
            <<"managers">> => #{
                <<"mainNode">> => <<"node-1">>,
                <<"nodes">> => [<<"node-1">>]
            },
            <<"workers">> => #{
                <<"nodes">> => [<<"node-1">>]
            },
            <<"databases">> => #{
                <<"nodes">> => [<<"node-1">>]
            }
        },
        <<"onezone">> => #{
            <<"name">> => ?OZ_NAME,
            % Submit uppercased domain to later assert it was downcased
            <<"domainName">> => string:uppercase(OzDomain),
            <<"users">> => [
                #{
                    <<"username">> => ?JOE_USERNAME,
                    <<"password">> => ?JOE_PASSWORD,
                    <<"groups">> => [<<"admins">>]
                }
            ]
        }
    }),

    ?assertMatch(
        #{<<"onezone">> := #{<<"domainName">> := OzDomain, <<"name">> := ?OZ_NAME, <<"configured">> := true}},
        get_zone_configuration()
    ),

    assert_users_created(),

    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz_not_deployed",
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
get_zone_domain(OzPanelNode) ->
    {ok, OzDomain} = test_utils:get_env(OzPanelNode, ?APP_NAME, test_web_cert_domain),
    str_utils:to_binary(OzDomain).


%% @private
deploy(Config, OzPanelNode, OzPanelHostIp, BatchConfig) ->
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
    ),

    update_oct_background(Config, OzPanelNode).


%% @private
update_oct_background(Config, OzPanelNode) ->
    % Connect with newly deployed workers and refresh oct_background to be able to use e.g. rpc
    OzWorkerNodes = panel_test_rpc:call(OzPanelNode, service_oz_worker, get_nodes, []),
    ConfigWithAllOzWorkerNodes = [{oz_worker_nodes, OzWorkerNodes} | Config],
    ConfigWithConnectedNodes = oct_environment:connect_with_nodes(ConfigWithAllOzWorkerNodes),
    oct_background:update_background_config(ConfigWithConnectedNodes),

    ok.


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
    ozw_test_rpc:are_basic_credentials_valid(?JOE_USERNAME, ?JOE_PASSWORD),

    % and both of them should belong to the admins group
    ?assertEqual(
        lists:sort(ozw_test_rpc:list_users()),
        lists:sort(ozw_test_rpc:get_group_users(<<"admins">>))
    ).
