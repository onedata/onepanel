%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Oneprovider batch deployment.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_op_batch_deployment_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
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

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).

% Time caveat is required in temporary tokens
-define(DEFAULT_TEMP_CAVEAT_TTL, 36000).


%%%===================================================================
%%% Tests
%%%===================================================================


deploy_using_batch_config_test(Config) ->
    AdminUserId = oct_background:get_user_id(admin),
    RegistrationToken = create_provider_registration_token(AdminUserId),

    OpPanelNodes = ?config(op_panel_nodes, Config),
    [OpIpHost1, OpIpHost2] = OpIps = lists:map(fun ip_test_utils:get_node_ip/1, OpPanelNodes),
    OpPanelHosts = hosts:from_nodes(OpPanelNodes),

    OpPanelNode1 = hd(OpPanelNodes),
    panel_test_rpc:set_emergency_passphrase(OpPanelNode1, ?ONENV_EMERGENCY_PASSPHRASE),

    BatchConfig = #{
        <<"cluster">> => #{
            <<"nodes">> => lists:foldl(
                fun({Num, {Hostname, Ip}}, Acc) ->
                    Acc#{str_utils:format_bin("node-~B", [Num]) => #{
                        <<"hostname">> => str_utils:to_binary(Hostname),
                        <<"externalIp">> => ip_test_utils:encode_ip(Ip)
                    }}
                end,
                #{},
                lists_utils:enumerate(lists:zip(OpPanelHosts, OpIps))
            ),
            <<"managers">> => #{
                <<"mainNode">> => <<"node-1">>,
                <<"nodes">> => [<<"node-1">>]
            },
            <<"workers">> => #{
                <<"nodes">> => [<<"node-1">>]
            },
            <<"oneS3">> => #{
                <<"nodes">> => [<<"node-2">>]
            },
            <<"databases">> => #{
                <<"nodes">> => [<<"node-1">>]
            }
        },
        <<"oneprovider">> => #{
            <<"register">> => true,
            <<"token">> => RegistrationToken,

            <<"name">> => <<"krakow">>,
            <<"adminEmail">> => <<"admin@example.eu">>,
            <<"subdomainDelegation">> => true,
            <<"subdomain">> => <<"krakow">>,
            <<"letsEncryptEnabled">> => true
        }
    },

    OpRequestOpts = #{
        auth => root,
        hostname => ip_test_utils:encode_ip(OpIpHost1)
    },
    {ok, ?HTTP_202_ACCEPTED, _, Resp} = panel_test_rest:post(
        OpPanelNode1, <<"/provider/configuration">>, OpRequestOpts#{json => BatchConfig}
    ),
    TaskId = maps:get(<<"taskId">>, Resp),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, #{<<"status">> := <<"ok">>}},
        panel_test_rest:get(OpPanelNode1, <<"/tasks/", TaskId/binary>>, OpRequestOpts),
        ?AWAIT_DEPLOYMENT_READY_ATTEMPTS
    ),

    OneS3Port = panel_test_rpc:call(OpPanelNode1, service_ones3, get_port, []),
    ?assertMatch({ok, _}, gen_tcp:connect(OpIpHost2, OneS3Port, [], 10), ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_not_deployed",
        posthook = fun(NewConfig) ->
            % Requests should be made without cert verification as provider
            % domain is set/changed during deployment
            panel_test_rest:set_insecure_flag(),

            dns_test_utils:update_zone_subdomain_delegation(true),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
create_provider_registration_token(AdminUserId) ->
    Now = ozw_test_rpc:timestamp_seconds(),

    Token = ozw_test_rpc:create_user_temporary_token(?USER(AdminUserId), AdminUserId, #{
        <<"type">> => ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId),
        <<"caveats">> => [#cv_time{valid_until = Now + ?DEFAULT_TEMP_CAVEAT_TTL}]
    }),
    {ok, SerializedToken} = tokens:serialize(Token),
    SerializedToken.
