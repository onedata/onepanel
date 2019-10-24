%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Integration tests for Ceph management REST endpoints.
%%% @end
%%%--------------------------------------------------------------------
-module(rest_ceph_test_SUITE).
-author("Wojciech Geisler").

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("modules/onepanel_dns.hrl").
-include("names.hrl").
-include("onepanel_test_rest.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    method_should_return_forbidden_error_test/1,
    without_ceph_method_should_return_not_found_error_test/1,
    method_should_return_not_found_error_test/1,
    get_should_return_pools_list_test/1,
    get_should_return_pool_details_test/1,
    get_should_return_mgr_list/1,
    get_should_return_mgr_details/1,
    get_should_return_mon_list/1,
    get_should_return_mon_details/1,
    get_should_return_cluster_data_usage/1,
    get_should_return_osd_data_usage/1,
    get_should_return_pool_data_usage/1,
    get_cluster_configuration_should_include_ceph/1,
    patch_should_modify_pool/1,
    post_should_deploy_ceph/1,
    post_should_deploy_oneprovider_with_ceph/1
]).

all() ->
    ?ALL([
        method_should_return_forbidden_error_test,
        without_ceph_method_should_return_not_found_error_test,
        method_should_return_not_found_error_test,
        get_should_return_pools_list_test,
        get_should_return_pool_details_test,
        get_should_return_mgr_list,
        get_should_return_mgr_details,
        get_should_return_mon_list,
        get_should_return_mon_details,
        get_should_return_cluster_data_usage,
        get_should_return_osd_data_usage,
        get_should_return_pool_data_usage,
        get_cluster_configuration_should_include_ceph,
        patch_should_modify_pool,
        post_should_deploy_ceph,
        post_should_deploy_oneprovider_with_ceph
    ]).


%%%===================================================================
%%% Test data
%%%===================================================================

-define(TIMEOUT, timer:seconds(5)).

-define(POOL_NAME, (<<"onedataPool">>)).
-define(POOL_SIZE, 2).
-define(POOL_MIN_SIZE, 1).

-define(MGR_ID, (<<"1">>)).
-define(MON_ID(Config), (list_to_binary(hd(?config(ceph_hosts, Config))))).
-define(MON_IP, (<<"1.2.3.4">>)).
-define(OSD_UUID1, <<"11111111-1111-1111-1111-111111111111">>).
-define(OSD1_ID, (<<"0">>)).
-define(OSD2_ID, (<<"1">>)).

% N - to differentiate variables
-define(KILOBYTES(N), 127 * N).
-define(BYTES(N), ?KILOBYTES(N) * 1024).

-define(CEPH_DF_OUTPUT, #{
    <<"pools">> => [
        #{
            <<"id">> => 1,
            <<"name">> => ?POOL_NAME,
            <<"stats">> =>
            #{<<"bytes_used">> => ?BYTES(1), <<"kb_used">> => ?KILOBYTES(1),
                <<"max_avail">> => ?BYTES(2), <<"objects">> => 0,
                <<"percent_used">> => 0.0}}
    ],
    <<"stats">> => #{
        <<"total_avail_bytes">> => ?BYTES(5),
        <<"total_bytes">> => ?BYTES(4),
        <<"total_used_bytes">> => ?BYTES(1)}
}).


-define(CEPH_OSD_DF_OUTPUT, #{
    <<"nodes">> => [
        #{<<"crush_weight">> => 0.164993, <<"depth">> => 2,
            <<"kb">> => ?KILOBYTES(3), <<"kb_avail">> => ?KILOBYTES(2),
            <<"kb_used">> => ?KILOBYTES(1), <<"name">> => <<"osd.0">>,
            <<"device_class">> => <<"hdd">>, <<"id">> => 0,
            <<"pgs">> => 67, <<"pool_weights">> => #{},
            <<"reweight">> => 1.0, <<"type">> => <<"osd">>,
            <<"type_id">> => 0, <<"utilization">> => 28.982829,
            <<"var">> => 1.0},
        #{<<"crush_weight">> => 0.164993, <<"depth">> => 2,
            <<"kb">> => ?KILOBYTES(6), <<"kb_avail">> => ?KILOBYTES(4),
            <<"kb_used">> => ?KILOBYTES(5), <<"name">> => <<"osd.1">>,
            <<"device_class">> => <<"hdd">>, <<"id">> => 1,
            <<"pgs">> => 128, <<"pool_weights">> => #{},
            <<"reweight">> => 1.0, <<"type">> => <<"osd">>,
            <<"type_id">> => 0, <<"utilization">> => 28.982818,
            <<"var">> => 1.0}
    ],
    <<"stray">> => [],
    <<"summary">> => #{
        <<"average_utilization">> => 28.982823, <<"dev">> => 5.0e-6,
        <<"max_var">> => 1.0, <<"min_var">> => 1.0,
        <<"total_kb_used">> => ?KILOBYTES(1),
        <<"total_kb_avail">> => ?KILOBYTES(2),
        <<"total_kb">> => ?KILOBYTES(3)
    }
}).


%%%===================================================================
%%% Test functions
%%%===================================================================

method_should_return_forbidden_error_test(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, 403, _, _}, onepanel_test_rest:auth_request(
            Host, <<Endpoint/binary>>, Method,
            ?OZ_AUTHS(Host, privileges:cluster_admin() -- [?CLUSTER_UPDATE])
        ))
    end, [
        {<<"/provider/ceph">>, post},
        {<<"/provider/ceph/managers">>, post},
        {<<"/provider/ceph/monitors">>, post},
        {<<"/provider/ceph/osds">>, post}
    ]).


without_ceph_method_should_return_not_found_error_test(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Host, <<Endpoint/binary>>, Method,
            ?OZ_OR_ROOT_AUTHS(Host, privileges:cluster_admin())
        ))
    end, [
        {<<"/provider/ceph/">>, get},
        {<<"/provider/ceph/managers">>, get},
        {<<"/provider/ceph/managers/someId">>, get},
        {<<"/provider/ceph/monitors">>, get},
        {<<"/provider/ceph/monitors/someId">>, get},
        {<<"/provider/ceph/osds">>, get},
        {<<"/provider/ceph/osds/someId">>, get},
        {<<"/provider/ceph/osds/someId/usage">>, get},
        {<<"/provider/ceph/pools">>, get},
        {<<"/provider/ceph/pools/someName">>, get},
        {<<"/provider/ceph/pools/someName">>, patch},
        {<<"/provider/ceph/pools/someName/usage">>, get},
        {<<"/provider/ceph/status">>, get},
        {<<"/provider/ceph/usage">>, get}
    ]).


method_should_return_not_found_error_test(Config) ->
    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        ?assertMatch({ok, 404, _, _}, onepanel_test_rest:auth_request(
            Host, <<Endpoint/binary>>, Method,
            ?OZ_OR_ROOT_AUTHS(Host, privileges:cluster_admin())
        ))
    end, [
        {<<"/provider/ceph/managers/nonexistentManagerId">>, get},
        {<<"/provider/ceph/monitors/nonexistentMonitorId">>, get},
        {<<"/provider/ceph/osds/nonexistentOsdId">>, get},
        {<<"/provider/ceph/osds/nonexistentOsdId/usage">>, get},
        {<<"/provider/ceph/pools/nonexistentPool">>, get},
        {<<"/provider/ceph/pools/nonexistentPool">>, patch},
        {<<"/provider/ceph/pools/nonexistentPool/usage">>, get}
    ]).


get_should_return_pools_list_test(Config) ->
    Expected = #{<<"pools">> => [#{
        <<"name">> => ?POOL_NAME,
        <<"copiesNumber">> => ?POOL_SIZE,
        <<"minCopiesNumber">> => ?POOL_MIN_SIZE
    }]},
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/pools">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_pool_details_test(Config) ->
    Expected = #{
        <<"name">> => ?POOL_NAME,
        <<"copiesNumber">> => ?POOL_SIZE,
        <<"minCopiesNumber">> => ?POOL_MIN_SIZE
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/pools/", (?POOL_NAME)/binary>>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_mgr_list(Config) ->
    CephHost = hd(?config(ceph_hosts, Config)),
    Expected = #{
        <<"managers">> => [
            #{
                <<"id">> => ?MGR_ID,
                <<"host">> => list_to_binary(CephHost)
            }
        ]
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/managers/">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_mgr_details(Config) ->
    CephHost = hd(?config(ceph_hosts, Config)),
    Expected = #{
        <<"id">> => ?MGR_ID,
        <<"host">> => list_to_binary(CephHost)
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/managers/", ?MGR_ID/binary>>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_mon_list(Config) ->
    CephHost = hd(?config(ceph_hosts, Config)),
    Expected = #{
        <<"monitors">> => [
            #{
                <<"id">> => ?MON_ID(Config),
                <<"host">> => list_to_binary(CephHost),
                <<"ip">> => ?MON_IP
            }
        ]
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/monitors/">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_mon_details(Config) ->
    CephHost = hd(?config(ceph_hosts, Config)),
    Expected = #{
        <<"id">> => ?MON_ID(Config),
        <<"host">> => list_to_binary(CephHost),
        <<"ip">> => ?MON_IP
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/monitors/", (?MON_ID(Config))/binary>>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_cluster_data_usage(Config) ->
    Expected = #{
        <<"total">> => #{
            <<"used">> => ?BYTES(1),
            <<"available">> => ?BYTES(5),
            <<"total">> => ?BYTES(4)
        },
        <<"osds">> => #{
            ?OSD1_ID => #{
                <<"used">> => ?BYTES(1),
                <<"available">> => ?BYTES(2),
                <<"total">> => ?BYTES(3)
            },
            ?OSD2_ID => #{
                <<"used">> => ?BYTES(5),
                <<"available">> => ?BYTES(4),
                <<"total">> => ?BYTES(6)
            }
        },
        <<"pools">> => #{
            ?POOL_NAME => #{
                <<"used">> => ?BYTES(1),
                <<"maxAvailable">> => ?BYTES(2)
            }
        }
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/usage">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_osd_data_usage(Config) ->
    Expected = #{
        <<"used">> => ?BYTES(1),
        <<"available">> => ?BYTES(2),
        <<"total">> => ?BYTES(3)
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/osds/", ?OSD1_ID/binary, "/usage">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_should_return_pool_data_usage(Config) ->
    Expected = #{
        <<"used">> => ?BYTES(1),
        <<"maxAvailable">> => ?BYTES(2)
    },
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/ceph/pools/", ?POOL_NAME/binary, "/usage">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        onepanel_test_rest:assert_body(JsonBody, Expected)
    end).


get_cluster_configuration_should_include_ceph(Config) ->
    ?eachHost(Config, fun(Host) ->
        {ok, _, _, JsonBody} = ?assertMatch({ok, 200, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/configuration">>, get,
                ?OZ_OR_ROOT_AUTHS(Config, [])
            )),
        ?assertMatch(#{
            <<"ceph">> := #{
                <<"osds">> := [],
                <<"managers">> := [#{}],
                <<"monitors">> := [#{}]
            }
        }, json_utils:decode(JsonBody))
    end).


post_should_deploy_ceph(Config) ->
    [Host1, Host2 | _] = ?config(ceph_hosts, Config),
    %% Endpoints /ceph, /ceph/osds, /ceph/managers, /ceph/monitors
    %% all use the same input format, filtering relevant parts
    ReqBody = #{
        <<"fsid">> => <<"customClusterUUID">>,
        <<"name">> => <<"customClusterName">>,
        <<"osds">> => [#{
            <<"type">> => <<"blockdevice">>,
            <<"device">> => <<"/dev/sdx">>,
            <<"host">> => list_to_binary(Host1),
            <<"uuid">> => ?OSD_UUID1
        }],
        <<"monitors">> => [#{
            <<"host">> => list_to_binary(Host2),
            <<"ip">> => <<"1.2.3.4">>
        }],
        <<"managers">> => [#{
            <<"host">> => list_to_binary(Host2)
        }]
    },

    OsdsCtx = #{osds => [#{
        type => blockdevice,
        device => <<"/dev/sdx">>,
        host => Host1,
        uuid => ?OSD_UUID1
    }]},
    MonsCtx = #{monitors => [#{
        host => Host2,
        ip => <<"1.2.3.4">>
    }]},
    ManagersCtx = #{managers => [#{
        host => Host2
    }]},
    FullCtx = lists:foldl(fun maps:merge/2,
        #{fsid => <<"customClusterUUID">>, cluster_name => <<"customClusterName">>},
        [OsdsCtx, MonsCtx, ManagersCtx]),

    EndpointToCtx = #{
        <<"ceph">> => FullCtx,
        <<"ceph/osds">> => OsdsCtx,
        <<"ceph/monitors">> => MonsCtx,
        <<"ceph/managers">> => ManagersCtx
    },

    ?eachEndpoint(Config, fun(Host, Endpoint, Method) ->
        Expected = maps:get(Endpoint, EndpointToCtx),
        Auths = ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
        {_, _, Headers, _} = ?assertMatch({ok, ?HTTP_201_CREATED, _, _},
            onepanel_test_rest:auth_request(
                Host, <<"/provider/", Endpoint/binary>>, Method, Auths, ReqBody
            )
        ),
        assert_async_task(Headers),
        % request is repeated for each Auth, do same number of receives
        lists:foreach(fun(_) ->
            {_, _, _, Ctx} = ?assertReceivedMatch({service, ceph, deploy, _}, ?TIMEOUT),
            ?assertEqual(Expected, Ctx)
        end, Auths)
    end, [{Endpoint, post} || Endpoint <- maps:keys(EndpointToCtx)]).


post_should_deploy_oneprovider_with_ceph(Config) ->
    [Host1, Host2 | _] = ?config(ceph_hosts, Config),
    %% Endpoints /ceph, /ceph/osds, /ceph/managers, /ceph/monitors
    %% all use the same input format, filtering relevant parts
    ReqBody = #{
        <<"cluster">> => #{
            <<"nodes">> => #{
                <<"n1">> => #{<<"hostname">> => list_to_binary(Host1)}},
            <<"databases">> => #{
                <<"nodes">> => [<<"n1">>]
            },
            <<"managers">> => #{
                <<"mainNode">> => <<"n1">>,
                <<"nodes">> => [<<"n1">>]
            },
            <<"workers">> => #{
                <<"nodes">> => [<<"n1">>]
            }
        },
        <<"ceph">> => #{
            % only Ceph uses Host2, to make sure it will be included
            % in hosts to be clustered anyway
            <<"fsid">> => <<"customClusterUUID">>,
            <<"name">> => <<"customClusterName">>,
            <<"osds">> => [#{
                <<"type">> => <<"blockdevice">>,
                <<"device">> => <<"/dev/sdx">>,
                <<"host">> => list_to_binary(Host2),
                <<"uuid">> => ?OSD_UUID1
            }],
            <<"monitors">> => [#{
                <<"host">> => list_to_binary(Host2),
                <<"ip">> => <<"1.2.3.4">>
            }],
            <<"managers">> => [#{
                <<"host">> => list_to_binary(Host2)
            }]
        },
        <<"oneprovider">> => #{
            <<"register">> => true,
            <<"name">> => <<"someName">>,
            <<"subdomainDelegation">> => false,
            <<"domain">> => <<"someDomain">>,
            <<"adminEmail">> => <<"admin@onedata.org">>
        }
    },

    Auths = ?OZ_OR_ROOT_AUTHS(Host1, [?CLUSTER_UPDATE]),
    {_, _, Headers, _} = ?assertMatch({ok, ?HTTP_201_CREATED, _, _},
        onepanel_test_rest:auth_request(
            Host1, <<"/provider/configuration">>, post, Auths, ReqBody
        )
    ),
    assert_async_task(Headers),
    SortedHosts = lists:usort([Host1, Host2]),
    lists:foreach(fun(_) ->
        {_, _, _, Ctx} = ?assertReceivedMatch({service, oneprovider, deploy, _}, ?TIMEOUT),
        ?assertMatch(#{
            ceph := #{
                osds := [#{host := Host2}],
                monitors := [#{host := Host2}],
                managers := [#{host := Host2}]
            },
            cluster := #{
                onepanel := #{
                    hosts := SortedHosts
                }
            },
            oneprovider := #{}
        }, Ctx)
    end, Auths).



patch_should_modify_pool(Config) ->
    [Host | _] = ?config(ceph_hosts, Config),
    Auths = ?OZ_OR_ROOT_AUTHS(Host, [?CLUSTER_UPDATE]),
    ?assertMatch({ok, ?HTTP_204_NO_CONTENT, _, _},
        onepanel_test_rest:auth_request(
            Host, <<"/provider/ceph/pools/", ?POOL_NAME/binary>>, patch, Auths,
            #{<<"copiesNumber">> => 4, <<"minCopiesNumber">> => 3}
        )
    ),
    Expected = #{name => ?POOL_NAME, copiesNumber => 4, minCopiesNumber => 3},
    lists:foreach(fun(_) ->
        {_, _, _, Ctx} = ?assertReceivedMatch({service, ceph, modify_pool, _}, ?TIMEOUT),
        ?assertEqual(Expected, Ctx)
    end, Auths).



%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    ssl:start(),
    hackney:start(),
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        CephNodes = tl(?config(all_nodes, NewConfig2)),
        CephHosts = tl(?config(all_hosts, NewConfig2)),
        [{ceph_nodes, CephNodes}, {ceph_hosts, CephHosts} | NewConfig2]
    end,
    [{?LOAD_MODULES, [onepanel_test_rest]}, {?ENV_UP_POSTHOOK, Posthook}
        | Config].

init_per_testcase(without_ceph_method_should_return_not_found_error_test, Config) ->
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),
    ?call(Config, service, delete, [?SERVICE_CEPH]),
    Config;

init_per_testcase(Case, Config) when
    Case == get_should_return_pool_details_test;
    Case == get_should_return_pools_list_test ->
    Config2 = init_per_testcase(default, Config),
    CephNodes = ?config(ceph_nodes, Config2),
    CephHosts = ?config(ceph_hosts, Config2),

    test_utils:mock_expect(CephNodes, ceph_cli, get_pool_param, fun
        (?POOL_NAME, <<"size">>) -> {ok, ?POOL_SIZE};
        (?POOL_NAME, <<"min_size">>) -> {ok, ?POOL_MIN_SIZE}
    end),
    Config2;

init_per_testcase(Case, Config) when
    Case == get_should_return_mgr_details;
    Case == get_should_return_mgr_list ->

    Config2 = init_per_testcase(default, Config),
    CephHosts = ?config(ceph_hosts, Config),
    ?call(Config, service, update_ctx, [?SERVICE_CEPH_MGR, #{
        instances => #{?MGR_ID => #{id => ?MGR_ID, host => hd(CephHosts)}}
    }]),
    Config2;

init_per_testcase(Case, Config) when
    Case == get_should_return_mon_details;
    Case == get_should_return_mon_list ->

    Config2 = init_per_testcase(default, Config),
    CephHosts = ?config(ceph_hosts, Config2),
    MonId = ?MON_ID(Config2),
    ok = ?call(Config2, service, save, [#service{
        name = ?SERVICE_CEPH_MON, hosts = CephHosts, ctx = #{instances => #{
            MonId => #{id => MonId, host => hd(CephHosts), ip => ?MON_IP}
        }}}]),
    Config2;

init_per_testcase(get_cluster_configuration_should_include_ceph, Config) ->
    Config2 = init_per_testcase(default, Config),
    CephHosts = ?config(ceph_hosts, Config2),
    ?call(Config2, service, update_ctx, [?SERVICE_CEPH_MGR, #{
        instances => #{?MGR_ID => #{id => ?MGR_ID, host => hd(CephHosts)}}
    }]),
    ?call(Config2, service, update_ctx, [?SERVICE_CEPH_MON, #{
        instances => #{?MON_ID(Config2) => #{id => ?MON_ID(Config2), host => hd(CephHosts)}}
    }]),
    ?call(Config2, service, update_ctx, [?SERVICE_CEPH_OSD, #{
        instances => #{}
    }]),

    % make rest_replier:format_service_configuration work
    ?call(Config2, onepanel_deployment, set_marker, [?PROGRESS_CLUSTER]),
    lists:foreach(fun(Service) ->
        ?call(Config, service, create, [#service{name = Service,
            ctx = #{main_host => hd(?config(all_hosts, Config2))}}])
    end, [?SERVICE_OP, ?SERVICE_OPW, ?SERVICE_CM, ?SERVICE_CB]),
    Config2;


init_per_testcase(Case, Config) when
    Case == get_should_return_cluster_data_usage;
    Case == get_should_return_pool_data_usage;
    Case == get_should_return_osd_data_usage ->

    Config2 = init_per_testcase(default, Config),
    CephHosts = ?config(ceph_hosts, Config),
    CephNodes = ?config(ceph_nodes, Config2),

    ?call(Config, service, update_ctx, [?SERVICE_CEPH_OSD, #{
        instances => #{?OSD1_ID => #{id => ?OSD1_ID, host => hd(CephHosts)}}
    }]),

    test_utils:mock_expect(CephNodes, ceph_cli, osd_df,
        fun() -> ?CEPH_OSD_DF_OUTPUT end),
    test_utils:mock_expect(CephNodes, ceph_cli, df,
        fun() -> ?CEPH_DF_OUTPUT end),

    Config2;

init_per_testcase(Case, Config) when
    Case == post_should_deploy_ceph;
    Case == patch_should_modify_pool;
    Case == post_should_deploy_oneprovider_with_ceph ->
    Self = self(),
    Nodes = ?config(all_nodes, Config),
    Config2 = init_per_testcase(default, Config),
    case Case of
        patch_should_modify_pool -> ok;
        _ ->
            % when ?SERVICE_CEPH does not exists, 201 is expected at POST /provider/ceph
            % when ?PROGRESS_CLUSTER is not exists, 201 is expected at POST /provider/configuration
            ok = ?call(Config2, service, delete, [?SERVICE_CEPH]),
            ok = ?call(Config2, onepanel_deployment, unset_marker, [?PROGRESS_CLUSTER])
    end,
    test_utils:mock_expect(Nodes, service, apply_sync, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        [{task_finished, {service, action, ok}}]
    end),
    test_utils:mock_expect(Nodes, service, apply_async, fun(Service, Action, Ctx) ->
        Self ! {service, Service, Action, Ctx},
        <<"someTaskId">>
    end),
    Config2;

init_per_testcase(_Case, Config) ->
    % do not mock Ceph on first node to ensure calls are routed correctly
    % to nodes having ceph
    CephNodes = ?config(ceph_nodes, Config),
    CephHosts = ?config(ceph_hosts, Config),
    onepanel_test_rest:set_default_passphrase(Config),
    onepanel_test_rest:mock_token_authentication(Config),

    % trigger models setup
    ?call(Config, service_ceph, get_steps, [deploy, #{}]),
    ?call(Config, service, update,
        [?SERVICE_CEPH, #{hosts => CephHosts}]),

    test_utils:mock_new(CephNodes, [ceph_cli]),
    % required in almost any test for exists_resource
    test_utils:mock_expect(CephNodes, ceph_cli, list_pools, fun() ->
        [#{name => ?POOL_NAME, number => 123}]
    end),

    Config.


end_per_testcase(without_ceph_method_should_return_not_found_error_test, _Config) ->
    ok;

end_per_testcase(_Case, Config) ->
    CephNodes = ?config(ceph_nodes, Config),
    test_utils:mock_validate_and_unload(CephNodes, [ceph_cli]),
    test_utils:mock_unload(?config(all_nodes, Config)).


end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec assert_async_task(http_client:headers()) -> ok.
assert_async_task(Headers) ->
    onepanel_test_utils:assert_values(Headers, [
        {<<"location">>, <<"/api/v3/onepanel/tasks/someTaskId">>}
    ]).

