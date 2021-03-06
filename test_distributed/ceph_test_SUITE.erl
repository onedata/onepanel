%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains integration tests of Ceph services.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_test_SUITE).
-author("Wojciech Geisler").

-include("names.hrl").
-include("modules/models.hrl").
-include("modules/errors.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, init_per_testcase/2,
    end_per_testcase/2, end_per_suite/1]).

%% tests
-export([
    localceph_storage_fails_without_ceph/1,
    ceph_is_deployed/1,
    monitor_is_added/1,
    manager_is_added/1,
    loopdevice_osd_is_added/1,
    blockdevice_osd_is_added/1,
    storage_is_added_with_pool/1,
    pool_creation_fails_with_too_few_osds/1,
    get_localceph_storage/1,
    storage_update_modifies_pool/1,
    storage_delete_removes_pool/1
]).

-define(TIMEOUT, timer:seconds(5)).
-define(ATTEMPTS, 5).

-define(PASSPHRASE, <<"passphrase">>).
-define(POOL_NAME, <<"some pool">>).
-define(POOL_NAME2, <<"otherPool">>).
-define(OSD_UUID1, <<"11111111-1111-1111-1111-111111111111">>).
-define(OSD_UUID2, <<"22222222-2222-2222-2222-222222222222">>).
-define(OSD_UUID3, <<"33333333-3333-3333-3333-333333333333">>).
-define(CUSTOM_LOOP_PATH, <<"/volumes/persistence/ceph-loopdevices/custompath.loop">>).
% path of loopdevice file used to test blockdevice type deployment
-define(MOCK_BLOCKDEVICE_PATH, <<"/volumes/persistence/ceph-loopdevices/blockdevice.loop">>).
-define(LOOPDEVICE_SIZE, 500 * 1024 * 1024).

-define(POOL_PARAMS, #{
    type => <<"localceph">>,
    clusterName => <<"ceph">>,
    poolName => <<"onedata">>,
    storagePathType => <<"flat">>,
    qosParameters => #{},
    lumaFeed => auto
}).


all() ->
    ?ALL([
        localceph_storage_fails_without_ceph,
        ceph_is_deployed,
        monitor_is_added,
        manager_is_added,
        loopdevice_osd_is_added,
        storage_is_added_with_pool,
        pool_creation_fails_with_too_few_osds,
        get_localceph_storage,
        storage_update_modifies_pool,
        storage_delete_removes_pool,
        blockdevice_osd_is_added
]).

%%%===================================================================
%%% Test functions
%%%===================================================================


localceph_storage_fails_without_ceph(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    PoolName = ?POOL_NAME,
    ?assertMatch({error, _}, onepanel_test_utils:attempt_service_action(
        OpNode, op_worker, add_storages, #{
            hosts => [hd(?config(oneprovider_hosts, Config))],
            storages => #{PoolName => ?POOL_PARAMS}
        })).


ceph_is_deployed(Config) ->
    [OpNode1, OpNode2 | _] = OpNodes = ?config(oneprovider_nodes, Config),
    [OpHost1, OpHost2 | _] = hosts:from_nodes(OpNodes),
    HostsBin = lists:sort(onepanel_utils:convert([OpHost1, OpHost2], {seq, binary})),
    onepanel_test_utils:service_action(OpNode1, ?SERVICE_CEPH, deploy, #{
        cluster_name => <<"test_cluster">>,
        monitors => [
            #{
                ip => test_utils:get_docker_ip(OpNode1),
                % a list!
                host => OpHost1
            }
            ,
            #{
                ip => test_utils:get_docker_ip(OpNode2),
                % a list!
                host => OpHost2
            }
        ],
        managers => [
            #{
                host => OpHost1
            },
            #{
                host => OpHost2
            }
        ],
        osds => [
            #{
                type => loopdevice,
                host => OpHost1,
                size => ?LOOPDEVICE_SIZE,
                uuid => ?OSD_UUID1 % UUID is always sent by user or filled in ceph_middleware
            }
        ]
    }),
    ?assertEqual(HostsBin, list_monitors(OpNode1), ?ATTEMPTS),
    ?assertEqual(HostsBin, list_managers(OpNode1), ?ATTEMPTS),
    ?assertEqual([<<"0">>], list_osds(OpNode1), ?ATTEMPTS).


monitor_is_added(Config) ->
    [OpNode, _, OpNode3 | _] = ?config(oneprovider_nodes, Config),
    [_, _, OpHost3 | _] = ?config(oneprovider_hosts, Config),
    onepanel_test_utils:service_action(OpNode, ?SERVICE_CEPH, deploy, #{
        monitors => [
            #{
                ip => test_utils:get_docker_ip(OpNode3),
                % a list!
                host => OpHost3
            }
        ]
    }),
    MonIds = ?assertMatch([_, _, _], list_monitors(OpNode), ?ATTEMPTS),
    ?assertEqual(MonIds, lists:sort(maps:keys(
        get_instances(OpNode, ?SERVICE_CEPH_MON)
    ))).


manager_is_added(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    [_, _, OpHost3 | _] = ?config(oneprovider_hosts, Config),
    onepanel_test_utils:service_action(OpNode, ?SERVICE_CEPH, deploy, #{
        managers => [
            #{
                host => OpHost3
            }
        ]
    }),
    MgrIds = ?assertMatch([_, _, _], list_managers(OpNode), ?ATTEMPTS),
    ?assertEqual(3, length(MgrIds)),
    ?assertEqual(MgrIds, lists:sort(maps:keys(
        get_instances(OpNode, ?SERVICE_CEPH_MGR)
    ))).

loopdevice_osd_is_added(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    [_, _, OpHost3 | _] = ?config(oneprovider_hosts, Config),

    onepanel_test_utils:service_action(OpNode, ?SERVICE_CEPH, deploy, #{
        osds => [
            #{
                host => OpHost3,
                type => loopdevice,
                path => ?CUSTOM_LOOP_PATH,
                size => ?LOOPDEVICE_SIZE,
                uuid => ?OSD_UUID2
            }
        ]
    }),
    OsdIds = ?assertMatch([<<"0">>, <<"1">>], list_osds(OpNode), ?ATTEMPTS),
    ?assertEqual(OsdIds, lists:sort(maps:keys(
        get_instances(OpNode, ?SERVICE_CEPH_OSD)
    ))).


blockdevice_osd_is_added(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    Device = ?config(blockdevice, Config), % vgroup/lvolume
    [_, _, OpHost3 | _] = ?config(oneprovider_hosts, Config),

    onepanel_test_utils:service_action(OpNode, ?SERVICE_CEPH, deploy, #{
        osds => [
            #{
                host => OpHost3,
                type => blockdevice,
                uuid => ?OSD_UUID3,
                device => Device
            }
        ]
    }),
    OsdIds = ?assertMatch([<<"0">>, <<"1">>, <<"2">>], list_osds(OpNode), ?ATTEMPTS),
    ?assertEqual(OsdIds, lists:sort(maps:keys(
        get_instances(OpNode, ?SERVICE_CEPH_OSD)
    ))).


storage_is_added_with_pool(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    PoolName = ?POOL_NAME,
    onepanel_test_utils:service_action(OpNode, op_worker, add_storages, #{
        hosts => [hd(?config(oneprovider_hosts, Config))],
        storages => #{PoolName => ?POOL_PARAMS}
    }),
    ?assertEqual([PoolName], rpc:call(OpNode, ceph_pool, list, [])),
    ?assertEqual(1,
        length(rpc:call(OpNode, service_op_worker, get_storages, [#{}]))).


pool_creation_fails_with_too_few_osds(Config) ->
    [OpNode | _] = ?config(oneprovider_nodes, Config),
    PoolName = ?POOL_NAME2,
    ?assertMatch({error, _}, onepanel_test_utils:attempt_service_action(
        OpNode, op_worker, add_storages, #{
            hosts => [hd(?config(oneprovider_hosts, Config))],
            storages => #{PoolName => ?POOL_PARAMS#{
                copiesNumber => 3
            }}
        })),
    ?assertMatch({error, _}, onepanel_test_utils:attempt_service_action(
        OpNode, op_worker, add_storages, #{
            hosts => [hd(?config(oneprovider_hosts, Config))],
            storages => #{PoolName => ?POOL_PARAMS#{
                minCopiesNumber => 3
            }}
        })).


get_localceph_storage(Config) ->
    % select a node without ceph, it should still work
    Node = lists:nth(4, ?config(oneprovider_nodes, Config)),
    [Id] = ?assertMatch([_], list_storage_ids(Node)),
    Storage = get_storage(Node, Id),
    onepanel_test_utils:assert_values(Storage, [
        {id, Id},
        {name, ?POOL_NAME},
        {type, ?LOCAL_CEPH_STORAGE_TYPE},
        {copiesNumber, 2},
        {minCopiesNumber, 2}
    ]).


storage_update_modifies_pool(Config) ->
    % select a node without ceph, it should still work
    Node = lists:nth(4, ?config(oneprovider_nodes, Config)),
    [Id] = ?assertMatch([_], list_storage_ids(Node)),

    NewTimeout = 300,
    Changes = #{id => Id, storage => #{
        type => ?LOCAL_CEPH_STORAGE_TYPE, name => ?POOL_NAME,
        copiesNumber => 1, minCopiesNumber => 1, timeout => NewTimeout
    }},
    onepanel_test_utils:service_action(Node, op_worker, update_storage, Changes),

    Storage = get_storage(Node, Id),
    onepanel_test_utils:assert_values(Storage, [
        {id, Id},
        {name, ?POOL_NAME},
        {type, ?LOCAL_CEPH_STORAGE_TYPE},
        {copiesNumber, 1},
        {minCopiesNumber, 1},
        {timeout, integer_to_binary(NewTimeout)}
    ]).


storage_delete_removes_pool(Config) ->
    % select a node without ceph, it should still work
    Node = lists:nth(4, ?config(oneprovider_nodes, Config)),
    [Id] = ?assertMatch([_], list_storage_ids(Node)),

    onepanel_test_utils:service_action(Node, op_worker, remove_storage, #{id => Id}),

    ?assertMatch([], list_storage_ids(Node)),
    ?assertEqual([], rpc:call(Node, ceph_pool, list, [])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        NewConfig2 = onepanel_test_utils:init(NewConfig),
        NewConfig3 = image_test_utils:deploy_onezone(?PASSPHRASE,
            <<"admin">>, <<"password">>, 2, NewConfig2),

        image_test_utils:deploy_oneprovider(?PASSPHRASE, _Storages = #{}, NewConfig3)
    end,
    [{?ENV_UP_POSTHOOK, Posthook} | Config].


init_per_testcase(blockdevice_osd_is_added, Config) ->
    % since it's hard to obtain a true block device in arbitrary test
    % environment, the bluestore deployment is also tested using a loopdevice,
    % except the loopdevice is created by the test here rather than onepanel.
    Config2 = init_per_testcase(default, Config),
    [_, _, OpHost3 | _] = ?config(oneprovider_hosts, Config),
    OpNode3 = nodes:service_to_node(?SERVICE_PANEL, OpHost3),

    % code based on service_ceph_osd:prepare_loopdevice/1
    Size = ?LOOPDEVICE_SIZE,
    UUID = ?OSD_UUID3,
    Path = ?MOCK_BLOCKDEVICE_PATH,
    Loop = ?assertMatch(<<_/binary>>,
        rpc:call(OpNode3, loopdevice, ensure_loopdevice, [Path, Size])),
    GroupName = gen_vgroup_name(UUID),
    VolumeName = gen_lvolume_name(UUID),

    ?assertEqual(ok, rpc:call(OpNode3, lvm, create_physical_volume, [Loop])),
    ?assertEqual(ok, rpc:call(OpNode3, lvm, create_volume_group, [GroupName, [Loop]])),
    ?assertEqual(ok, rpc:call(OpNode3, lvm, create_logical_volume, [VolumeName, GroupName])),

    Device = <<GroupName/binary, "/", VolumeName/binary>>,
    [{blockdevice, Device} | Config2];

init_per_testcase(_Case, Config) ->
    onepanel_test_utils:clear_msg_inbox(),
    Config.


end_per_testcase(_Case, _Config) ->
    ok.

end_per_suite(Config) ->
    OpNodes = ?config(oneprovider_nodes, Config),
    % disable all created lvm groups and detach loopdevices.
    % No assserts of success since it's more important to traverse all nodes
    % and attempt all devices than to fail early.
    lists:foreach(fun(Node) ->
        rpc:call(Node, onepanel_shell, execute, [["pkill", "-9", "ceph-osd"]]),
        lists:foreach(fun(UUID) ->
            LoopFile = <<"/volumes/persistence/ceph-loopdevices/osd-", UUID/binary, ".loop">>,
            rpc:call(Node, lvm, disable_volume_group, [<<"osd-", UUID/binary>>]),
            rpc:call(Node, lvm, remove_volume_group, [<<"osd-", UUID/binary>>]),

            % eagerly list all loop devices which may have been created during the test,
            % regardless which node is currently processed
            LoopDevices = rpc:call(Node, loopdevice, list_loopdevices, [LoopFile]) ++
                rpc:call(Node, loopdevice, list_loopdevices, [?CUSTOM_LOOP_PATH]) ++
                rpc:call(Node, loopdevice, list_loopdevices, [?MOCK_BLOCKDEVICE_PATH]),

            lists:foreach(fun(LoopDevice) ->
                rpc:call(Node, lvm, remove_physical_volume, [LoopDevice]),
                rpc:call(Node, loopdevice, detach_loopdevice, [LoopDevice])
            end, LoopDevices)
        end, [?OSD_UUID1, ?OSD_UUID2, ?OSD_UUID3])
    end, OpNodes).


%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec list_monitors(Node :: node()) -> [binary()].
list_monitors(Node) ->
    lists:sort(rpc:call(Node, service_ceph_mon, list_quorum, [])).


-spec list_managers(Node :: node()) -> [binary()].
list_managers(Node) ->
    lists:sort(rpc:call(Node, service_ceph_mgr, list_running, [])).


-spec list_osds(Node :: node()) -> [binary()].
list_osds(Node) ->
    #{<<"nodes">> := Nodes} = rpc:call(Node, ceph_cli, osd_df, []),
    List = [integer_to_binary(Id) || #{<<"id">> := Id} <- Nodes],
    lists:sort(List).


-spec get_instances(Node :: node(), Service :: service:name()) ->
    #{service_ceph_mon:id() => map()}.
get_instances(Node, Service) ->
    Result = ?assertMatch(#{instances := _},
        rpc:call(Node, service, get_ctx, [Service])),
    maps:get(instances, Result).


-spec list_storage_ids(node()) -> [op_worker_storage:id()].
list_storage_ids(Node) ->
    Ctx = #{hosts => [hosts:from_node(Node)]},
    onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx),
    ?assertMatch(List when is_list(List), rpc:call(Node,
        middleware_utils, result_from_service_action,
        [?SERVICE_OPW, get_storages]
    )).


-spec get_storage(node(), op_worker_storage:id()) -> map().
get_storage(Node, Id) ->
    Ctx = #{id => Id, hosts => [hosts:from_node(Node)]},
    onepanel_test_utils:service_action(Node, op_worker, get_storages, Ctx),
    rpc:call(Node, middleware_utils, result_from_service_action,
        [?SERVICE_OPW, get_storages, Ctx]).


%% @private
-spec gen_vgroup_name(ceph:uuid()) -> binary().
gen_vgroup_name(UUID) ->
    <<"osd-", UUID/binary>>.


%% @private
-spec gen_lvolume_name(ceph:uuid()) -> binary().
gen_lvolume_name(UUID) ->
    <<"osd-data-", UUID/binary>>.
