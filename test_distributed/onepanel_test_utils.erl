%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains common functions for integration tests.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_test_utils).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([init/1, ensure_started/1, set_test_envs/1, set_test_envs/2,
    mock_start/1]).
-export([assert_fields/2, assert_values/2, clear_msg_inbox/0]).

-type config() :: proplists:proplist().

-define(TEST_ENVS, [
    {cluster_manager_vm_args_file, "/etc/cluster_manager/vm.args"},
    {cluster_manager_app_config_file, "/etc/cluster_manager/app.config"},
    {cluster_manager_env_file, "/usr/lib/cluster_manager/lib/env.sh"},
    {op_worker_vm_args_file, "/etc/op_worker/vm.args"},
    {op_worker_app_config_file, "/etc/op_worker/app.config"},
    {oz_worker_vm_args_file, "/etc/oz_worker/vm.args"},
    {oz_worker_app_config_file, "/etc/oz_worker/app.config"},
    {services_check_delay, timer:hours(1)}
]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes onepanel clusters. If a domain name of a test node
%% contains 'oneprovider' segment it will be added to the oneprovider release
%% type of a onepanel cluster. Similarly, if a domain name of a test node
%% contains 'onezone' segment it will be added to the onezone release
%% type of a onepanel cluster. Rest of the test nodes will be added to a cluster
%% having a default release type.
%% @end
%%--------------------------------------------------------------------
-spec init(Config :: config()) -> Config :: config().
init(Config) ->
    ensure_started(Config),
    Nodes = ?config(onepanel_nodes, Config),
    ProviderNodes = filter_nodes(oneprovider, Nodes),
    ZoneNodes = filter_nodes(onezone, Nodes),
    PanelNodes = (Nodes -- ProviderNodes) -- ZoneNodes,
    set_test_envs(Nodes),
    create_cluster(ProviderNodes),
    create_cluster(ZoneNodes),
    create_cluster(PanelNodes),
    NewConfig = [
        {all_nodes, Nodes},
        {oneprovider_nodes, ProviderNodes},
        {oneprovider_hosts, onepanel_cluster:nodes_to_hosts(ProviderNodes)},
        {onezone_nodes, ZoneNodes},
        {onezone_hosts, onepanel_cluster:nodes_to_hosts(ZoneNodes)},
        {onepanel_nodes, PanelNodes},
        {onepanel_hosts, onepanel_cluster:nodes_to_hosts(PanelNodes)},
        {letsencrypt_nodes, PanelNodes},
        {letsencrypt_hosts, onepanel_cluster:nodes_to_hosts(PanelNodes)} |
        lists:keydelete(onepanel_nodes, 1, Config)
    ],
    ensure_started(NewConfig).


%%--------------------------------------------------------------------
%% @doc Waits until onepanel application is started on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec ensure_started(Config :: config()) -> Config :: config().
ensure_started(Config) ->
    Nodes = proplists:get_value(all_nodes, Config,
        proplists:get_value(onepanel_nodes, Config)),
    lists:foreach(fun(Node) ->
        ?assertEqual(ok, rpc:call(Node, onepanel_utils, wait_until,
            [application, ensure_started, [?APP_NAME], {equal, ok}, 30]))
    end, Nodes),
    Config.


%%--------------------------------------------------------------------
%% @equiv set_test_envs(Nodes, ?TEST_ENVS)
%% @end
%%--------------------------------------------------------------------
-spec set_test_envs(Nodes :: [node()]) -> ok.
set_test_envs(Nodes) ->
    set_test_envs(Nodes, ?TEST_ENVS).


%%--------------------------------------------------------------------
%% @doc Overwrites the default application variables for the test purposes.
%% @end
%%--------------------------------------------------------------------
-spec set_test_envs(Nodes :: [node()], TestEnvs :: proplists:proplist()) -> ok.
set_test_envs(Nodes, TestEnvs) ->
    lists:foreach(fun({Key, Value}) ->
        rpc:multicall(Nodes, onepanel_env, set, [Key, Value])
    end, TestEnvs).


%%--------------------------------------------------------------------
%% @doc Starts meck application and its dependencies on the test nodes.
%% @end
%%--------------------------------------------------------------------
-spec mock_start(Config :: config()) -> Config :: config().
mock_start(Config) ->
    Nodes = ?config(all_nodes, Config),
    lists:foreach(fun(App) ->
        {Results, []} = ?assertMatch({_, []},
            rpc:multicall(Nodes, application, start, [App])),
        ?assert(lists:all(fun(Result) -> Result =:= ok end, Results))
    end, [tools, meck]),
    Config.


%%--------------------------------------------------------------------
%% @doc Checks whether the tuple list contains specified fields.
%% @end
%%--------------------------------------------------------------------
-spec assert_fields(KeyValue :: [tuple()] | maps:map() , Fields :: [binary()]) -> ok.
assert_fields(Map = #{}, Fields) ->
    assert_fields(maps:to_list(Map), Fields);
assert_fields(TupleList, Fields) ->
    lists:foreach(fun(Field) ->
        ?assert(lists:keymember(Field, 1, TupleList))
    end, Fields).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body contains specified fields and their
%% values match the expected ones.
%% @end
%%--------------------------------------------------------------------
-spec assert_values(KeyValue :: [tuple()] | maps:map(), Values :: [{binary(), any()}]) -> ok.
assert_values(Map = #{}, Values) ->
    assert_values(maps:to_list(Map), Values);
assert_values(TupleList, Values) ->
    lists:foreach(fun({Field, Value}) ->
        ?assert(lists:keymember(Field, 1, TupleList)),
        ?assertEqual({Field, Value}, lists:keyfind(Field, 1, TupleList))
    end, Values).


%%--------------------------------------------------------------------
%% @doc Removes all messages from calling process message inbox.
%% @end
%%--------------------------------------------------------------------
-spec clear_msg_inbox() -> ok.
clear_msg_inbox() ->
    receive
        _ -> clear_msg_inbox()
    after
        timer:seconds(1) -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns a list of nodes that have the provided segment in their
%% domain name.
%% @end
%%--------------------------------------------------------------------
-spec filter_nodes(Segment :: atom(), Nodes :: [node()]) -> FilteredNodes :: [node()].
filter_nodes(Segment, Nodes) ->
    lists:filter(fun(Node) ->
        Host = onepanel_cluster:node_to_host(Node),
        lists:member(erlang:atom_to_list(Segment), string:tokens(Host, "."))
    end, Nodes).


%%--------------------------------------------------------------------
%% @private @doc Deploys onepanel cluster on the provided nodes.
%%--------------------------------------------------------------------
-spec create_cluster(Nodes :: [node()]) -> ok.
create_cluster([]) ->
    ok;

create_cluster([Node | _] = Nodes) ->
    Hosts = onepanel_cluster:nodes_to_hosts(Nodes),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [onepanel, deploy, #{
            hosts => Hosts, auth => ?DEFAULT_AUTH, api_version => ?API_VERSION
        }]
    )).