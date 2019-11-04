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
-include("modules/errors.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([init/1, ensure_started/1, set_test_envs/1, set_test_envs/2,
    mock_start/1]).
-export([assert_fields/2, assert_values/2, clear_msg_inbox/0]).
-export([service_host_action/3, service_host_action/4,
    service_action/3, service_action/4, attempt_service_action/4]).
-export([get_domain/1]).

-type config() :: proplists:proplist().

-define(TEST_ENVS, [
    {cluster_manager_vm_args_file, "/etc/cluster_manager/vm.args"},
    {cluster_manager_app_config_file, "/var/lib/cluster_manager/app.config"},
    {cluster_manager_generated_config_file, "/etc/cluster_manager/autogenerated.config"},
    {cluster_manager_overlay_config_file, "/etc/cluster_manager/overlay.config"},
    {cluster_manager_env_file, "/usr/lib/cluster_manager/lib/env.sh"},
    {op_worker_vm_args_file, "/etc/op_worker/vm.args"},
    {op_worker_app_config_file, "/var/lib/op_worker/app.config"},
    {op_worker_generated_config_file, "/etc/op_worker/autogenerated.config"},
    {op_worker_overlay_config_file, "/etc/op_worker/overlay.config"},
    {oz_worker_vm_args_file, "/etc/oz_worker/vm.args"},
    {oz_worker_app_config_file, "/var/lib/oz_worker/app.config"},
    {oz_worker_generated_config_file, "/etc/oz_worker/autogenerated.config"},
    {oz_worker_overlay_config_file, "/etc/oz_worker/overlay.config"},
    {services_check_period, timer:hours(1)}
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
        {all_hosts, hosts:from_nodes(Nodes)},
        {oneprovider_nodes, ProviderNodes},
        {oneprovider_hosts, hosts:from_nodes(ProviderNodes)},
        {onezone_nodes, ZoneNodes},
        {onezone_hosts, hosts:from_nodes(ZoneNodes)},
        {onepanel_nodes, PanelNodes},
        {onepanel_hosts, hosts:from_nodes(PanelNodes)},
        {letsencrypt_nodes, PanelNodes},
        {letsencrypt_hosts, hosts:from_nodes(PanelNodes)} |
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
-spec assert_fields(KeyValue :: [tuple()] | map() , Fields :: [binary()]) -> ok.
assert_fields(Map = #{}, ExpectedFields) ->
    assert_fields(maps:to_list(Map), ExpectedFields);
assert_fields(TupleList, ExpectedFields) ->
    lists:foreach(fun(Field) ->
        case lists:keymember(Field, 1, TupleList) of
            true -> ok;
            false ->
                ct:pal("Key ~p missing in body ~p", [Field, TupleList]),
                ?assert(false)
        end
    end, ExpectedFields).


%%--------------------------------------------------------------------
%% @doc Checks whether the response body contains specified fields and their
%% values match the expected ones.
%% @end
%%--------------------------------------------------------------------
-spec assert_values(KeyValue :: [tuple()] | map(), ExpectedValues :: [{binary(), any()}]) -> ok.
assert_values(ResponseMap = #{}, ExpectedValues) ->
    assert_values(maps:to_list(ResponseMap), ExpectedValues);
assert_values(ResponseProplist, ExpectedValues) ->
    lists:foreach(fun({Field, Value}) ->
        ?assert(lists:keymember(Field, 1, ResponseProplist)),
        ?assertEqual({Field, Value}, lists:keyfind(Field, 1, ResponseProplist))
    end, ExpectedValues).


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


%%--------------------------------------------------------------------
%% @doc @equiv service_host_action(Node, Service, Action, #{}).
%% @end
%%--------------------------------------------------------------------
-spec service_host_action(Node :: node(), Service :: service:name(),
    Action :: atom()) -> ok | no_return().
service_host_action(Node, Service, Action) ->
    service_host_action(Node, Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Performs service action setting given Node as target host.
%% @end
%%--------------------------------------------------------------------
-spec service_host_action(Node :: node(), Service :: service:name(),
    Action :: atom(), Ctx :: service:ctx()) -> ok | no_return().
service_host_action(Node, Service, Action, Ctx) ->
    Host = hosts:from_node(Node),
    service_action(Node, Service, Action, Ctx#{hosts => [Host]}).


%%--------------------------------------------------------------------
%% @doc @equiv service_action(Node, Service, Action, #{}).
%% @end
%%--------------------------------------------------------------------
-spec service_action(Node :: node(), Service :: service:name(),
    Action :: atom()) -> ok | no_return().
service_action(Node, Service, Action) ->
    service_action(Node, Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Performs service action on given node and ensures success.
%% @end
%%--------------------------------------------------------------------
-spec service_action(Node :: node(), Service :: service:name(),
    Action :: atom(), Ctx :: service:ctx()) -> ok | no_return().
service_action(Node, Service, Action, Ctx) ->
    Self = self(),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [Service, Action, Ctx, Self]
    )).


%%--------------------------------------------------------------------
%% @doc Performs service action on given node.
%% @end
%%--------------------------------------------------------------------
-spec attempt_service_action(Node :: node(), Service :: service:name(),
    Action :: atom(), Ctx :: service:ctx()) -> ok | #error{}.
attempt_service_action(Node, Service, Action, Ctx) ->
    Self = self(),
    rpc:call(Node, service, apply, [Service, Action, Ctx, Self]).


%%--------------------------------------------------------------------
%% @doc Returns hostname stripped of the first segment.
%% @end
%%--------------------------------------------------------------------
-spec get_domain(Hostname :: term()) -> binary().
get_domain(Hostname) when not is_binary(Hostname) ->
    get_domain(str_utils:to_binary(Hostname));
get_domain(Hostname) ->
    [_Hostname, Domain] = binary:split(Hostname, <<".">>),
    Domain.

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
        Host = hosts:from_node(Node),
        lists:member(erlang:atom_to_list(Segment), string:tokens(Host, "."))
    end, Nodes).


%%--------------------------------------------------------------------
%% @private @doc Deploys onepanel cluster on the provided nodes.
%%--------------------------------------------------------------------
-spec create_cluster(Nodes :: [node()]) -> ok.
create_cluster([]) ->
    ok;

create_cluster([Node | _] = Nodes) ->
    Hosts = hosts:from_nodes(Nodes),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [onepanel, deploy, #{
            hosts => Hosts, api_version => ?API_VERSION
        }]
    )).