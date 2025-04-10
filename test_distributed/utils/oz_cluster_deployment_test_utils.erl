%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for oz cluster deployment tests.
%%% @end
%%%--------------------------------------------------------------------
-module(oz_cluster_deployment_test_utils).
-author("Bartosz Walkowicz").

-include("cluster_deployment_test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/assertions.hrl").

-type oz_cluster_config() :: #oz_cluster_config{}.

-export_type([oz_cluster_config/0]).

%% API
-export([
    deploy_batch/1,

    deploy_all_services/1
]).

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).


%%%===================================================================
%%% API
%%%===================================================================


-spec deploy_batch(oz_cluster_config()) -> ok.
deploy_batch(ClusterConfig) ->
    Node = get_main_node(ClusterConfig),
    BatchConfig = build_batch_config(ClusterConfig),

    {ok, ?HTTP_202_ACCEPTED, _, #{<<"taskId">> := TaskId}} = panel_test_rest:post(
        Node, <<"/zone/configuration">>, #{
            auth => root,
            json => BatchConfig
        }
    ),
    cluster_management_test_utils:await_task_status(Node, TaskId, <<"ok">>, ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


-spec deploy_all_services(oz_cluster_config()) -> ok.
deploy_all_services(ClusterConfig) ->
    Node = get_main_node(ClusterConfig),
    ClusterConfig2 = build_cluster_config(ClusterConfig),

    {ok, ?HTTP_202_ACCEPTED, _, #{<<"taskId">> := TaskId}} = panel_test_rest:post(
        Node, <<"/zone/configuration">>, #{
            auth => root,
            json => #{<<"cluster">> => ClusterConfig2}
        }
    ),
    cluster_management_test_utils:await_task_status(Node, TaskId, <<"ok">>, ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_main_node(#oz_cluster_config{nodes = Nodes, main_manager = MainManager}) ->
    #node_details{node = Node} = maps:get(MainManager, Nodes),
    Node.


%% @private
build_batch_config(#oz_cluster_config{} = Config) ->
    #{
        <<"cluster">> => build_cluster_config(Config),
        <<"onezone">> => build_zone_config(Config)
    }.


%% @private
build_cluster_config(#oz_cluster_config{
    nodes = Nodes,
    managers = Managers,
    main_manager = MainManager,
    workers = Workers,
    databases = Databases
}) ->
    #{
        <<"nodes">> => maps:fold(fun(Id, #node_details{hostname = Host, ip = Ip}, Acc) ->
            Acc#{node_id_to_name(Id) => #{
                <<"hostname">> => Host,
                <<"externalIp">> => ip_test_utils:encode_ip(Ip)
            }}
        end, #{}, Nodes),
        <<"managers">> => #{
            <<"mainNode">> => node_id_to_name(MainManager),
            <<"nodes">> => lists:map(fun node_id_to_name/1, Managers)
        },
        <<"workers">> => #{
            <<"nodes">> => lists:map(fun node_id_to_name/1, Workers)
        },
        <<"databases">> => #{
            <<"nodes">> => lists:map(fun node_id_to_name/1, Databases)
        }
    }.


%% @private
build_zone_config(#oz_cluster_config{
    name = Name,
    domain = Domain,
    lets_encrypt = LetsEncrypt,
    built_in_dns = BuiltInDns
}) ->
    #{
        <<"name">> => Name,
        <<"domainName">> => Domain,
        <<"letsEncryptEnabled">> => LetsEncrypt,
        <<"builtInDnsServer">> => BuiltInDns
    }.


%% @private
node_id_to_name(Id) ->
    <<"node-", (integer_to_binary(Id))/binary>>.
