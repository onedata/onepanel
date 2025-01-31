%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for cluster deployment tests.
%%% @end
%%%--------------------------------------------------------------------
-module(cluster_deployment_test_utils).
-author("Bartosz Walkowicz").

-include("cluster_deployment_test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/assertions.hrl").

-type node_details() :: #node_details{}.
-type op_cluster_config() :: #op_cluster_config{}.

-export_type([
    node_details/0,
    op_cluster_config/0
]).

%% API
-export([
    infer_node_details/1,

    deploy_batch/1,

    deploy_cluster/1,
    deploy_ones3/1,

    register_provider/1,
    configure_dns/1,
    configure_web_cert/1
]).

-define(AWAIT_DEPLOYMENT_READY_ATTEMPTS, 180).


%%%===================================================================
%%% API
%%%===================================================================


-spec infer_node_details(node()) -> node_details().
infer_node_details(Node) ->
    NodeIp = ip_test_utils:get_node_ip(Node),
    NodeHost = str_utils:to_binary(hosts:from_node(Node)),

    #node_details{
        node = Node,
        hostname = NodeHost,
        ip = NodeIp
    }.


-spec deploy_batch(op_cluster_config()) -> ok.
deploy_batch(ClusterConfig) ->
    Node = get_main_node(ClusterConfig),
    BatchConfig = build_batch_config(ClusterConfig),

    {ok, ?HTTP_202_ACCEPTED, _, #{<<"taskId">> := TaskId}} = panel_test_rest:post(
        Node, <<"/provider/configuration">>, #{
            auth => root,
            json => BatchConfig
        }
    ),
    cluster_test_utils:await_task_status(Node, TaskId, <<"ok">>, ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


-spec deploy_cluster(op_cluster_config()) -> ok.
deploy_cluster(ClusterConfig) ->
    Node = get_main_node(ClusterConfig),
    ClusterConfig2 = build_cluster_config(ClusterConfig),

    {ok, ?HTTP_202_ACCEPTED, _, #{<<"taskId">> := TaskId}} = panel_test_rest:post(
        Node, <<"/provider/configuration">>, #{
            auth => root,
            json => #{<<"cluster">> => ClusterConfig2}
        }
    ),
    cluster_test_utils:await_task_status(Node, TaskId, <<"ok">>, ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


-spec deploy_ones3(op_cluster_config()) -> ok.
deploy_ones3(ClusterConfig = #op_cluster_config{
    nodes = Nodes,
    ones3_nodes = OneS3Nodes
}) ->
    Node = get_main_node(ClusterConfig),

    OneS3Hosts = lists:map(fun(NodeId) ->
        NodeDetails = maps:get(NodeId, Nodes),
        NodeDetails#node_details.hostname
    end, OneS3Nodes),

    {ok, ?HTTP_202_ACCEPTED, _, #{<<"taskId">> := TaskId}} = panel_test_rest:post(
        Node, <<"/provider/ones3">>, #{
            auth => root,
            json => #{<<"hosts">> => OneS3Hosts}
        }
    ),
    cluster_test_utils:await_task_status(Node, TaskId, <<"ok">>, ?AWAIT_DEPLOYMENT_READY_ATTEMPTS).


-spec register_provider(op_cluster_config()) -> ok.
register_provider(#op_cluster_config{
    name = Name,
    admin_email = AdminEmail,
    registration_token = Token,
    subdomain_delegation = SubdomainDelegation,
    subdomain = Subdomain,
    domain = Domain
} = Config) ->
    Node = get_main_node(Config),
    
    RegistrationConfig = #{
        <<"register">> => true,
        <<"token">> => Token,
        <<"name">> => Name,
        <<"adminEmail">> => AdminEmail,
        <<"subdomainDelegation">> => SubdomainDelegation
    },

    RegistrationConfig2 = case SubdomainDelegation of
        true -> RegistrationConfig#{<<"subdomain">> => Subdomain};
        false -> RegistrationConfig#{<<"domain">> => Domain}
    end,

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:post(Node, <<"/provider">>, #{
            auth => root,
            json => RegistrationConfig2
        })
    ),
    ok.


-spec configure_dns(op_cluster_config()) -> ok.
configure_dns(#op_cluster_config{nodes = Nodes} = Config) ->
    Node = get_main_node(Config),
    
    ExternalHostIps = #{<<"hosts">> => maps:fold(fun(_, #node_details{
        hostname = Host,
        ip = Ip
    }, Acc) ->
        Acc#{Host => ip_test_utils:encode_ip(Ip)}
    end, #{}, Nodes)},

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(Node, <<"/provider/cluster_ips">>, #{
            auth => root,
            json => ExternalHostIps
        })
    ),

    ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(Node, <<"/dns_check">>, #{auth => root})
    ),

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(Node, <<"/dns_check/configuration">>, #{
            auth => root,
            json => #{<<"dnsCheckAcknowledged">> => true}
        })
    ),
    ok.


-spec configure_web_cert(op_cluster_config()) -> ok.
configure_web_cert(#op_cluster_config{lets_encrypt = LetsEncrypt} = Config) ->
    Node = get_main_node(Config),

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(Node, <<"/web_cert">>, #{
            auth => root,
            json => #{<<"letsEncrypt">> => LetsEncrypt}
        })
    ),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_main_node(#op_cluster_config{nodes = Nodes, main_manager = MainManager}) ->
    #node_details{node = Node} = maps:get(MainManager, Nodes),
    Node.


%% @private
build_batch_config(#op_cluster_config{} = Config) ->
    #{
        <<"cluster">> => build_cluster_config(Config),
        <<"oneprovider">> => build_provider_config(Config)
    }.


%% @private
build_cluster_config(#op_cluster_config{
    nodes = Nodes,
    managers = Managers,
    main_manager = MainManager,
    workers = Workers,
    databases = Databases,
    ones3_nodes = OneS3Nodes
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
        },
        <<"oneS3">> => #{
            <<"nodes">> => lists:map(fun node_id_to_name/1, OneS3Nodes)
        }
    }.


%% @private
build_provider_config(#op_cluster_config{
    name = Name,
    admin_email = AdminEmail,
    register = Register,
    registration_token = Token,
    subdomain_delegation = SubdomainDelegation,
    subdomain = Subdomain,
    domain = Domain,
    lets_encrypt = LetsEncrypt
}) ->
    Config = #{
        <<"register">> => Register,
        <<"name">> => Name,
        <<"adminEmail">> => AdminEmail
    },
    
    Config1 = case Register of
        true -> Config#{<<"token">> => Token};
        false -> Config
    end,

    Config2 = Config1#{
        <<"subdomainDelegation">> => SubdomainDelegation,
        <<"letsEncryptEnabled">> => LetsEncrypt
    },

    case SubdomainDelegation of
        true -> Config2#{<<"subdomain">> => Subdomain};
        false -> Config2#{<<"domain">> => Domain}
    end.


%% @private
node_id_to_name(Id) ->
    <<"node-", (integer_to_binary(Id))/binary>>.
