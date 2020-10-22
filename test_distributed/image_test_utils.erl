%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Utils for test suites using actual Oneprovider/Onezone images.
%%% @end
%%%--------------------------------------------------------------------
-module(image_test_utils).
-author("Wojciech Geisler").

-include("names.hrl").
-include("modules/models.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% API
-export([deploy_onezone/5, deploy_oneprovider/3]).
-export([get_registration_token/1]).
-export([proxy_rpc/4, proxy_rpc/5]).

-define(AWAIT_OZ_CONNECTIVITY_ATTEMPTS, 30).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec deploy_onezone(Passphrase :: binary(), Username :: binary(),
    Password :: binary(), MaxWorkersNum :: pos_integer(),
    Config :: onepanel_test_utils:config()) ->
    onepanel_test_utils:config().
deploy_onezone(Passphrase, Username, Password, MaxWorkersNum, Config) ->
    [OzNode | _] = OzNodes = ?config(onezone_nodes, Config),
    OzHosts = hosts:from_nodes(OzNodes),
    OzwHosts = lists:sublist(OzHosts, min(length(OzHosts), MaxWorkersNum)),
    OzDomain = onepanel_test_utils:get_domain(hd(OzHosts)),
    onepanel_test_utils:set_test_envs(OzNodes, [{test_web_cert_domain, OzDomain}]),

    % generate certificate with correct onezone domain
    regenerate_web_certificate(OzNodes, OzDomain),

    rpc:call(OzNode, emergency_passphrase, set, [Passphrase]),
    onepanel_test_utils:service_action(OzNode, ?SERVICE_OZ, deploy, #{
        cluster => #{
            ?SERVICE_PANEL => #{
                hosts => OzHosts
            },
            ?SERVICE_CB => #{
                hosts => OzHosts
            },
            ?SERVICE_CM => #{
                hosts => OzHosts, main_cm_host => hd(OzHosts), worker_num => length(OzwHosts)
            },
            ?SERVICE_OZW => #{
                hosts => OzwHosts, main_cm_host => hd(OzHosts),
                cm_hosts => OzHosts, db_hosts => OzHosts,
                onezone_name => <<"someOnezone">>,
                onezone_domain => string:uppercase(OzDomain),
                onezone_users => [#{
                    username => Username,
                    password => Password,
                    groups => [<<"admins">>]
                }]
            },
            ?SERVICE_LE => #{
                hosts => OzHosts,
                letsencrypt_enabled => false
            }
        },
        ?SERVICE_OZ => #{
            name => <<"someOnezone">>,
            domain => string:uppercase(OzDomain)
        }
    }),
    [{oz_worker_hosts, OzwHosts}, {onezone_domain, OzDomain} | Config].


-spec deploy_oneprovider(Passphrase :: binary(), Storages :: map(),
    Config :: onepanel_test_utils:config()) -> onepanel_test_utils:config().
deploy_oneprovider(Passphrase, Storages, Config) ->
    [OzNode | _] = ?config(onezone_nodes, Config),
    OzIp = test_utils:get_docker_ip(OzNode),
    OzDomain = ?config(onezone_domain, Config),
    [OpNode | _] = OpNodes = ?config(oneprovider_nodes, Config),
    OpHosts = hosts:from_nodes(OpNodes),
    OpwHosts = lists:sublist(OpHosts, length(OpHosts) - 1),
    OpDomain = onepanel_test_utils:get_domain(hd(OpHosts)),

    % We do not have a DNS server that would resolve OZ domain for provider,
    % so we need to simulate it using /etc/hosts.
    lists:foreach(fun(Node) ->
        rpc:call(Node, file, write_file, [
            "/etc/hosts",
            <<"\n", OzIp/binary, "\t", OzDomain/binary>>,
            [append]
        ])
    end, OpNodes),

    RegistrationToken = get_registration_token(OzNode),
    ?assertEqual(ok, rpc:call(OpNode, emergency_passphrase, set, [Passphrase])),
    onepanel_test_utils:service_action(OpNode, ?SERVICE_OP, deploy, #{
        cluster => #{
            ?SERVICE_PANEL => #{
                hosts => OpHosts
            },
            ?SERVICE_CB => #{
                hosts => OpHosts
            },
            ?SERVICE_CM => #{
                hosts => OpHosts, main_cm_host => hd(OpHosts), worker_num => length(OpwHosts)
            },
            ?SERVICE_OPW => #{
                hosts => OpwHosts, main_cm_host => hd(OpHosts),
                cm_hosts => OpHosts, db_hosts => OpHosts
            },
            storages => #{
                hosts => OpwHosts,
                storages => Storages
            },
            ?SERVICE_LE => #{
                hosts => OpHosts,
                letsencrypt_enabled => false
            }
        },
        ?SERVICE_OP => #{
            hosts => OpwHosts,
            oneprovider_geo_latitude => 10.0,
            oneprovider_geo_longitude => 10.0,
            oneprovider_name => <<"provider1">>,
            oneprovider_domain => string:uppercase(OpDomain),
            oneprovider_register => true,
            oneprovider_admin_email => <<"admin@onedata.org">>,
            oneprovider_token => RegistrationToken,
            onezone_domain => str_utils:to_binary(OzDomain)
        }
    }),
    [{op_worker_hosts, OpwHosts}, {oneprovider_domain, OpDomain} | Config].


%%--------------------------------------------------------------------
%% @doc
%% For unknown reasons sometimes direct rpc from testmaster to worker
%% node doesn't work, but proxying through onepanel node does.
%% proxy_rpc/4 chooses the proxy node automatically, proxy_rpc/5 accepts
%% explicit proxy node.
%% @end
%%--------------------------------------------------------------------
-spec proxy_rpc(TargetNode :: node(), Module :: module(),
    Function :: atom(), Args :: [term()]) -> term().
proxy_rpc(TargetNode, Module, Function, Args) ->
    ProxyNode = nodes:service_to_node(?SERVICE_PANEL, TargetNode),
    proxy_rpc(ProxyNode, TargetNode, Module, Function, Args).

-spec proxy_rpc(ProxyNode :: node(), TargetNode :: node(), Module :: module(),
    Function :: atom(), Args :: [term()]) -> term().
proxy_rpc(ProxyNode, TargetNode, Module, Function, Args) ->
    rpc:call(ProxyNode, rpc, call, [
        TargetNode, Module, Function, Args
    ]).


-spec get_registration_token(OzNode :: node()) -> Token :: binary().
get_registration_token(OzNode) ->
    OzwNode = nodes:service_to_node(?SERVICE_OZW, OzNode),

    {ok, [OnezoneUserId | _]} = proxy_rpc(OzNode, OzwNode, user_logic, list, [?ROOT]),
    {ok, RegistrationToken} = ?assertMatch({ok, _},
        proxy_rpc(OzNode, OzwNode,
            user_logic, create_provider_registration_token,
            [?USER(OnezoneUserId), OnezoneUserId]
        ),
        ?AWAIT_OZ_CONNECTIVITY_ATTEMPTS),

    {ok, SerializedToken} = tokens:serialize(RegistrationToken),
    SerializedToken.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates certificate using test CA for given domain.
%% @end
%%--------------------------------------------------------------------
-spec regenerate_web_certificate([node()], Domain :: string()) -> ok | no_return().
regenerate_web_certificate(Nodes, Domain) ->
    [Node | _] = Nodes,
    WebKeyPath = rpc_get_env(Node, web_key_file),
    WebCertPath = rpc_get_env(Node, web_cert_file),
    WebChainPath = rpc_get_env(Node, web_cert_chain_file),

    % Both key and cert are expected in the same file
    CAPath = rpc_get_env(Node, test_web_cert_ca_path),

    {_, []} = rpc:multicall(Nodes, cert_utils, create_signed_webcert, [
        WebKeyPath, WebCertPath, Domain, CAPath, CAPath]),
    {_, []} = rpc:multicall(Nodes, file, copy, [CAPath, WebChainPath]),

    {_, []} = rpc:multicall(Nodes, service_op_worker, reload_webcert, [#{}]),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reads app config from given Node
%% @end
%%--------------------------------------------------------------------
-spec rpc_get_env(node(), atom()) -> term().
rpc_get_env(Node, Key) ->
    rpc:call(Node, onepanel_env, get, [Key]).
