%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Utility functions used throughout middleware plugins.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_utils).
-author("Wojciech Geisler").

-include("deployment_progress.hrl").
-include("middleware/middleware.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").

-export([has_privilege/2]).
-export([format_onepanel_configuration/0, format_service_configuration/1]).
-export([get_hosts/2, get_cluster_ips/1]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Checks if a client is of 'member' type and has given privilege.
%% @end
%%--------------------------------------------------------------------
-spec has_privilege(Client :: middleware:client(),
    RequiredPrivilege :: privileges:cluster_privilege()) -> boolean().
has_privilege(#client{role = member, privileges = UserPrivileges}, RequiredPrivilege) ->
    lists:member(RequiredPrivilege, UserPrivileges);

has_privilege(#client{}, _RequiredPrivileges) -> false.


%%--------------------------------------------------------------------
%% @doc Formats public configuration details.
%% @end
%%--------------------------------------------------------------------
-spec format_onepanel_configuration() -> map().
format_onepanel_configuration() ->
    try
        format_onepanel_configuration(onepanel_env:get_cluster_type())
    catch _:_ ->
        % it is preferable for this endpoint to return something
        format_onepanel_configuration(common)
    end.


%%--------------------------------------------------------------------
%% @doc Returns formatted cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec format_service_configuration(SModule :: service_onezone | service_oneprovider) ->
    Response :: json_utils:json_map().
format_service_configuration(SModule) ->
    DbHosts = hosts:all(?SERVICE_CB),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(?SERVICE_CM),
    WrkHosts = case SModule of
        service_onezone -> hosts:all(?SERVICE_OZW);
        service_oneprovider -> hosts:all(?SERVICE_OPW)
    end,
    {SName, Ctx, Details} = case SModule of
        service_onezone ->
            {ok, #service{ctx = ServiceCtx}} = service:get(?SERVICE_OZ),
            OzDetails = #{domainName => service_oz_worker:get_domain()},
            {maps:get(name, ServiceCtx, null), ServiceCtx, OzDetails};
        service_oneprovider ->
            {ok, #service{ctx = ServiceCtx}} = service:get(?SERVICE_OP),
            Name = case service_oneprovider:is_registered(ServiceCtx) of
                true ->
                    case oz_providers:get_details(provider) of
                        {ok, #provider_details{name = N}} -> N;
                        {error, _Reason} -> null
                    end;
                false -> null
            end,
            {Name, ServiceCtx, #{}}
    end,
    MasterHostBin = case maps:get(master_host, Ctx, null) of
        null -> null;
        MasterHost -> onepanel_utils:convert(MasterHost, binary)
    end,

    CommonClusterConfiguration = #{
        <<"master">> => MasterHostBin,
        <<"hosts">> => onepanel_utils:convert(service_onepanel:get_hosts(), {seq, binary}),
        <<"databases">> => #{
            <<"hosts">> => onepanel_utils:convert(DbHosts, {seq, binary})
        },
        <<"managers">> => #{
            <<"mainHost">> => onepanel_utils:convert(MainCmHost, binary),
            <<"hosts">> => onepanel_utils:convert(CmHosts, {seq, binary})
        },
        <<"workers">> => #{
            <<"hosts">> => onepanel_utils:convert(WrkHosts, {seq, binary})
        }
    },
    ClusterConfiguration = case SModule of
        service_onezone ->
            CommonClusterConfiguration;
        service_oneprovider ->
            CommonClusterConfiguration#{<<"oneS3">> => #{
                <<"hosts">> => onepanel_utils:convert(hosts:all(?SERVICE_ONES3), {seq, binary}),
                <<"port">> => service_ones3:get_port()
            }}
    end,

    #{
        <<"cluster">> => ClusterConfiguration,
        SModule:name() => Details#{
            <<"name">> => SName,
            <<"configured">> => is_service_configured()
        }
    }.


%%--------------------------------------------------------------------
%% @doc Returns lists of hosts for given service based on cluster description
%% format.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts(Keys :: [atom()], middleware_handler:handler_input()) ->
    Hosts :: [service:host()] | no_return().
get_hosts(Keys, Data) ->
    CommonSuffix = common_hostname_suffix(Data),
    Nodes = kv_utils:get([cluster, nodes], Data),
    HostsMap = maps:fold(fun(Alias, Props, Acc) ->
        Host = <<(maps:get(hostname, Props))/binary, CommonSuffix/binary>>,
        Acc#{Alias => Host}
    end, #{}, Nodes),
    Aliases = kv_utils:get(Keys, Data),
    AliasesList = case erlang:is_list(Aliases) of
        true -> Aliases;
        false -> [Aliases]
    end,
    lists:map(fun(Alias) ->
        case maps:find(Alias, HostsMap) of
            {ok, Host} -> onepanel_utils:convert(Host, list);
            error ->
                throw(?ERR_BAD_VALUE_LIST_NOT_ALLOWED(
                    ?err_ctx(),
                    str_utils:join_as_binaries(Keys, <<".">>),
                    maps:keys(HostsMap)
                ))
        end
    end, AliasesList).


%%--------------------------------------------------------------------
%% @doc Returns map from cluster hosts to their IPs given
%% in cluster description.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_ips(middleware_handler:handler_input()) ->
    #{service:host() => binary()} | no_return().
get_cluster_ips(Data) ->
    CommonSuffix = common_hostname_suffix(Data),
    Nodes = kv_utils:get([cluster, nodes], Data),
    NodesWithIPs = maps:filter(fun(_Node, Props) ->
        maps:is_key(externalIp, Props)
    end, Nodes),

    maps:fold(fun(_Alias, Props, Acc) ->
        IP = maps:get(externalIp, Props),
        Host = onepanel_utils:convert(
            <<(maps:get(hostname, Props))/binary, CommonSuffix/binary>>,
            list),
        maps:put(Host, IP, Acc)
    end, #{}, NodesWithIPs).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec format_onepanel_configuration(ClusterType :: onedata:cluster_type() | common) ->
    #{atom() := term()}.
format_onepanel_configuration(onezone) ->
    Defaults = #{serviceType => onezone, zoneDomain => null, zoneName => null},
    try
        Details = service_oz_worker:get_details(#{}),
        ZoneDomain = maps:get(domain, Details, undefined),
        ZoneName = maps:get(name, Details, undefined),
        Configuration = Defaults#{
            zoneDomain => ZoneDomain,
            zoneName => ZoneName
        },

        maps_utils:undefined_to_null(
            maps:merge(Configuration, format_onepanel_configuration(common))
        )
    catch _:_ ->
        % probably no oz_worker nodes
        maps_utils:undefined_to_null(
            maps:merge(Defaults, format_onepanel_configuration(common))
        )
    end;

format_onepanel_configuration(oneprovider) ->
    Common = #{serviceType => oneprovider},
    OpConfiguration = case service_oneprovider:is_registered() of
        false ->
            Common#{
                zoneDomain => null,
                providerId => null,
                isRegistered => false,
                providerName => null,
                providerDomain => null
            };
        true ->
            try
                Details = service_oneprovider:get_details(),
                ProviderId = maps:get(id, Details, undefined),
                ProviderName = maps:get(name, Details, undefined),
                ProviderDomain = maps:get(domain, Details, undefined),
                ZoneDomain = maps:get(onezoneDomainName, Details, undefined),
                maps_utils:undefined_to_null(Common#{
                    providerId => ProviderId,
                    providerName => ProviderName,
                    providerDomain => ProviderDomain,
                    zoneDomain => ZoneDomain,
                    isRegistered => true
                })
            catch
                _:_ ->
                    % If op_worker was configured, the Onezone domain can be
                    % read even when the worker is down.
                    Common#{
                        providerId => null,
                        providerName => null,
                        providerDomain => null,
                        zoneDomain => list_to_binary(service_oneprovider:get_oz_domain()),
                        isRegistered => true
                    }
            end
    end,
    maps:merge(format_onepanel_configuration(common), OpConfiguration);

format_onepanel_configuration(_ClusterType) ->
    ClusterId = try clusters:get_id() catch _:_ -> null end,
    {BuildVersion, AppVersion} = onepanel:get_build_and_version(),
    #{
        version => AppVersion,
        build => BuildVersion,
        deployed => is_service_configured(),
        clusterId => ClusterId
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc Checks if all configuration steps have been performed.
%% @end
%%--------------------------------------------------------------------
-spec is_service_configured() -> boolean().
is_service_configured() ->
    onepanel_deployment:is_set(?PROGRESS_READY).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns suffix to be appended to each node's "hostname" field
%% to create full node hostname.
%% @end
%%--------------------------------------------------------------------
-spec common_hostname_suffix(middleware_handler:handler_input()) -> binary().
common_hostname_suffix(Data) ->
    case kv_utils:get([cluster, domainName], Data, <<>>) of
        <<>> -> <<>>;
        DomainName -> <<".", DomainName/binary>>
    end.
