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

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").

-export([execute_service_action/3]).
-export([result_from_service_action/2, result_from_service_action/3,
    result_from_service_action/5]).
-export([has_privilege/2]).
-export([format_onepanel_configuration/0, format_service_configuration/1]).
-export([get_hosts/2, get_cluster_ips/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Executes service action and checks for errors.
%% @end
%%--------------------------------------------------------------------
-spec execute_service_action(service:name(), service:action(), service:step_ctx()) -> ok.
execute_service_action(Service, Action, Ctx) ->
    ActionResults = service:apply_sync(Service, Action, Ctx),
    service_utils:throw_on_error(ActionResults),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @equiv result_from_service_action(Service, Action, #{}).
%% @end
%%--------------------------------------------------------------------
-spec result_from_service_action(service:name(), service:action()) ->
    term() | errors:error().
result_from_service_action(Service, Action) ->
    result_from_service_action(Service, Action, #{}).


%%--------------------------------------------------------------------
%% @doc Executes service action. Returns result of a function named
%% like the action, defined in service's main module.
%% @equiv result_from_service_action(Service, Action, Ctx, get_module(Service), Action).
%% @end
%%--------------------------------------------------------------------
-spec result_from_service_action(service:name(), service:action(), service:step_ctx()) ->
    term() | errors:error().
result_from_service_action(Service, Action, Ctx) ->
    result_from_service_action(Service, Action, Ctx,
        service:get_module(Service), Action).


%%--------------------------------------------------------------------
%% @doc Executes service action. Returns result value
%% of given Module:Function executed as one of the action steps.
%% If the function was executed on multiple hosts, result from
%% one host only is returned.
%% If any step returned an error, the error is thrown.
%% @end
%%--------------------------------------------------------------------
-spec result_from_service_action(service:name(), service:action(), service:step_ctx(),
    module(), Function :: atom()) -> term().
result_from_service_action(Service, Action, Ctx, Module, Function) ->
    ActionResults = service:apply_sync(Service, Action, Ctx),
    case service_utils:results_contain_error(ActionResults) of
        {true, Error} ->
            throw(Error);
        false ->
            {[{_Host, Result} | _], []} = service_utils:select_service_step(
                Module, Function, ActionResults),
            Result
    end.


%%--------------------------------------------------------------------
%% @doc Checks if a client is of 'member' type and has given privilege.
%% @end
%%--------------------------------------------------------------------
-spec has_privilege(Client :: middleware:client(),
    RequiredPrivilege :: privileges:cluster_privilege()) -> boolean().
has_privilege(#client{role = member, privileges = UserPrivileges}, RequiredPrivilege)  ->
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
    Ceph = case service:get_hosts(?SERVICE_CEPH) /= [] of
        true -> #{
            <<"ceph">> => maps_utils:merge([
                service_ceph_mgr:list(),
                service_ceph_mon:list(),
                service_ceph_osd:list()
            ])
        };
        false ->
            #{}
    end,
    Ceph#{
        <<"cluster">> => #{
            <<"master">> => MasterHostBin,
            <<"hosts">> =>
                onepanel_utils:convert(service_onepanel:get_hosts(), {seq, binary}),
            <<"databases">> => #{
                <<"hosts">> => onepanel_utils:convert(DbHosts, {seq, binary})},
            <<"managers">> => #{
                <<"mainHost">> => onepanel_utils:convert(MainCmHost, binary),
                <<"hosts">> => onepanel_utils:convert(CmHosts, {seq, binary})},
            <<"workers">> => #{
                <<"hosts">> => onepanel_utils:convert(WrkHosts, {seq, binary})}
        },
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
-spec get_hosts(Keys :: [atom()], Args :: middleware:data()) ->
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
                throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(
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
-spec get_cluster_ips(middleware:data()) ->
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
        maps:merge(Defaults, format_onepanel_configuration(common))
    end;

format_onepanel_configuration(oneprovider) ->
    Common = #{serviceType => oneprovider},
    OpConfiguration = case service_oneprovider:is_registered() of
        false ->
            Common#{
                zoneDomain => null,
                providerId => null,
                isRegistered => false
            };
        true ->
            try
                Details = service_oneprovider:get_details(),
                ProviderID = maps:get(id, Details, undefined),
                ProviderName = maps:get(name, Details, undefined),
                ProviderDomain = maps:get(domain, Details,undefined),
                ZoneDomain = maps:get(onezoneDomainName, Details, undefined),
                Common#{
                    providerId => ProviderID,
                    providerName => ProviderName,
                    providerDomain => ProviderDomain,
                    zoneDomain => ZoneDomain,
                    isRegistered => true
                }
            catch
                _:_ ->
                    % If op_worker was configured, the Onezone domain can be
                    % read even when the worker is down.
                    Common#{
                        zoneDomain => list_to_binary(service_oneprovider:get_oz_domain()),
                        providerId => null,
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
-spec common_hostname_suffix(middleware:data()) -> binary().
common_hostname_suffix(Data) ->
    case kv_utils:get([cluster, domainName], Data, <<>>) of
        <<>> -> <<>>;
        DomainName -> <<".", DomainName/binary>>
    end.
