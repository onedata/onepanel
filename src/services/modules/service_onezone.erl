%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains onezone services management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onezone).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("modules/onepanel_dns.hrl").
-include("service.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_definitions.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

-export([set_up_service_in_onezone/0]).
-export([mark_configured/1, format_cluster_ips/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    onezone.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    lists:usort(lists:append([
        service:get_hosts(?SERVICE_CB),
        service:get_hosts(?SERVICE_CM),
        service:get_hosts(?SERVICE_OZW)
    ])).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    nodes:all(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    service:create(#service{name = name()}),
    SelfHost = hosts:self(),

    {ok, OpaCtx} = onepanel_maps:get([cluster, ?SERVICE_PANEL], Ctx),
    {ok, LeCtx} = onepanel_maps:get([cluster, ?SERVICE_LE], Ctx),
    {ok, CbCtx} = onepanel_maps:get([cluster, ?SERVICE_CB], Ctx),
    {ok, CmCtx} = onepanel_maps:get([cluster, ?SERVICE_CM], Ctx),
    {ok, OzwCtx} = onepanel_maps:get([cluster, ?SERVICE_OZW], Ctx),

    DnsConfig = onepanel_maps:get([name(), dns_check_config], Ctx, #{}),

    OzCtx1 = onepanel_maps:get(name(), Ctx, #{}),
    OzCtx2 = OzCtx1#{
        master_host => SelfHost
    },
    S = #step{verify_hosts = false},
    Ss = #steps{verify_hosts = false},
    [
        Ss#steps{service = ?SERVICE_PANEL, action = deploy, ctx = OpaCtx},
        Ss#steps{service = ?SERVICE_PANEL, action = add_users, ctx = OpaCtx},
        Ss#steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        S#step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        Ss#steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        S#step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        Ss#steps{service = ?SERVICE_OZW, action = deploy, ctx = OzwCtx},
        S#step{service = ?SERVICE_OZW, function = status, ctx = OzwCtx},
        Ss#steps{action = set_up_service_in_onezone, ctx = OzwCtx},
        Ss#steps{service = ?SERVICE_OZW, action = configure_dns_check,
            ctx = maps:merge(OzwCtx, DnsConfig), condition = fun(_) -> DnsConfig /= #{} end},

        Ss#steps{service = ?SERVICE_LE, action = deploy, ctx = LeCtx#{
            letsencrypt_plugin => ?SERVICE_OZW
        }},
        S#step{module = onepanel_deployment, function = mark_completed, ctx = OpaCtx,
            args = [?PROGRESS_CLUSTER], selection = first},
        S#step{module = service, function = save, ctx = OpaCtx,
            args = [#service{name = name(), ctx = OzCtx2}],
            selection = first
        },
        Ss#steps{service = ?SERVICE_LE, action = update, ctx = LeCtx},
        S#step{module = onepanel_deployment, function = mark_completed, ctx = OpaCtx,
            args = [?PROGRESS_READY], selection = first},
        S#step{function = mark_configured, ctx = OpaCtx, selection = any,
            condition = fun(FunCtx) -> not maps:get(interactive_deployment, FunCtx, true) end}
    ];

get_steps(start, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = start},
        #steps{service = ?SERVICE_CM, action = start},
        #steps{service = ?SERVICE_OZW, action = start}
    ];

get_steps(stop, _Ctx) ->
    [
        #steps{service = ?SERVICE_OZW, action = stop},
        #steps{service = ?SERVICE_CM, action = stop},
        #steps{service = ?SERVICE_CB, action = stop}
    ];

get_steps(restart, _Ctx) ->
    [
        #steps{action = stop},
        #steps{action = start}
    ];

% returns any steps only on the master node
get_steps(manage_restart, Ctx) ->
    MasterHost = case service:get(name()) of
        {ok, #service{ctx = #{master_host := Master}}} -> Master;
        _ ->
            [FirstHost | _] = get_hosts(),
            ?info("No master host configured, defaulting to ~p", [FirstHost]),
            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = C#{master_host => FirstHost}}
            end),
            FirstHost
    end,

    case hosts:self() == MasterHost of
        true -> [
            #steps{service = ?SERVICE_PANEL, action = wait_for_cluster},
            #steps{action = stop},
            #steps{service = ?SERVICE_CB, action = resume},
            #steps{service = ?SERVICE_CM, action = resume},
            #steps{service = ?SERVICE_OZW, action = resume},
            #steps{service = ?SERVICE_LE, action = resume,
                ctx = Ctx#{letsencrypt_plugin => ?SERVICE_OZW}},
            #steps{action = set_up_service_in_onezone}
        ];
        false ->
            ?info("Waiting for master node \"~s\" to start", [MasterHost]),
            []
    end;

get_steps(status, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = status},
        #steps{service = ?SERVICE_CM, action = status},
        #steps{service = ?SERVICE_OZW, action = status}
    ];

get_steps(set_cluster_ips, Ctx) ->
    GeneratedConfigFile = service_ctx:get(oz_worker_generated_config_file, Ctx),
    Ctx2 = Ctx#{
        generated_config_file => GeneratedConfigFile,
        name => ?SERVICE_OZW
    }, [
        #steps{action = set_cluster_ips, ctx = Ctx2, service = ?SERVICE_CW},
        #step{function = reconcile_dns, selection = any, service = ?SERVICE_OZW,
            hosts = get_hosts()}
    ];

get_steps(format_cluster_ips, _Ctx) ->
    [#step{hosts = get_hosts(), function = format_cluster_ips, selection = any}];

get_steps(set_up_service_in_onezone, _Ctx) ->
    [#step{function = set_up_service_in_onezone, args = [], selection = any}].


%%--------------------------------------------------------------------
%% @doc Returns IPs of hosts with oz_worker instances.
%% @end
%%--------------------------------------------------------------------
-spec format_cluster_ips(service:ctx()) ->
    #{isConfigured := boolean(), hosts := #{binary() => binary()}}.
format_cluster_ips(Ctx) ->
    service_cluster_worker:get_cluster_ips(Ctx#{name => ?SERVICE_OZW}).


%%-------------------------------------------------------------------
%% @doc
%% Marks all configuration steps as already performed.
%% @end
%%-------------------------------------------------------------------
-spec mark_configured(service:ctx()) -> ok.
mark_configured(_Ctx) ->
    onepanel_deployment:mark_completed([
        ?PROGRESS_LETSENCRYPT_CONFIG,
        ?PROGRESS_CLUSTER_IPS,
        ?DNS_CHECK_ACKNOWLEDGED
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Sets up Onezone panel service in Onezone - deploys static GUI files and
%% updates version info (release, build and GUI versions).
%% @end
%%--------------------------------------------------------------------
-spec set_up_service_in_onezone() -> ok.
set_up_service_in_onezone() ->
    ?info("Setting up Onezone panel service in Onezone"),

    GuiPackagePath = https_listener:gui_package_path(),
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {ok, GuiHash} = rpc:call(OzNode, static_gui_router, deploy_static_gui_files, [
        ?OZ_PANEL, filename:absname(GuiPackagePath)
    ]),
    ?info("Deployed static GUI files (~s)", [GuiHash]),

    {BuildVersion, AppVersion} = onepanel_app:get_build_and_version(),

    {rpc, Client} = zone_client:root_auth(),
    VersionInfo = {AppVersion, BuildVersion, GuiHash},
    ok = rpc:call(OzNode, cluster_logic, update_version_info,
        [Client, clusters:get_id(), ?ONEPANEL_SERVICE, VersionInfo]),

    % pre-warm cache
    (catch clusters:get_current_cluster()),

    ?info("Onezone panel service successfully set up in Onezone").
