%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider/ceph/* REST resources.
%%%-------------------------------------------------------------------
-module(rest_ceph).
-author("Wojciech Geisler").

-include("names.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% API
-export([read_ceph_args/1]).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, is_conflict/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).


%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Extracts request parameters relevant to Ceph.
%% @end
%%--------------------------------------------------------------------
-spec read_ceph_args(rest_handler:args()) -> {CephCtx :: #{
    cluster_name => binary(), fsid => ceph:uuid(),
    osds | monitors | managers => [service:ctx(), ...]
}, CephHosts :: [service:host()]}.
read_ceph_args(Args) ->
    Osds = lists:map(fun get_osd_params/1, maps:get(osds, Args, [])),
    Mons = lists:map(fun get_mon_params/1, maps:get(monitors, Args, [])),
    Mgrs = lists:map(fun get_mgr_params/1, maps:get(managers, Args, [])),

    Ctx = kv_utils:copy_found([
        {name, cluster_name},
        {fsid, fsid}
    ], Args, #{
        osds => Osds,
        monitors => Mons,
        managers => Mgrs
    }),
    Hosts = ceph:extract_hosts(Osds ++ Mons ++ Mgrs),
    IsNonempty = fun(_Key, []) -> false; (_, _) -> true end,
    {maps:filter(IsNonempty, Ctx), Hosts}.


%%%===================================================================
%%% Rest behaviour callbacks
%%%===================================================================

-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) -> {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = root}}) ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{client = #client{role = member}}) ->
    {true, Req};
is_authorized(Req, _Method, #rstate{client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_UPDATE), Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = Resource}) when
    Resource == block_devices ->
    {true, Req};

exists_resource(Req, #rstate{resource = Resource, bindings = #{name := Name}}) when
    Resource == ceph_pool;
    Resource == ceph_pool_usage ->
    {service:get_hosts(?SERVICE_CEPH) /= [] andalso ceph_pool:exists(Name), Req};

exists_resource(Req, #rstate{resource = Service}) when
    Service == service_ceph_monitors;
    Service == service_ceph_managers;
    Service == service_ceph_osds;
    Service == ceph_status ->
    {service:get_hosts(?SERVICE_CEPH) /= [], Req};

exists_resource(Req, #rstate{resource = service_ceph_monitor, bindings = #{id := Id}}) ->
    {service:exists(?SERVICE_CEPH_MON) andalso service_ceph_mon:exists(Id), Req};

exists_resource(Req, #rstate{resource = service_ceph_manager, bindings = #{id := Id}}) ->
    {service:exists(?SERVICE_CEPH_MGR) andalso service_ceph_mgr:exists(Id), Req};

exists_resource(Req, #rstate{resource = OsdResource, bindings = #{id := Id}}) when
    OsdResource == service_ceph_osd;
    OsdResource == ceph_osd_usage ->
    {service:exists(?SERVICE_CEPH_OSD) andalso service_ceph_osd:exists(Id), Req};

exists_resource(Req, _State) ->
    {service:get_hosts(?SERVICE_CEPH) /= [], Req}.


-spec is_conflict(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {IsConflict :: boolean(), Req :: cowboy_req:req()}
    | {stop, Req :: cowboy_req:req()}.
is_conflict(Req, _Method, _Args, _State) ->
    {false, Req}.


-spec is_available(Req :: cowboy_req:req(),
    Method :: rest_handler:method_type(), State :: rest_handler:state()) ->
    {Available :: boolean(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req()}.
is_available(Req, _Method, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req()}.
accept_resource(Req, 'PATCH', Args, #rstate{resource = ceph_pool, bindings = #{name := Name}}) ->
    Ctx = kv_utils:copy_found([
        % camelCase is not common in locally used params, but matches
        % storage args convention
        {copiesNumber, copiesNumber},
        {minCopiesNumber, minCopiesNumber}
    ], Args, #{name => Name}),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE_CEPH, modify_pool, Ctx
    ))};

accept_resource(Req, 'POST', Args, #rstate{resource = service_ceph_osds} = State) ->
    accept_resource(Req, 'POST', maps:with([osds], Args),
        State#rstate{resource = service_ceph});

accept_resource(Req, 'POST', Args, #rstate{resource = service_ceph_monitors} = State) ->
    accept_resource(Req, 'POST', maps:with([monitors], Args),
        State#rstate{resource = service_ceph});

accept_resource(Req, 'POST', Args, #rstate{resource = service_ceph_managers} = State) ->
    accept_resource(Req, 'POST', maps:with([managers], Args),
        State#rstate{resource = service_ceph});

accept_resource(Req, 'POST', Args, #rstate{resource = service_ceph, version = Version}) ->
    {Ctx, _} = read_ceph_args(Args),

    {true, rest_replier:handle_service_action_async(Req, service:apply_async(
        ?SERVICE_CEPH, deploy, Ctx
    ), Version)}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req(), State :: rest_handler:state()}.
provide_resource(Req, #rstate{resource = service_ceph}) ->
    {rest_replier:format_service_step(service_ceph, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_details
        ))
    ), Req};

provide_resource(Req, #rstate{resource = ceph_pool, bindings = #{name := Name}}) ->
    {rest_replier:format_service_step(ceph_pool, get,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_pool, #{name => Name}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = ceph_pools}) ->
    {rest_replier:format_ceph_pools(), Req};

provide_resource(Req, #rstate{resource = ceph_usage}) ->
    {rest_replier:format_service_step(service_ceph, get_usage,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_usage, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = ceph_pool_usage, bindings = #{name := Name}}) ->
    {rest_replier:format_service_step(service_ceph, get_usage,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_usage, #{pool => Name}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = ceph_osd_usage, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_ceph_osd, get_usage_by_id,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_OSD, get_usage_by_id, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = service_ceph_osd, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_ceph_osd, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_OSD, get_details, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = service_ceph_osds}) ->
    {rest_replier:format_service_step(service_ceph_osd, list,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_OSD, list
        ))
    ), Req};

provide_resource(Req, #rstate{resource = service_ceph_monitor, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_ceph_mon, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_MON, get_details, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = service_ceph_monitors}) ->
    {rest_replier:format_service_step(service_ceph_mon, list,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_MON, list
        ))
    ), Req};


provide_resource(Req, #rstate{resource = service_ceph_manager, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_ceph_mgr, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_MGR, get_details, #{id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = service_ceph_managers}) ->
    {rest_replier:format_service_step(service_ceph_mgr, list,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_MGR, list
        ))
    ), Req};


provide_resource(Req, #rstate{resource = ceph_status}) ->
    {rest_replier:format_service_step(service_ceph, get_health_report,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_health_report
        ))
    ), Req};

provide_resource(Req, #rstate{resource = block_devices, params = #{host := Host}}) ->
    {rest_replier:format_service_step(service_ceph_osd, get_disks,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH_OSD, get_disks, #{hosts => [binary_to_list(Host)]}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, _State) ->
    {false, Req}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_osd_params(map()) -> service:ctx().
get_osd_params(#{host := Host, type := Type} = Spec) ->
    Params = maps:merge(maps:with([device, size, path, uuid], Spec), #{
        type => onepanel_utils:convert(Type, atom),
        host => onepanel_utils:convert(Host, list)
    }),
    case Params of
        #{uuid := UUID} -> Params#{uuid => normalize_uuid(UUID, [osds])};
        _ -> Params#{uuid => ceph:gen_uuid()}
    end.


-spec get_mon_params(map()) -> service:ctx().
get_mon_params(#{host := Host} = Spec) ->
    maps:with([host, ip], Spec#{host => onepanel_utils:convert(Host, list)}).


-spec get_mgr_params(map()) -> service:ctx().
get_mgr_params(#{host := Host}) ->
    #{host => onepanel_utils:convert(Host, list)}.


%%--------------------------------------------------------------------
%% @private
%% @doc Ensures UUID is hyphenated.
%% Argument Keys is used to indicate error reason.
%% @end
%%--------------------------------------------------------------------
-spec normalize_uuid(binary(), Keys :: [atom()]) -> <<_:256>>.
normalize_uuid(<<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>>, Keys) ->
    onepanel_utils:join([A, B, C, D, E], <<"-">>);

normalize_uuid(<<UUIDWithHyphens/binary>>, Keys) when byte_size(UUIDWithHyphens) > 32 ->
    case binary:replace(UUIDWithHyphens, <<"-">>, <<>>, [global]) of
        <<Cleaned:32/binary>> ->
            normalize_uuid(Cleaned, Keys);
        _ ->
            throw(?ERROR_BAD_VALUE_IDENTIFIER(
                str_utils:join_as_binaries(Keys ++ [uuid], <<$.>>)))
    end;

normalize_uuid(_, Keys) ->
    throw(?ERROR_BAD_VALUE_IDENTIFIER(
        str_utils:join_as_binaries(Keys ++ [uuid], <<$.>>))).
