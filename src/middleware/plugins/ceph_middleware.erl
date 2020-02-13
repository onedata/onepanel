%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_ceph type.
%%% Handles management of Ceph services and Ceph pools.
%%% @end
%%%-------------------------------------------------------------------
-module(ceph_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([read_ceph_args/1]).

%% middleware_plugin callbacks
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Extracts request parameters relevant to Ceph.
%% @end
%%--------------------------------------------------------------------
-spec read_ceph_args(middleware:data()) -> {CephCtx :: #{
    cluster_name => binary(), fsid => ceph:uuid(),
    osds | monitors | managers => [service:ctx(), ...]
}, CephHosts :: [service:host()]}.
read_ceph_args(Data) ->
    Osds = lists:map(fun get_osd_params/1, maps:get(osds, Data, [])),
    Mons = lists:map(fun get_mon_params/1, maps:get(monitors, Data, [])),
    Mgrs = lists:map(fun get_mgr_params/1, maps:get(managers, Data, [])),

    Ctx = kv_utils:copy_found([
        {name, cluster_name},
        {fsid, fsid}
    ], Data, #{
        osds => Osds,
        monitors => Mons,
        managers => Mgrs
    }),
    Hosts = ceph:extract_hosts(Osds ++ Mons ++ Mgrs),
    IsNonempty = fun(_Key, []) -> false; (_, _) -> true end,
    {maps:filter(IsNonempty, Ctx), Hosts}.


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
operation_supported(create, cluster, private) -> onepanel:is_op_panel();
operation_supported(create, managers, private) -> onepanel:is_op_panel();
operation_supported(create, monitors, private) -> onepanel:is_op_panel();
operation_supported(create, osds, private) -> onepanel:is_op_panel();

operation_supported(get, global_params, private) -> onepanel:is_op_panel();
operation_supported(get, block_devices, private) -> onepanel:is_op_panel();
operation_supported(get, status, private) -> onepanel:is_op_panel();
operation_supported(get, usage, private) -> onepanel:is_op_panel();

operation_supported(get, managers, private) -> onepanel:is_op_panel();
operation_supported(get, {manager, _Id}, private) -> onepanel:is_op_panel();
operation_supported(get, monitors, private) -> onepanel:is_op_panel();
operation_supported(get, {monitor, _Id}, private) -> onepanel:is_op_panel();
operation_supported(get, osds, private) -> onepanel:is_op_panel();
operation_supported(get, {osd, _Id}, private) -> onepanel:is_op_panel();
operation_supported(get, {osd_usage, _Id}, private) -> onepanel:is_op_panel();

operation_supported(get, pools, private) -> onepanel:is_op_panel();
operation_supported(get, {pool, _Name}, private) -> onepanel:is_op_panel();
operation_supported(get, {pool_usage, _Name}, private) -> onepanel:is_op_panel();

operation_supported(update, {pool, _Name}, private) -> onepanel:is_op_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(_, _, _) -> [].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = get, client = #client{role = member}
}, _) ->
    true;

authorize(#onp_req{
    operation = Op, client = #client{role = member} = Client
}, _) when
    Op == create;
    Op == update
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = Aspect}}, _) when
    Aspect == cluster;
    Aspect == managers;
    Aspect == monitors;
    Aspect == osds
->
    ok;

validate(#onp_req{operation = get, gri = #gri{aspect = Aspect}}, _) when
    Aspect == global_params;
    Aspect == managers;
    Aspect == monitors;
    Aspect == osds;
    Aspect == status;
    Aspect == usage;
    Aspect == pools
->
    case service:get_hosts(?SERVICE_CEPH) of
        [] -> throw(?ERROR_NOT_FOUND);
        _ -> ok
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = {Aspect, Id}}}, _) when
    Aspect == manager;
    Aspect == monitor;
    Aspect == osd;
    Aspect == osd_usage
->
    Service = aspect_to_service(Aspect),
    Module = service:get_module(Service),
    case service:exists(Service) andalso Module:exists(Id) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = {Aspect, Name}}}, _) when
    Aspect == pool;
    Aspect == pool_usage
->
    case service:get_hosts(?SERVICE_CEPH) /= []
        andalso ceph_pool:exists(Name) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = block_devices}}, _) ->
    ok;

validate(#onp_req{operation = update, gri = #gri{aspect = {pool, Name}}}, _) ->
    case service:get_hosts(?SERVICE_CEPH) /= []
        andalso ceph_pool:exists(Name) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.



-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = Aspect}, data = Data}) when
    % all those operations use a subset of the same model
    Aspect == cluster;
    Aspect == managers;
    Aspect == monitors;
    Aspect == osds
->
    {Ctx, _} = read_ceph_args(Data),
    {ok, value, _TaskId = service:apply_async(?SERVICE_CEPH, deploy, Ctx)}.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = global_params}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_details
    )};

get(#onp_req{gri = #gri{aspect = status}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_health_report
    )};

get(#onp_req{gri = #gri{aspect = usage}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_usage
    )};

get(#onp_req{gri = #gri{aspect = block_devices}, data = Data}, _) ->
    Host = maps:get(host, Data),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH_OSD, get_disks, #{hosts => [binary_to_list(Host)]}
    )};

get(#onp_req{gri = #gri{aspect = pools}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_all_pools, #{},
        ceph_pool, get_all
    )};

get(#onp_req{gri = #gri{aspect = {pool, Name}}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_pool, #{name => Name},
        ceph_pool, get
    )};

get(#onp_req{gri = #gri{aspect = {pool_usage, Name}}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH, get_usage, #{pool => Name}
    )};

get(#onp_req{gri = #gri{aspect = Aspect}}, _) when
    Aspect == managers;
    Aspect == monitors;
    Aspect == osds
->
    {ok, value, middleware_utils:result_from_service_action(
        aspect_to_service(Aspect), list
    )};

get(#onp_req{gri = #gri{aspect = {Aspect, Id}}}, _) when
    Aspect == manager;
    Aspect == monitor;
    Aspect == osd
->
    {ok, value, middleware_utils:result_from_service_action(
        aspect_to_service(Aspect), get_details, #{id => Id}
    )};

get(#onp_req{gri = #gri{aspect = {osd_usage, Id}}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_CEPH_OSD, get_usage_by_id, #{id => Id}
    )}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = {pool, Name}}, data = Data}) ->
    % Reuse camelCase params as used in the rest API.
    % while camelCase is not common in internal names, it matches storage
    % args convention.
    Ctx = Data#{name => Name},
    middleware_utils:execute_service_action(?SERVICE_CEPH, modify_pool, Ctx).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


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
normalize_uuid(<<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>>, _Keys) ->
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


%%--------------------------------------------------------------------
%% @private
%% @doc Returns name of a service relevant for handling given aspect.
%% @end
%%--------------------------------------------------------------------
-spec aspect_to_service(atom()) -> service:name().
aspect_to_service(managers) -> ?SERVICE_CEPH_MGR;
aspect_to_service(manager) -> ?SERVICE_CEPH_MGR;
aspect_to_service(monitors) -> ?SERVICE_CEPH_MON;
aspect_to_service(monitor) -> ?SERVICE_CEPH_MON;
aspect_to_service(osds) -> ?SERVICE_CEPH_OSD;
aspect_to_service(osd) -> ?SERVICE_CEPH_OSD;
aspect_to_service(osd_usage) -> ?SERVICE_CEPH_OSD.
