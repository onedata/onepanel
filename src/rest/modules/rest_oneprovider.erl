%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /provider REST resources.
%%%-------------------------------------------------------------------
-module(rest_oneprovider).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

-define(SERVICE, service_oneprovider:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, _State) ->
    case service:get(?SERVICE) of
        {ok, #service{ctx = #{registered := true}}} -> {true, Req};
        {ok, #service{}} -> {false, Req};
        #error{reason = ?ERR_NOT_FOUND} -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:get_store(onezoneDomainName, Args, onezone_domain),
    Ctx2 = onepanel_maps:get_store(name, Args, oneprovider_name, Ctx),
    Ctx3 = onepanel_maps:get_store(redirectionPoint, Args, oneprovider_redirection_point, Ctx2),
    Ctx4 = onepanel_maps:get_store(geoLatitude, Args, oneprovider_geo_latitude, Ctx3),
    Ctx5 = onepanel_maps:get_store(geoLongitude, Args, oneprovider_geo_longitude, Ctx4),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, register, Ctx5
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:get_store(name, Args, oneprovider_name, Args),
    Ctx2 = onepanel_maps:get_store(redirectionPoint, Args, oneprovider_redirection_point, Ctx),
    Ctx3 = onepanel_maps:get_store(geoLatitude, Args, oneprovider_geo_latitude, Ctx2),
    Ctx4 = onepanel_maps:get_store(geoLongitude, Args, oneprovider_geo_longitude, Ctx3),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, modify_details, Ctx4
    ))};

accept_resource(Req, 'POST', Args, #rstate{resource = spaces}) ->
    Ctx = onepanel_maps:get_store(name, Args, name),
    Ctx2 = onepanel_maps:get_store(token, Args, token, Ctx),
    Ctx3 = onepanel_maps:get_store(size, Args, size, Ctx2),
    Ctx4 = onepanel_maps:get_store(storageId, Args, storage_id, Ctx3),
    Ctx5 = onepanel_maps:get_store(mountInRoot, Args, mount_in_root, Ctx4),
    Ctx6 = get_storage_import_args(Args, Ctx5),
    Ctx7 = get_storage_update_args(Args, Ctx6),

    {true, rest_replier:handle_service_step(Req, service_oneprovider, support_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, support_space, Ctx7
        ))
    )};

accept_resource(Req, 'PATCH', Args, #rstate{resource=space, bindings = #{id := Id}}) ->
    Ctx2 = get_storage_update_args(Args),
    Ctx3 = get_storage_import_args(Args, Ctx2),
    Ctx4 = Ctx3#{space_id => Id},

    {true, rest_replier:handle_service_step(Req, service_oneprovider, modify_space,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, modify_space, Ctx4
        ))
    )}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = provider}) ->
    {rest_replier:format_service_step(service_oneprovider, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_details, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = spaces}) ->
    {rest_replier:format_service_step(service_oneprovider, get_spaces,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_spaces, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(service_oneprovider, get_space_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_space_details, #{id => Id}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = provider}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, unregister, #{}
    ))};

delete_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, revoke_space_support, #{id => Id}
    ))}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc @equiv get_storage_update_args(Args, #{}).
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_args(Args :: rest_handler:args()) -> service:ctx().
get_storage_update_args(Args) ->
    get_storage_update_args(Args, #{}).

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_update.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_update_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_storage_update_args(Args, Ctx) ->
    Ctx2 = onepanel_maps:get_store([storageUpdate, strategy], Args,
        [storage_update, strategy], Ctx),
    Ctx3 = onepanel_maps:get_store([storageUpdate, maxDepth], Args,
        [storage_update, max_depth], Ctx2),
    Ctx4 = onepanel_maps:get_store([storageUpdate, writeOnce], Args,
        [storage_update, write_once], Ctx3),
    Ctx5 = onepanel_maps:get_store([storageUpdate, deleteEnable], Args,
        [storage_update, delete_enable], Ctx4),
    onepanel_maps:get_store([storageUpdate, scanInterval], Args,
        [storage_update, scan_interval], Ctx5).

%%-------------------------------------------------------------------
%% @private
%% @doc Parse args for storage_import.
%% @end
%%-------------------------------------------------------------------
-spec get_storage_import_args(Args :: rest_handler:args(), Ctx :: service:ctx())
        -> service:ctx().
get_storage_import_args(Args, Ctx) ->
    Ctx2 = onepanel_maps:get_store([storageImport, strategy], Args,
        [storage_import, strategy], Ctx),
    onepanel_maps:get_store([storageImport, maxDepth], Args,
        [storage_import, max_depth], Ctx2).