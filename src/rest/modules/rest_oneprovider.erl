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
-include("modules/logger.hrl").
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
%% @doc @see rest_behaviour:is_authorized/3
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:exists_resource/2
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:accept_resource/4
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'PUT', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:store(onezone_domain, onezoneDomainName, Args),
    Ctx2 = onepanel_maps:store(oneprovider_name, name, Args, Ctx),
    Ctx3 = onepanel_maps:store(oneprovider_redirection_point, redirectionPoint, Args, Ctx2),
    Ctx4 = onepanel_maps:store(oneprovider_geo_latitude, geoLatitude, Args, Ctx3),
    Ctx5 = onepanel_maps:store(oneprovider_geo_longitude, geoLongitude, Args, Ctx4),

    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, register, Ctx5
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = provider}) ->
    Ctx = onepanel_maps:store(oneprovider_name, name, Args, Args),
    Ctx2 = onepanel_maps:store(oneprovider_redirection_point, redirectionPoint, Args, Ctx),
    Ctx3 = onepanel_maps:store(oneprovider_geo_latitude, geoLatitude, Args, Ctx2),
    Ctx4 = onepanel_maps:store(oneprovider_geo_longitude, geoLongitude, Args, Ctx3),

    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, modify_details, Ctx4
    ))};

accept_resource(Req, 'PUT', Args, #rstate{resource = spaces}) ->
    Ctx = onepanel_maps:store(name, name, Args, Args),
    Ctx2 = onepanel_maps:store(token, token, Args, Ctx),
    Ctx3 = onepanel_maps:store(size, size, Args, Ctx2),
    Ctx4 = onepanel_maps:store(storageId, storageId, Args, Ctx3),
    Ctx5 = onepanel_maps:store(storageName, storageName, Args, Ctx4),

    rest_utils:verify_any([storageId, storageName], Args),

    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, support_space, Ctx5
    ))};

accept_resource(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:provide_resource/2
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = provider}) ->
    {rest_utils:format_service_step(service_oneprovider, get_details,
        service_executor:apply_sync(?SERVICE, get_details, #{})
    ), Req};

provide_resource(Req, #rstate{resource = spaces}) ->
    {rest_utils:format_service_step(service_oneprovider, get_spaces,
        service_executor:apply_sync(?SERVICE, get_spaces, #{})
    ), Req};

provide_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {rest_utils:format_service_step(service_oneprovider, get_space_details,
        service_executor:apply_sync(?SERVICE, get_space_details, #{id => Id})
    ), Req};

provide_resource(Req, _State) ->
    {[], Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = provider}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, unregister, #{}
    ))};

delete_resource(Req, #rstate{resource = space, bindings = #{id := Id}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, revoke_space_support, #{id => Id}
    ))};

delete_resource(Req, _State) ->
    {false, Req}.