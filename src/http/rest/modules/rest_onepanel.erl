%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /hosts REST resources.
%%%-------------------------------------------------------------------
-module(rest_onepanel).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, is_conflict/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

-define(SERVICE, service_onepanel:name()).
-define(LE_SERVICE, service_letsencrypt:name()).

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

is_authorized(Req, _Method, #rstate{resource = configuration}) ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{resource = node}) ->
    {true, Req};

is_authorized(Req, 'POST', #rstate{resource = cluster}) ->
    {service_onepanel:available_for_clustering(), Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = host, bindings = #{host := Host}}) ->
    {lists:member(Host, service_onepanel:get_hosts()), Req};

exists_resource(Req, #rstate{resource = web_cert}) ->
    {model:exists(service) andalso service:exists(service_letsencrypt:name()), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_conflict/4}
%% @end
%%--------------------------------------------------------------------
is_conflict(Req, _Method, _Args, #rstate{resource = cluster}) ->
    {not service_onepanel:available_for_clustering(), Req};

is_conflict(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, _Method, #rstate{resource = web_cert}) ->
    {service:all_healthy(), Req};

is_available(Req, _Method, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', #{clusterHost := Host} = Args, #rstate{resource = cluster}) ->
    Ctx = onepanel_maps:get_store(cookie, Args, cookie, #{
        cluster_host => onepanel_utils:convert(Host, list)
    }),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, join_cluster, Ctx
    ))};


accept_resource(Req, 'POST', #{address := Address},
    #rstate{resource = hosts, version = ApiVersion}) ->
    {true, cowboy_req:set_resp_body(json_utils:encode(
        rest_replier:format_service_step(service_onepanel, extend_cluster,
            service_utils:throw_on_error(service:apply_sync(
                ?SERVICE, extend_cluster, #{address => Address, api_version => ApiVersion}
            )))
    ), Req)};

accept_resource(Req, 'PATCH', Args, #rstate{resource = web_cert}) ->
    Ctx = onepanel_maps:get_store(letsEncrypt, Args, letsencrypt_enabled),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?LE_SERVICE, update, Ctx
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = cookie}) ->
    {erlang:get_cookie(), Req};

provide_resource(Req, #rstate{resource = node}) ->
    Hostname = onepanel_utils:convert(onepanel_cluster:node_to_host(), binary),
    ReleaseType = onepanel_env:get(release_type),
    {#{hostname => Hostname, componentType => ReleaseType}, Req};

provide_resource(Req, #rstate{resource = hosts}) ->
    Hosts = service_onepanel:get_hosts(),
    {lists:sort(onepanel_utils:convert(Hosts, {seq, binary})), Req};

provide_resource(Req, #rstate{resource = configuration}) ->
    {rest_replier:format_onepanel_configuration(), Req};

provide_resource(Req, #rstate{resource = web_cert}) ->
    {rest_replier:format_service_step(service_letsencrypt, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?LE_SERVICE, get_details, #{}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = host, bindings = #{host := Host}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, leave_cluster, #{hosts => [Host]}
    ))}.
