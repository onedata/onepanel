%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C): 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /zone REST resources.
%%%-------------------------------------------------------------------
-module(rest_onezone).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

-define(SERVICE, service_onezone:name()).

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
        {ok, #service{}} -> {true, Req};
        #error{reason = ?ERR_NOT_FOUND} -> {false, Req}
    end.

accept_resource(Req, 'PATCH', Args, #rstate{resource = cluster_ips}) ->
    {ok, ClusterIps} = onepanel_maps:get(hosts, Args),
    Ctx = #{cluster_ips => keys_binary_to_list(ClusterIps)},

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, set_cluster_ips, Ctx
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = cluster_ips}) ->
    {rest_replier:format_service_step(service_onezone, get_cluster_ips,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, get_cluster_ips, #{}
        ))
    ), Req}.

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    no_return().
delete_resource(_Req, #rstate{}) ->
    ?throw_error(?ERR_NOT_FOUND).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @private
%% @doc Converts keys of a map from binaries to lists
%% @end
%%-------------------------------------------------------------------
-spec keys_binary_to_list(#{binary() => term()}) -> #{string() => term()}.
keys_binary_to_list(Map) ->
    KeyVals = maps:to_list(Map),
    maps:from_list(lists:map(fun({K, V}) -> {binary_to_list(K), V} end, KeyVals)).

