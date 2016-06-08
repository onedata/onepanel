%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /cluster/managers REST resources.
%%%-------------------------------------------------------------------
-module(rest_cluster_manager).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([routes/0, is_authorized/4, resource_exists/2, accept_resource/6,
    provide_resource/3, delete_resource/2]).

-define(SERVICE, service_cluster_manager:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback routes/0
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), rest_handler, State :: rest_handler:rstate()}].
routes() ->
    State = #rstate{module = ?MODULE},
    Module = rest_handler,
    [
        {<<"/cluster/managers">>, Module, State#rstate{resource = cms,
            methods = [get, patch]}},
        {<<"/cluster/managers/:host">>, Module, State#rstate{resource = cms,
            methods = [get, patch]}}
    ].


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback is_authorized/4
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: rest_handler:resource(),
    Method :: rest_handler:method(), Ctx :: rest_handler:ctx(),
    Client :: rest_handler:client()) -> boolean().
is_authorized(_Resource, _Method, _Ctx, #client{role = admin}) ->
    true;
is_authorized(_Resource, _Method, _Ctx, _Client) ->
    false.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback resource_exists/2
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> boolean().
resource_exists(_Resource, #{bindings := #{host := Host}}) ->
    service:member(?SERVICE, Host);

resource_exists(_Resource, _Ctx) ->
    service:exists(?SERVICE).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback accept_resource/5
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: rest_handler:resource(),
    Method :: rest_handler:accept_method(), Ctx :: rest_handler:ctx(),
    Data :: rest_handler:data(), Client :: rest_handler:client(),
    Req :: cowboy_req:req()) -> {boolean(), cowboy_req:req()} | no_return().
accept_resource(cms, patch, #{qs_vals := #{<<"started">> := <<"true">>},
    bindings := #{host := Host}}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        ?SERVICE, start, #{hosts => [Host]}
    ), Req)};

accept_resource(cms, patch, #{qs_vals := #{<<"started">> := <<"true">>}}, _Data,
    _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        ?SERVICE, start, #{}
    ), Req)};

accept_resource(cms, patch, #{qs_vals := #{<<"started">> := <<"false">>},
    bindings := #{host := Host}}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        ?SERVICE, stop, #{hosts => [Host]}
    ), Req)};

accept_resource(cms, patch, #{qs_vals := #{<<"started">> := <<"false">>}},
    _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        ?SERVICE, stop, #{}
    ), Req)};

accept_resource(_Resource, _Method, _Ctx, _Data, _Client, _Req) ->
    rest_utils:report_error(invalid_request).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback provide_resource/3
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) ->
    Data :: rest_handler:data().
provide_resource(cms, #{bindings := #{host := Host}}, _Client) ->
    rest_utils:format_results(service_executor:apply_sync(
        ?SERVICE, status, #{hosts => [Host]}
    ));

provide_resource(cms, _Ctx, _Client) ->
    rest_utils:format_results(service_executor:apply_sync(?SERVICE, status, #{})).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> no_return().
delete_resource(_Resource, _Ctx) ->
    rest_utils:report_error(invalid_request).