%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /cluster/workers REST resources.
%%%-------------------------------------------------------------------
-module(rest_cluster_worker).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").
-include_lib("ctool/include/logging.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([routes/0, is_authorized/4, resource_exists/2, accept_resource/6,
    provide_resource/3, delete_resource/2]).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback routes/0
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), rest_handler, State :: rest_handler:rstate()}].
routes() ->
    State = #rstate{},
    Module = rest_handler,
    [
        {<<"/cluster/workers">>, Module, State#rstate{resource = wrks,
            methods = [get, patch]}},
        {<<"/cluster/workers/:host">>, Module, State#rstate{resource = wrks,
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
resource_exists(_Resource, #{service := Name, bindings := #{host := Host}}) ->
    service:member(Name, Host);

resource_exists(_Resource, #{service := Name}) ->
    service:exists(Name).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback accept_resource/5
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: rest_handler:resource(),
    Method :: rest_handler:accept_method(), Ctx :: rest_handler:ctx(),
    Data :: rest_handler:data(), Client :: rest_handler:client(),
    Req :: cowboy_req:req()) -> {boolean(), cowboy_req:req()} | no_return().
accept_resource(wrks, patch, #{qs_vals := #{<<"started">> := <<"true">>},
    bindings := #{host := Host}, service := Name}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        Name, start, #{hosts => [Host]}
    ), Req)};

accept_resource(wrks, patch, #{qs_vals := #{<<"started">> := <<"true">>},
    service := Name}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        Name, start, #{}
    ), Req)};

accept_resource(wrks, patch, #{qs_vals := #{<<"started">> := <<"false">>},
    bindings := #{host := Host}, service := Name}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        Name, stop, #{hosts => [Host]}
    ), Req)};

accept_resource(wrks, patch, #{qs_vals := #{<<"started">> := <<"false">>},
    service := Name}, _Data, _Client, Req) ->
    {true, rest_utils:set_results(service_executor:apply_sync(
        Name, stop, #{}
    ), Req)};

accept_resource(_Resource, _Method, _Ctx, _Data, _Client, _Req) ->
    rest_utils:report_error(invalid_request).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback provide_resource/3
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) ->
    Data :: rest_handler:data().
provide_resource(wrks, #{bindings := #{host := Host}, service := Name}, _Client) ->
    rest_utils:format_results(service_executor:apply_sync(
        Name, status, #{hosts => [Host]}
    ));

provide_resource(wrks, #{service := Name}, _Client) ->
    rest_utils:format_results(service_executor:apply_sync(Name, status, #{})).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> no_return().
delete_resource(_Resource, _Ctx) ->
    rest_utils:report_error(invalid_request).