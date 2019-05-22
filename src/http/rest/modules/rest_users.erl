%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(rest_users).
-author("Wojciech Geisler").

-include("names.hrl").
-include("http/rest.hrl").
-include("authentication.hrl").
-include("modules/models.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_possible/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

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
is_authorized(Req, _Method, #rstate{client = #client{role = root}}) ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{client = #client{role = member}}) ->
    {true, Req};
is_authorized(Req, _Method, #rstate{client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_UPDATE), Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = current_user, client = Client}) ->
    {Client#client.role == member, Req};

exists_resource(Req, #rstate{resource = onezone_user, bindings = #{id := Id}}) ->
    {onezone_users:user_exists(Id), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_possible/4}
%% @end
%%--------------------------------------------------------------------
accept_possible(Req, _Method, _Args, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, _Method, #rstate{resource = onezone_user}) ->
    {service:all_healthy(), Req};

is_available(Req, _Method, #rstate{resource = onezone_users}) ->
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
accept_resource(Req, 'POST', Args, #rstate{resource = onezone_users}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE_OZ, add_users, #{onezone_users => [Args]}
    ))};

accept_resource(Req, 'PATCH', Args,
    #rstate{resource = onezone_user, bindings = #{id := Id}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE_OZ, set_user_password, #{
            user_id => Id,
            new_password => maps:get(newPassword, Args)
        }
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = onezone_users}) ->
    {rest_replier:format_service_step(onezone_users, list_users,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_OZ, list_users, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = onezone_user, bindings = #{id := Id}}) ->
    {rest_replier:format_service_step(onezone_users, get_user,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_OZ, get_user, #{user_id => Id}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = current_user, client = Client}) when
    Client#client.role == member ->

    #client{privileges = Privileges, user = User} = Client,
    #user_details{full_name = FullName, id = Id} = User,
    {#{
        username => FullName, userId => Id,
        clusterPrivileges => Privileges
    }, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, _State) ->
    {false, Req}.
