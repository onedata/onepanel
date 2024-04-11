%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains oneS3 service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ones3).
-author("Bartosz Walkowicz").

-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([
    exists/0,
    create_service/1, add_service_host/1
]).


%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> service:name().
name() ->
    ?SERVICE_ONES3.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


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
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(create, #{hosts := Hosts}) ->
    NewHosts = lists_utils:subtract(Hosts, get_hosts()),

    [
        #step{function = create_service, selection = first},
        #step{hosts = NewHosts, function = add_service_host}
    ].


%%%===================================================================
%%% API functions
%%%===================================================================


-spec exists() -> boolean().
exists() ->
    service:exists(name()).


-spec create_service(service:step_ctx()) -> ok.
create_service(_Ctx) ->
    case service:create(#service{name = name()}) of
        {ok, _} -> ok;
        ?ERR_ALREADY_EXISTS -> ok
    end.


-spec add_service_host(service:step_ctx()) -> ok.
add_service_host(_Ctx) ->
    Host = hosts:self(),
    service:add_host(name(), Host).