%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_onezone).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/logger.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_steps/2]).

-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_OZ, service_oz_worker:name()).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:name/0
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    onezone.


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    CbCtx = maps:get(?SERVICE_CB, Ctx),
    CmCtx = maps:get(?SERVICE_CM, Ctx),
    OzCtx = maps:get(?SERVICE_OZ, Ctx),
    [
        #steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        #step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        #steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        #step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        #steps{service = ?SERVICE_OZ, action = deploy, ctx = OzCtx},
        #step{service = ?SERVICE_OZ, function = status, ctx = OzCtx},
        #step{service = ?SERVICE_OZ, function = wait_for_init, ctx = OzCtx,
            selection = first}
    ];

get_steps(start, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = start},
        #steps{service = ?SERVICE_CM, action = start},
        #steps{service = ?SERVICE_OZ, action = start}
    ];

get_steps(stop, _Ctx) ->
    [
        #steps{service = ?SERVICE_OZ, action = stop},
        #steps{service = ?SERVICE_CM, action = stop},
        #steps{service = ?SERVICE_CB, action = stop}
    ];

get_steps(restart, _Ctx) ->
    [
        #steps{action = stop},
        #steps{action = start}
    ];

get_steps(status, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = status, ignore_errors = true},
        #steps{service = ?SERVICE_CM, action = status, ignore_errors = true},
        #steps{service = ?SERVICE_OZ, action = status, ignore_errors = true}
    ];

get_steps(Action, _Ctx) ->
    ?throw({action_not_supported, Action}).

