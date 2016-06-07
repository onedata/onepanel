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
-module(service_oneprovider).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([get_steps/2]).

%% API
-export([name/0]).

-define(NAME, oneprovider).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_OP, service_op_worker:name()).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    CbCtx = maps:get(?SERVICE_CB, Ctx),
    CmCtx = maps:get(?SERVICE_CM, Ctx),
    OpwCtx = maps:get(?SERVICE_OP, Ctx),
    OpCtx = maps:get(?NAME, Ctx, #{}),
    Register = fun
        (#{register := true}) -> true;
        (_) -> false
    end,
    [
        #steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        #step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        #steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        #step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        #steps{service = ?SERVICE_OP, action = deploy, ctx = OpwCtx},
        #step{service = ?SERVICE_OP, function = status, ctx = OpwCtx},
        #step{service = ?SERVICE_OP, function = wait_for_init, ctx = OpwCtx,
            selection = first},
        #steps{action = register, ctx = OpCtx, condition = Register}
    ];

get_steps(start, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = start},
        #steps{service = ?SERVICE_CM, action = start},
        #steps{service = ?SERVICE_OP, action = start}
    ];

get_steps(stop, _Ctx) ->
    [
        #steps{service = ?SERVICE_OP, action = stop},
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
        #steps{service = ?SERVICE_OP, action = status, ignore_errors = true}
    ];

get_steps(register, _Ctx) ->
    [].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?NAME.