%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains helper functions used for interaction
%%% with op-worker LUMA DB.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_luma).
-author("Jakub Kudzia").

-include("modules/errors.hrl").

%% API
-export([execute/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function calls Operation associated with LUMA DB in op-worker.
%% If isLocalFeedLumaRequest flag is set to true in Ctx it checks
%% whether feed for corresponding LUMA DB is currently set to `local`.
%% In case it is not `local` it throws ?ERROR_NOT_FOUND exception.
%% @end
%%--------------------------------------------------------------------
-spec execute(function(), service:step_ctx()) -> ok | {ok, term()} | {error, term()}.
execute(Operation, Ctx) ->
    IsNotLocalFeedLumaRequest = not maps:get(isLocalFeedLumaRequest, Ctx),
    StorageId = maps:get(id, Ctx),
    case IsNotLocalFeedLumaRequest orelse
        (op_worker_rpc:storage_get_luma_feed(StorageId) =:= local)
    of
        true -> case Operation() of
            ok -> ok;
            {ok, Result} -> Result;
            Error -> throw(Error)
        end;
        false -> throw(?ERROR_NOT_FOUND)
    end.
