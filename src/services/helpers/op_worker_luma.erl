%%%-------------------------------------------------------------------
%%% @author Jakub Kudzia
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_luma).
-author("Jakub Kudzia").

-include("modules/errors.hrl").

%% API
-export([execute/2]).

-type storage_id() :: binary().
-type space_id() :: binary().
-type onedata_user_id() :: binary().
-type uid() :: integer().
-type acl_principal() :: integer().
-type storage() :: term(). % todo
-type mapping() :: term(). % todo
-type credentials() :: term(). % todo

%%%===================================================================
%%% API functions
%%%===================================================================

execute(Operation, Ctx) ->
    % todo a moze by tak te funkcje zdefiniowac tutaj, podawac im Ctx i podawac im funy?
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
