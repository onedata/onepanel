%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Common utilities for panel Onezone GUI message middleware handlers.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_gui_message_middleware_handler_utils).
-author("Bartosz Walkowicz").

-include("deployment_progress.hrl").
-include("middleware/middleware.hrl").

-export([validate/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec validate(binary()) -> ok | no_return().
validate(MessageId) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end,
    case service:get_hosts(?SERVICE_OZW) /= [] andalso oz_worker_rpc:gui_message_exists(MessageId) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.
