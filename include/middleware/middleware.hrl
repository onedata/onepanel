%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Record definitions for the middleware modules.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_MIDDLEWARE_HRL).
-define(ONEPANEL_MIDDLEWARE_HRL, 1).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("names.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").


-record(onp_req_ctx, {
    interface :: middleware_handler:interface(),
    gri :: gri:gri(),
    operation :: middleware_handler:operation(),
    client :: middleware:client()
}).

-record(onp_req_state, {
    ctx :: middleware_handler:req_ctx(),
    input :: middleware_handler:handler_input()
}).


-record(onp_req, {
    gri :: gri:gri(),
    operation :: middleware_handler:operation(),
    client :: middleware:client(),
    data = undefined :: middleware_handler:interface_input(),
    data_spec = undefined :: undefined | onepanel_parser:object_spec(),
    % applicable for create/get requests - returns the revision of resource
    return_revision = false :: boolean()
}).


-endif.
