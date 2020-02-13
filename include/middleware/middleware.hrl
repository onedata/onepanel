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

-include_lib("ctool/include/aai/aai.hrl").

-record(onp_req, {
    gri :: gri:gri(),
    operation :: middleware:operation(),
    client :: middleware:client(),
    data = undefined :: middleware:data(),
    data_spec = undefined :: undefined | onepanel_parser:object_spec(),
    % applicable for create/get requests - returns the revision of resource
    return_revision = false :: boolean()
}).


-endif.
