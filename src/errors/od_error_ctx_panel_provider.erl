%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Implementation of od_error_ctx_provider_behaviour for panel service.
%%% @end
%%%-------------------------------------------------------------------
-module(od_error_ctx_panel_provider).
-author("Bartosz Walkowicz").

-behaviour(od_error_ctx_provider_behaviour).

-include_lib("ctool/include/onedata.hrl").

%% od_error_ctx_provider_behaviour callbacks
-export([
    service/0,
    service_id/0,
    service_domain/0,
    service_release_version/0,
    service_build_version/0
]).


%%%===================================================================
%%% od_error_ctx_provider_behaviour callbacks
%%%===================================================================


-spec service() -> ?OZ_PANEL | ?OP_PANEL.
service() ->
    case onepanel_env:get_cluster_type() of
        ?ONEZONE -> ?OZ_PANEL;
        ?ONEPROVIDER -> ?OP_PANEL
    end.


-spec service_id() -> undefined | onedata:service_id().
service_id() ->
    try clusters:get_id() catch _:_ -> undefined end.


-spec service_domain() -> undefined | binary().
service_domain() ->
    Config = middleware_utils:format_onepanel_configuration(),

    case onepanel_env:get_cluster_type() of
        ?ONEZONE -> maps:get(zoneDomain, Config, undefined);
        ?ONEPROVIDER -> maps:get(providerDomain, Config, undefined)
    end.


-spec service_release_version() -> onedata:release_version().
service_release_version() -> onepanel:get_release_version().


-spec service_build_version() -> binary().
service_build_version() -> onepanel:get_build_version().
