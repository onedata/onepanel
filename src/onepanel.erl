%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module providing general information about the Onepanel.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel).
-author("Wojciech Geisler").

-include("names.hrl").
-include_lib("ctool/include/onedata.hrl").

%% API
-export([get_build_and_version/0]).
-export([is_oz_panel/0, is_op_panel/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns version information about the running app
%% @end
%%--------------------------------------------------------------------
-spec get_build_and_version() -> {BuildVersion :: binary(), AppVersion :: binary()}.
get_build_and_version() ->
    BuildVersion = case application:get_env(?APP_NAME, build_version, "unknown") of
        "" -> "unknown";
        Build -> Build
    end,
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    {list_to_binary(BuildVersion), list_to_binary(AppVersion)}.


%%--------------------------------------------------------------------
%% @doc Returns true if this Onepanel is bundled with a Onezone deployment.
%% @end
%%--------------------------------------------------------------------
-spec is_oz_panel() -> boolean().
is_oz_panel() ->
    onepanel_env:get_cluster_type() == ?ONEZONE.


%%--------------------------------------------------------------------
%% @doc Returns true if this Onepanel is bundled with a Oneprovider deployment.
%% @end
%%--------------------------------------------------------------------
-spec is_op_panel() -> boolean().
is_op_panel() ->
    onepanel_env:get_cluster_type() == ?ONEPROVIDER.
