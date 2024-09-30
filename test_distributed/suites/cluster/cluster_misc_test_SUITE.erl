%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone/Oneprovider clusters miscellaneous functionality .
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_misc_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    service_oneprovider_fetch_compatibility_registry_test/1
]).

all() -> [
    service_oneprovider_fetch_compatibility_registry_test
].


%%%===================================================================
%%% Tests
%%%===================================================================


service_oneprovider_fetch_compatibility_registry_test(_Config) ->
    % place some initial, outdated compatibility registry on all nodes
    OpPanelNodes = panel_test_utils:get_panel_nodes(krakow),
    OldRevision = 2000010100,
    lists:foreach(fun(Node) ->
        CurrentRegistryPath = rpc:call(Node, ctool, get_env, [current_compatibility_registry_file]),
        DefaultRegistryPath = rpc:call(Node, ctool, get_env, [default_compatibility_registry_file]),
        OldRegistry = #{<<"revision">> => OldRevision},
        ok = rpc:call(Node, ctool, set_env, [compatibility_registry_mirrors, []]),
        ok = rpc:call(Node, file, write_file, [CurrentRegistryPath, json_utils:encode(OldRegistry)]),
        ok = rpc:call(Node, file, write_file, [DefaultRegistryPath, json_utils:encode(OldRegistry)]),
        ok = rpc:call(Node, compatibility, clear_registry_cache, [])
    end, OpPanelNodes),

    % force a registry query that should cause Onepanel to fetch a newer one from Onezone
    ChosenNode = lists_utils:random_element(OpPanelNodes),
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(ChosenNode, <<"/provider/onezone_info">>, #{auth => root})
    ),

    NewerRevision = peek_current_registry_revision_on_node(ChosenNode),
    ?assertNotEqual(NewerRevision, OldRevision),

    % in the process, the new registry should be propagated to all nodes
    lists:foreach(fun(Node) ->
        ?assertEqual(NewerRevision, peek_current_registry_revision_on_node(Node))
    end, OpPanelNodes -- [ChosenNode]).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec peek_current_registry_revision_on_node(node()) -> integer().
peek_current_registry_revision_on_node(Node) ->
    Resolver = compatibility:build_resolver([Node], []),
    {ok, Rev} = rpc:call(Node, compatibility, peek_current_registry_revision, [Resolver]),
    Rev.
