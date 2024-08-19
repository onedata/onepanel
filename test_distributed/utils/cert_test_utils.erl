%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for Lets Encrypt tests.
%%% @end
%%%--------------------------------------------------------------------
-module(cert_test_utils).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    get_cert_details/1,

    enable_lets_encrypt/1,
    disable_lets_encrypt/1,

    deploy_certs/3
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_cert_details(oct_background:entity_selector()) -> json_utils:json_map().
get_cert_details(EntitySelector) ->
    {ok, _, _, CertDetails} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(EntitySelector, <<"/web_cert">>, #{auth => root})
    ),
    CertDetails.


-spec enable_lets_encrypt(oct_background:entity_selector()) -> ok.
enable_lets_encrypt(EntitySelector) ->
    update_lets_encrypt(EntitySelector, true).


-spec disable_lets_encrypt(oct_background:entity_selector()) -> ok.
disable_lets_encrypt(EntitySelector) ->
    update_lets_encrypt(EntitySelector, false).


%%--------------------------------------------------------------------
%% @doc
%% Writs predefined certificate files on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec deploy_certs(oct_background:entity_selector(), map(), test_config:config()) ->
    ok.
deploy_certs(EntitySelector, SourcePaths, Config) ->
    Nodes = get_panel_nodes(EntitySelector),

    lists:foreach(fun({FileType, Path}) ->

        {ok, Content} = file:read_file(?TEST_FILE(Config, Path)),
        {Result, []} = utils:rpc_multicall(Nodes, erlang, apply, [fun() ->
            Dest = onepanel_env:get(FileType),
            file:write_file(Dest, Content)
        end, []]),
        ExpResult = lists:duplicate(length(Nodes), ok),
        ?assertEqual(ExpResult, Result)

    end, maps:to_list(SourcePaths)).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
update_lets_encrypt(EntitySelector, Enabled) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(EntitySelector, <<"/web_cert">>, #{auth => root, json => #{
            <<"letsEncrypt">> => Enabled
        }})
    ),
    ok.


%% @private
get_panel_nodes(zone) -> oct_background:get_zone_panels();
get_panel_nodes(ProviderSelector) -> oct_background:get_provider_panels(ProviderSelector).
