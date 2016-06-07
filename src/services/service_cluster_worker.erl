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
-module(service_cluster_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("db/models.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = configure},
        #step{hosts = Hosts, function = start}
    ];

get_steps(start, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = start}];

get_steps(start, _) ->
    [#step{hosts = fetch, function = start}];

get_steps(stop, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = stop}];

get_steps(stop, _) ->
    [#step{hosts = fetch, function = stop}];

get_steps(restart, #{hosts := Hosts}) ->
    [
        #step{hosts = Hosts, function = stop},
        #step{hosts = Hosts, function = start}
    ];

get_steps(restart, _) ->
    [
        #step{hosts = fetch, function = stop},
        #step{hosts = fetch, function = start}
    ];

get_steps(status, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = status}];

get_steps(status, _) ->
    [#step{hosts = fetch, function = status}].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{name := Name, cm_nodes := CmNodes, db_nodes := DbNodes,
    app_config := AppConfig, app_config_path := AppConfigPath,
    vm_args_path := VmArgsPath} = Ctx) ->

    Host = onepanel_utils:node_to_host(),
    Node = onepanel_utils:host_to_node(Name, Host),

    ok = app_config_editor:write([Name, cm_nodes], CmNodes, AppConfigPath),
    ok = app_config_editor:write([Name, db_nodes], DbNodes, AppConfigPath),

    maps:fold(fun(Key, Value, _) ->
        ok = app_config_editor:write([Name, Key], Value, AppConfigPath)
    end, #{}, AppConfig),

    ok = vm_config_editor:write(<<"name">>, erlang:atom_to_binary(Node, utf8),
        VmArgsPath),
    ok = vm_config_editor:write(<<"setcookie">>,
        erlang:atom_to_binary(maps:get(cookie, Ctx, erlang:get_cookie()), utf8),
        VmArgsPath),

    service:create(#service{name = Name}),
    service:add_host(Name, Host).


%%--------------------------------------------------------------------
%% @doc @see service:start/1
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(#{init_script := InitScript} = Ctx) ->
    service:start(InitScript, Ctx).


%%--------------------------------------------------------------------
%% @doc @see service:stop/1
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(#{init_script := InitScript}) ->
    service:stop(InitScript).


%%--------------------------------------------------------------------
%% @doc @see service:status/1
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> ok | no_return().
status(#{init_script := InitScript}) ->
    service:status(InitScript).