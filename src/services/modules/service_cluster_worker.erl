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

-include("modules/errors.hrl").
-include("modules/logger.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Service behaviour callbacks
-export([name/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1,
    nagios_report/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:name/0
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    cluster_worker.


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{name := Name}) ->
    [
        #step{module = service, function = save,
            args = [#service{name = Name}], selection = first},
        #step{function = configure},
        #step{function = start}
    ];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}];

get_steps(nagios_report, _Ctx) ->
    [#step{function = nagios_report}];

get_steps(Action, _Ctx) ->
    ?throw({action_not_supported, Action}).

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

    Host = onepanel_cluster:node_to_host(),
    Node = onepanel_cluster:host_to_node(Name, Host),

    onepanel_env:write([Name, cm_nodes], CmNodes, AppConfigPath),
    onepanel_env:write([Name, db_nodes], DbNodes, AppConfigPath),

    maps:fold(fun(Key, Value, _) ->
        onepanel_env:write([Name, Key], Value, AppConfigPath)
    end, #{}, AppConfig),

    onepanel_vm:write(<<"name">>, erlang:atom_to_binary(Node, utf8),
        VmArgsPath),
    onepanel_vm:write(<<"setcookie">>,
        erlang:atom_to_binary(maps:get(cookie, Ctx, erlang:get_cookie()), utf8),
        VmArgsPath),

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


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(#{name := Name, wait_for_init_attempts := Attempts,
    wait_for_init_delay := Delay} = Ctx) ->
    Module = service:get_module(Name),
    onepanel_utils:wait_until(Module, nagios_report, [Ctx], {equal, ok},
        Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec nagios_report(Ctx :: service:ctx()) -> Status :: atom().
nagios_report(#{nagios_protocol := Protocol, nagios_port := Port}) ->
    Host = onepanel_cluster:node_to_host(),
    Url = onepanel_utils:join([Protocol, "://", Host, ":", Port, "/nagios"]),

    {ok, 200, _Headers, Body} = http_client:get(Url),

    {Xml, _} = xmerl_scan:string(binary_to_list(Body)),
    [Status] = [X#xmlAttribute.value || X <- Xml#xmlElement.attributes,
        X#xmlAttribute.name == status],

    list_to_atom(Status).