%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains Ceph Manager Daemon management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ceph_mgr).
-author("Wojciech Geisler").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-type id() :: binary().
-type details() :: #{atom() => term()}.

-export_type([id/0]).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API functions
-export([exists/1]).

%% Step functions
-export([start/1, wait_for_init/1, status/1, stop/1, register_host/1, list/0,
    list_running/0, get_details/1, create_keyring/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

-spec name() -> Name :: service:name().
name() ->
    ?SERVICE_CEPH_MGR.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    ceph:extract_hosts(list_instances()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> no_return().
get_nodes() ->
    error(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy_all, #{managers := Managers}) ->
    WithIds = ceph:ensure_ids_from_hosts(Managers),
    Filtered = ceph:filter_out_existing_instances(name(), WithIds),
    service_utils:for_each_ctx(Filtered, [
        #steps{action = deploy}
    ]);

get_steps(deploy, #{host := Host}) ->
    [
        #step{function = create_keyring, hosts = [Host]},
        #step{function = start, hosts = [Host]},
        #step{function = wait_for_init, hosts = [Host]},
        #step{function = register_host, hosts = [Host]}
    ];

get_steps(resume_all, _Ctx) ->
    Instances = list_instances(),
    service_utils:for_each_ctx(Instances,
        [#steps{action = resume}]);

get_steps(resume, #{id := _, hosts := [_]}) ->
    [
        #step{function = start},
        #step{function = wait_for_init}
    ];

get_steps(stop_all, _Cxt) ->
    service_utils:for_each_ctx(list_instances(), [
        #steps{action = stop}
    ]);

get_steps(stop, #{id := Id}) ->
    #{host := Host} = get_instance(Id),
    [#step{function = stop, selection = any, hosts = [Host]}];

get_steps(Action, _Ctx) when
    Action == get_details ->
    [#step{function = Action, selection = any}];

get_steps(NoArgsAction, _Ctx) when
    NoArgsAction == list ->
    [#step{function = NoArgsAction, args = [], selection = any}].


%%%===================================================================
%%% API functions
%%%===================================================================

-spec exists(id()) -> boolean().
exists(Id) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := _}} -> true;
        _ -> false
    end.


%%%===================================================================
%%% Step functions
%%%===================================================================

-spec register_host(#{id := id()}) -> ok.
register_host(#{id := Id}) ->
    Host = hosts:self(),
    service:store_in_ctx(name(), [instances, Id],
        #{id => Id, host => Host, deployment_finished => true}),
    service:add_host(name(), Host).


-spec start(#{id := id()}) -> ok.
start(#{id := Id} = Ctx) ->
    ceph_cli:mgr_start(Id),
    service:register_healthcheck(name(), Ctx),
    ?info("Service ceph_mgr (id ~p) started", [Id]).


-spec wait_for_init(#{id := id()}) -> ok | no_return().
wait_for_init(#{id := Id}) ->
    StartAttempts = onepanel_env:get(ceph_mgr_wait_for_init_attempts),
    Validator = fun(Running) -> true = lists:member(Id, Running) end,
    onepanel_utils:wait_until(?MODULE, list_running, [],
        {validator, Validator}, StartAttempts),
    ok.


-spec stop(#{id := id()}) -> ok.
stop(#{id := Id}) ->
    StartedBy = ceph_cli:mgr_start_cmd(Id),
    ceph_cli:stop_with_timeout(StartedBy).


%%--------------------------------------------------------------------
%% @doc Status used by service healthcheck.
%% @end
%%--------------------------------------------------------------------
-spec status(#{id := id()}) -> service:status().
status(#{id := Id}) ->
    StartedBy = ceph_cli:mgr_start_cmd(Id),
    case onepanel_shell:process_exists(StartedBy) of
        true -> healthy;
        false -> stopped
    end.


-spec get_details(#{id := id()}) -> details().
get_details(#{id := Id}) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := Details}} -> instance_to_details(Details);
        _ -> throw(?ERROR_NOT_FOUND)
    end.


-spec list() -> #{managers => [details()]}.
list() ->
    #{managers => lists:map(fun instance_to_details/1, list_instances())}.


-spec list_running() -> [id()].
list_running() ->
    ClusterStatus = ceph_cli:status(),
    ActiveId = kv_utils:get([<<"mgrmap">>, <<"active_name">>], ClusterStatus),
    Standbys = kv_utils:get([<<"mgrmap">>, <<"standbys">>], ClusterStatus),
    StanbdyIds = [Id || #{<<"name">> := Id} <- Standbys],
    lists:usort([ActiveId | StanbdyIds]).


-spec create_keyring(#{id := id()}) -> ok.
create_keyring(#{id := Id}) ->
    DataDir = ceph:get_data_dir(mgr, Id),
    KeyringPath = filename:join(DataDir, "keyring"),
    KeyringContent = ceph_cli:auth_get_or_create(<<"mgr.", Id/binary>>, [
        {mon, <<"allow profile mgr">>},
        {osd, <<"allow *">>},
        {mds, <<"allow *">>}
    ]),
    % The keyring content MUST HAVE newline at the end, which is stripped by onepanel_shell
    ValidKeyring = str_utils:ensure_suffix(KeyringContent, "\n"),

    ok = filelib:ensure_dir(KeyringPath),
    ok = file:write_file(KeyringPath, ValidKeyring).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec list_instances() -> [ceph:instance()].
list_instances() ->
    case service:get_ctx(name()) of
        #{instances := Instances} -> maps:values(Instances);
        _ -> []
    end.


%% @private
-spec get_instance(id()) -> ceph:instance().
get_instance(Id) ->
    kv_utils:get([instances, Id], service:get_ctx(name())).


-spec instance_to_details(ceph:instance()) -> #{atom() => binary()}.
instance_to_details(Instance) ->
    onepanel_utils:convert(maps:with([id, host], Instance), {values, binary}).
