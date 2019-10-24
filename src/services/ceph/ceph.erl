%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Convenience functions for accessing facts about the Ceph cluster,
%%% common utils and types for service_ceph_* modules.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph).
-author("Wojciech Geisler").

-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

% @formatter:off
-type id() :: binary().
-type daemon() :: mon | mgr | osd.
-type daemon_service() :: ?SERVICE_CEPH_MON | ?SERVICE_CEPH_MGR | ?SERVICE_CEPH_OSD.
-type uuid() :: <<_:288>>. % 32 hex characters + 4 hyphens
-type status_level() :: ok | warning | error.
-type instance() :: #{
    id := id(),
    host := service:host(),
    % only instances marked as deployed will be filtered out
    % when there is another request to deploy given id
    deployment_finished := boolean(),
    atom() => term()
}.
% @formatter:on

-export_type([id/0, daemon/0, uuid/0, status_level/0, instance/0]).

-export([get_cluster_name/0]).
-export([get_client_name/1, get_pool_user/0, get_admin_keyring_path/0, get_cluster_uuid/0]).
-export([gen_uuid/0, get_data_dir/2, get_conf_path/0]).
-export([ensure_ids_from_hosts/1, extract_hosts/1, id_to_host/2,
    filter_out_existing_instances/2, filter_out_existing_instances/3]).

%%%===================================================================
%%% Getters
%%%===================================================================

-spec get_cluster_name() -> binary().
get_cluster_name() ->
    maps:get(cluster_name, service:get_ctx(?SERVICE_CEPH)).


-spec get_cluster_uuid() -> ceph:uuid().
get_cluster_uuid() ->
    maps:get(fsid, service:get_ctx(?SERVICE_CEPH)).


%%--------------------------------------------------------------------
%% @doc Path to the ceph.conf file.
%% @end
%%--------------------------------------------------------------------
-spec get_conf_path() -> binary().
get_conf_path() ->
    <<"/etc/ceph/", (get_cluster_name())/binary, ".conf">>.


%%--------------------------------------------------------------------
%% @doc Returns default data directory for given Ceph service.
%% @end
%%--------------------------------------------------------------------
-spec get_data_dir(daemon(), Name :: binary()) -> Path :: binary().
get_data_dir(Service, Name) ->
    ClusterName = ceph:get_cluster_name(),
    onepanel_utils:join(["/var/lib/ceph/", Service, "/", ClusterName, "-", Name, "/"]).


%%--------------------------------------------------------------------
%% @doc Creates a full client name from a username.
%% @end
%%--------------------------------------------------------------------
-spec get_client_name(User :: binary()) -> binary().
get_client_name(User) ->
    <<"client.", User/binary>>.


%%--------------------------------------------------------------------
%% @doc Returns key of a user with pool rw capabilities.
%% @end
%%--------------------------------------------------------------------
-spec get_pool_user() -> {Name :: binary(), Key :: binary()}.
get_pool_user() ->
    Name  = get_client_name(onepanel_env:typed_get(ceph_pool_user_username, binary)),
    Capabilities = [{mon, <<"allow r">>}, {osd, <<"allow rw">>}],
    Key = ceph_cli:auth_get_or_create_key(Name, Capabilities),
    {Name, Key}.


-spec get_admin_keyring_path() -> binary().
get_admin_keyring_path() ->
    Client = get_client_name(onepanel_env:typed_get(ceph_admin_username, binary)),
    <<"/etc/ceph/", (get_cluster_name())/binary, ".", Client/binary, ".keyring">>.


%%%===================================================================
%%% Utils
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generates random UUID, in the format
%% xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
%% @end
%%--------------------------------------------------------------------
-spec gen_uuid() -> uuid().
gen_uuid() ->
    Parts = [str_utils:rand_hex(Chars div 2) || Chars <- [8, 4, 4, 4, 12]],
    onepanel_utils:join(Parts, <<"-">>).


%%--------------------------------------------------------------------
%% @doc Ensures each service specification contains an id.
%% Missing ids are created based on 'host' field.
%% @end
%%--------------------------------------------------------------------
-spec ensure_ids_from_hosts([#{host := Host, id => binary(), K => V}]) ->
    [#{host := Host, id := binary(), K => V}].
ensure_ids_from_hosts(Ctxs) ->
    lists:map(fun
        (#{id := <<_/binary>>} = Ctx) -> Ctx;
        (#{host := Host} = Ctx) -> Ctx#{id => list_to_binary(Host)}
    end, Ctxs).


%%--------------------------------------------------------------------
%% @doc Returns host on which instance with given Id is deployed.
%% Accepts an ceph:instance() map as well.
%% @end
%%--------------------------------------------------------------------
-spec id_to_host(service:name(), ceph:id() | #{id := id()} | #{host := service:host()}) ->
    service:host() | no_return().
id_to_host(_ServiceName, #{host := Host}) ->
    Host;

id_to_host(ServiceName, #{id := Id}) ->
    id_to_host(ServiceName, Id);

id_to_host(ServiceName, Id) when is_binary(Id) ->
    #{instances := #{Id := #{host := Host}}} = service:get_ctx(ServiceName),
    Host.


-spec extract_hosts([#{host => service:host()}]) -> [service:host()].
extract_hosts(Ctxs) ->
    lists:usort([Host || #{host := Host} <- Ctxs]).


%%--------------------------------------------------------------------
%% @doc @equiv filter_out_existing_instances(Service, Ctxs, id)
%% @end
%%--------------------------------------------------------------------
-spec filter_out_existing_instances(daemon_service(), [Ctx]) -> [Ctx] when
    Ctx :: #{id := id(), _ => _}.
filter_out_existing_instances(Service, Ctxs) ->
    filter_out_existing_instances(Service, Ctxs, id).


%%--------------------------------------------------------------------
%% @doc Removes from a list of instance specifications those
%% with an identifier already used by successfully deployed daemons.
%% The identifier is extracted from the Ctx by given key.
%% Ctxs missing the key are always unique.
%% @end
%%--------------------------------------------------------------------
-spec filter_out_existing_instances(daemon_service(), [Ctx], UniqueKey) -> [Ctx] when
    Ctx :: #{UniqueKey := term(), _ => _}.
filter_out_existing_instances(Service, Ctxs, UniqueKey) ->
    Instances = maps:get(instances, service:get_ctx(Service), #{}),
    Ids = [Id || #{UniqueKey := Id, deployment_finished := true} <- maps:values(Instances)],
    lists:filter(fun(Ctx) ->
        case maps:find(UniqueKey, Ctx) of
            {ok, Id} -> not lists:member(Id, Ids);
            error -> true
        end
    end, Ctxs).
