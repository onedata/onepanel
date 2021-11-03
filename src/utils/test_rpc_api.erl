%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module exposing onepanel functions that are used in tests.
%%% @end
%%%-------------------------------------------------------------------
-module(test_rpc_api).
-author("Piotr Duleba").

-include_lib("ctool/include/errors.hrl").

%% API
-export([
    get_op_worker_nodes/0,
    get_cert_chain_ders/0,
    get_cluster_id/0,
    create_invite_token/0,
    is_emergency_passphrase_set/0,
    set_emergency_passphrase/1,
    unset_emergency_passphrase/0,
    verify_emergency_passphrase/1,
    list_onepanel_deployment/0,
    is_onepanel_initialized/0,
    service_apply_sync/3,
    service_apply/4,
    service_get_ctx/1,
    get_result_from_service_action/2,
    get_result_from_service_action/3,
    list_ceph_pool/0,
    ensure_loopdevice/2,
    lvm_create_physical_volume/1,
    lvm_create_logical_volume/2,
    lvm_create_volume_group/2,
    ceph_mon_list_quorum/0,
    ceph_mgr_list_running/0,
    ceph_cli_osd_df/0
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_op_worker_nodes() -> [node()].
get_op_worker_nodes() ->
    service_op_worker:get_nodes().


-spec get_cert_chain_ders() -> [public_key:der_encoded()].
get_cert_chain_ders() ->
    https_listener:get_cert_chain_ders().


-spec get_cluster_id() -> binary().
get_cluster_id() ->
    clusters:get_id().


-spec create_invite_token() -> {ok, binary()} | {error, term()}.
create_invite_token() ->
    invite_tokens:create().


-spec is_emergency_passphrase_set() -> boolean().
is_emergency_passphrase_set() ->
    emergency_passphrase:is_set().


-spec set_emergency_passphrase(binary()) -> ok | ?ERROR_BAD_VALUE_PASSWORD.
set_emergency_passphrase(NewPassphrase) ->
    emergency_passphrase:set(NewPassphrase).


-spec unset_emergency_passphrase() -> ok | no_return().
unset_emergency_passphrase() ->
    emergency_passphrase:unset().


-spec verify_emergency_passphrase(binary()) -> boolean().
verify_emergency_passphrase(Passphrase) ->
    emergency_passphrase:verify(Passphrase).


-spec list_onepanel_deployment() -> list().
list_onepanel_deployment() ->
    onepanel_deployment:list().


-spec is_onepanel_initialized() -> boolean().
is_onepanel_initialized() ->
    case https_listener:healthcheck() of
        ok -> true;
        _ -> false
    end.


-spec service_apply_sync(service:name(), service:action(), service:step_ctx()) ->
    service_executor:results() | errors:error().
service_apply_sync(Service, Action, Ctx) ->
    service:apply_sync(Service, Action, Ctx).


-spec service_apply(service:name(), service:action(), service:step_ctx(), pid()) ->
    service_executor:results() | errors:error().
service_apply(Service, Action, Ctx, Self) ->
    service:apply(Service, Action, Ctx, Self).



-spec service_get_ctx(service:name()) -> term().
service_get_ctx(Service) ->
    service:get_ctx(Service).


-spec get_result_from_service_action(service:name(), service:action()) -> term() | errors:error().
get_result_from_service_action(Service, Action) ->
    middleware_utils:result_from_service_action(Service, Action).


-spec get_result_from_service_action(service:name(), service:action(), service:step_ctx()) -> term() | errors:error().
get_result_from_service_action(Service, Action, Ctx) ->
    middleware_utils:result_from_service_action(Service, Action, Ctx).


-spec list_ceph_pool() -> [binary()].
list_ceph_pool() ->
    ceph_pool:list().


-spec ensure_loopdevice(binary(), integer()) -> binary().
ensure_loopdevice(Path, Size) ->
    loopdevice:ensure_loopdevice(Path, Size).


-spec lvm_create_physical_volume(binary()) -> ok.
lvm_create_physical_volume(Device) ->
    lvm:create_physical_volume(Device).

-spec lvm_create_logical_volume(binary(), binary()) -> ok.
lvm_create_logical_volume(VolumeName, GroupName) ->
    lvm:create_logical_volume(VolumeName, GroupName).


-spec lvm_create_volume_group(binary(), [binary()]) -> ok.
lvm_create_volume_group(GroupName, Devices) ->
    lvm:create_volume_group(GroupName, Devices).


-spec ceph_mon_list_quorum()-> [binary()].
ceph_mon_list_quorum()->
    service_ceph_mon:list_quorum().


-spec ceph_mgr_list_running()-> [binary()].
ceph_mgr_list_running()->
    service_ceph_mgr:list_running().


-spec ceph_cli_osd_df() -> map().
ceph_cli_osd_df() ->
    ceph_cli:osd_df().