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
    add_storage/1
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


-spec add_storage(map()) ->  {error, _} | {ok, binary()}.
add_storage(Data) ->
    [{Name, Params}] = maps:to_list(onepanel_parser:parse(Data, rest_model:storage_create_request_model())),
    {Name, Result} = op_worker_storage:add(#{name => Name, params => Params}),
    Result.
