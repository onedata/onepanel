%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_storage resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_rest_translator).
-author("Wojciech Geisler").

-include("middleware/middleware.hrl").
-include("http/rest.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/headers.hrl").

-export([create_response/3, get_response/2, update_response/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_response(gri:gri(), value, Resource :: term()) -> #rest_resp{}.
create_response(#gri{aspect = instances}, value, StoragesMap) ->
    case adding_storages_caused_error(StoragesMap) of
        true ->
            #rest_resp{
                code = ?HTTP_400_BAD_REQUEST,
                headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                body = StoragesMap
            };
        false ->
            ?OK_REPLY(StoragesMap)
    end.

-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = list}, StorageIds) ->
    ?OK_REPLY(#{<<"ids">> => StorageIds});

get_response(#gri{aspect = instance}, StorageDetails) ->
    TypeAdjusted = format_storage_details(StorageDetails),
    ?OK_REPLY(onepanel_utils:convert(TypeAdjusted, {keys, binary}));

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).


-spec update_response(gri:gri(), Result :: term()) -> #rest_resp{}.
update_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc All storage helper parameters are stored as binaries in op_worker.
%% This function attempts to find their desired type in API model
%% used for creating them and convert accordingly.
%% @end
%%--------------------------------------------------------------------
-spec format_storage_details(#{Key => term()}) ->
    #{Key => term()}.
format_storage_details(StorageDetails) ->
    StorageType = onepanel_utils:get_converted(type, StorageDetails, atom),
    Model = get_storage_model(StorageType),
    Typemap = model_to_typemap(Model),
    maps:map(fun
        (Key, Value) when is_binary(Value) ->
            KeyAtom = onepanel_utils:convert(Key, atom),
            case maps:find(KeyAtom, Typemap) of
                {ok, string} -> Value;
                {ok, Type} -> onepanel_utils:convert(Value, Type);
                error -> Value
            end;
        (_Key, Value) -> Value
    end, StorageDetails).


%%--------------------------------------------------------------------
%% @private
%% @doc Simplifies API model by stripping optionality modifiers
%% and filtering out more complex specs such as "anyOf".
%% Transforms only top level fields.
%% @end
%%--------------------------------------------------------------------
-spec model_to_typemap(Model :: #{atom() := atom() | tuple()}) ->
    #{Key :: atom() := Type :: atom()}.
model_to_typemap(Model) ->
    maps:fold(fun
        (Key, Type, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, optional}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, required}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, {optional, _}}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (_Key, _ValueSpec, Acc) -> Acc
    end, #{}, Model).


%%--------------------------------------------------------------------
%% @private
%% @doc Finds REST model used for creating storage of given type.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_model(StorageType :: atom()) -> onepanel_parser:object_spec().
get_storage_model(posix) -> rest_model:posix_model();
get_storage_model(s3) -> rest_model:s3_model();
get_storage_model(ceph) -> rest_model:ceph_model();
get_storage_model(cephrados) -> rest_model:cephrados_model();
get_storage_model(localceph) -> rest_model:localceph_model();
get_storage_model(swift) -> rest_model:swift_model();
get_storage_model(glusterfs) -> rest_model:glusterfs_model();
get_storage_model(nulldevice) -> rest_model:nulldevice_model();
get_storage_model(webdav) -> rest_model:webdav_model();
get_storage_model(http) -> rest_model:http_model();
get_storage_model(xrootd) -> rest_model:xrootd_model().


%%--------------------------------------------------------------------
%% @private
%% @doc Finds, if any error occurred during adding storages, based on response storages map.
%% @end
%%--------------------------------------------------------------------
-spec adding_storages_caused_error(map()) -> boolean().
adding_storages_caused_error(StoragesMap) ->
    lists:foldl(fun(StorageStatus, ErrorOccurredAcc) ->
        case maps:is_key(<<"error">>, StorageStatus) of
            true -> true;
            false -> ErrorOccurredAcc
        end
    end, false, maps:values(StoragesMap)).
