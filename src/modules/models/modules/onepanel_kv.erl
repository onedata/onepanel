%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module defines model for storing arbitrary key-value pairs,
%%% mostly global configuration.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_kv).
-author("Wojciech Geisler").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, upgrade/2, create/1,
    save/1, update/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([find/1, find/2]).
-export([set/2]).

-type record() :: #onepanel_kv{}.
-type key() :: term().
-type value() :: term().

-export_type([key/0, value/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_fields/0}
%% @end
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_record_version/0}
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> model_behaviour:version().
get_record_version() ->
    1.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    ok.


-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    no_return().
upgrade(1, _Record) ->
    error(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, model_behaviour:key()} | {error, _} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Record :: record()) -> ok | no_return().
save(Record) ->
    model:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | no_return().
update(Key, Diff) ->
    model:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: record()} | ?ERR_DOC_NOT_FOUND | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    % ensure table existence - relevant e.g. for clustering check in service_onepanel
    model:exists(?MODULE) andalso model:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:delete/1}
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | no_return().
delete(Key) ->
    model:delete(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:list/0}
%% @end
%%--------------------------------------------------------------------
-spec list() -> Records :: [model_behaviour:record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


-spec find(Key :: key()) -> {ok, value()} | ?ERR_DOC_NOT_FOUND.
find(Key) ->
    case ?MODULE:get(Key) of
        {ok, #onepanel_kv{value = Value}} -> {ok, Value};
        ?ERR_DOC_NOT_FOUND = Error -> Error
    end.

-spec find(Key :: key(), Default :: value()) -> value().
find(Key, Default) ->
    case ?MODULE:get(Key) of
        {ok, #onepanel_kv{value = Value}} -> Value;
        _ -> Default
    end.


-spec set(Key :: key(), Value :: value()) -> ok.
set(Key, Value) ->
    ok = save(#onepanel_kv{key = Key, value = Value}).
