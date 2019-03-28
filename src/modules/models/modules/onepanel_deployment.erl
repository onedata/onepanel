%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains functions for managing singleton model
%%% storing completed configuration steps.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_deployment).
-author("Wojciech Geisler").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").
-include("deployment_progress.hrl").
-include("names.hrl").
-include("service.hrl").

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, upgrade/2, create/1,
    save/1, update/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([set_marker/1, unset_marker/1, is_set/1, get_all/0]).

-type record() :: #onepanel_deployment{}.
-type marker() :: atom().

-export_type([marker/0]).

-define(ID, ?MODULE).

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
    2.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    create(#onepanel_deployment{id = ?ID, completed = gb_sets:new()}).


-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    {NewVsn :: model_behaviour:version(), NewRecord :: record()}.
upgrade(1, Record) ->
    {onepanel_deployment, Id, Completed} = Record,
    % Introduce tracking of storage setup in the deployment marks.
    % Assume some storage to have been created in an existing cluster.
    {2, {onepanel_deployment, Id,
        case onepanel_env:get_cluster_type() of
            onezone -> Completed;
            oneprovider -> gb_sets:add_element(?PROGRESS_STORAGE_SETUP, Completed)
        end}}.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, model_behaviour:key()} | #error{} | no_return().
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
    {ok, Record :: record()} | #error{} | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    model:exists(?MODULE, Key).


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


%%--------------------------------------------------------------------
%% @doc Marks interactive configuration step as completed.
%% @end
%%--------------------------------------------------------------------
-spec set_marker(ProgressMarks :: marker() | [marker()]) -> ok.
set_marker([]) -> ok;
set_marker([_|_] = ProgressMarks) ->
    ?MODULE:update(?ID, fun(#onepanel_deployment{completed = C} = Record) ->
        Record#onepanel_deployment{
            completed = lists:foldl(fun gb_sets:add_element/2, C, ProgressMarks)
        }
    end);
set_marker(ProgressMark) -> set_marker([ProgressMark]).


%%--------------------------------------------------------------------
%% @doc Marks configuration step(s) as uncompleted or waiting for user decision.
%% @end
%%--------------------------------------------------------------------
-spec unset_marker(ProgressMarks :: marker() | [marker()]) -> ok.
unset_marker([]) -> ok;
unset_marker([_|_] = ProgressMarks) ->
    ?MODULE:update(?ID, fun(#onepanel_deployment{completed = C} = Record) ->
        Record#onepanel_deployment{
            completed = lists:foldl(fun gb_sets:del_element/2, C, ProgressMarks)
        }
    end);
unset_marker(ProgressMark) -> unset_marker([ProgressMark]).


%%--------------------------------------------------------------------
%% @doc Checks if given configuration step was completed by the user.
%% @end
%%--------------------------------------------------------------------
-spec is_set(ProgressMark :: marker()) -> boolean().
is_set(ProgressMarker) ->
    case ?MODULE:get(?ID) of
        {ok, #onepanel_deployment{completed = C}} ->
            gb_sets:is_member(ProgressMarker, C);
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @doc Returns unordered list of all completed steps.
%% @end
%%--------------------------------------------------------------------
-spec get_all() -> [marker()].
get_all() ->
    case ?MODULE:get(?ID) of
        {ok, #onepanel_deployment{completed = C}} ->
            gb_sets:to_list(C);
        _ -> []
    end.
