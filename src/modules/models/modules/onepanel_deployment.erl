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
-include("names.hrl").
-include("service.hrl").

%% Model behaviour callbacks
-export([get_fields/0, seed/0, create/1, save/1, update/2, get/1, exists/1,
    delete/1, list/0]).

%% API
-export([mark_completed/1, mark_not_completed/1, is_completed/1]).

-type record() :: #onepanel_deployment{}.
-type mark() :: atom().

-export_type([mark/0]).

-define(KEY, ?MODULE).

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
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    create(#onepanel_deployment{key = ?KEY, completed = gb_sets:new()}).


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
-spec mark_completed(ProgressMark :: mark()) -> ok.
mark_completed(ProgressMark) ->
    ?MODULE:update(?KEY, fun(#onepanel_deployment{completed = C} = OP) ->
        OP#onepanel_deployment{completed = gb_sets:add_element(ProgressMark, C)}
    end).


%%--------------------------------------------------------------------
%% @doc Marks configuration step as uncompleted or waiting for user decision.
%% @end
%%--------------------------------------------------------------------
-spec mark_not_completed(ProgressMark :: mark()) -> ok.
mark_not_completed(ProgressMark) ->
    ?MODULE:update(?KEY, fun(#onepanel_deployment{completed = C} = OP) ->
        OP#onepanel_deployment{completed = gb_sets:del_element(ProgressMark, C)}
    end).


%%--------------------------------------------------------------------
%% @doc Checks if given configuration step was completed by the user.
%% @end
%%--------------------------------------------------------------------
-spec is_completed(ProgressMark :: mark()) -> boolean().
is_completed(ProgressMark) ->
    case ?MODULE:get(?KEY) of
        {ok, #onepanel_deployment{completed = C}} ->
            gb_sets:is_member(ProgressMark, C);
        _ -> false
    end.
