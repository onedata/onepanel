%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains functions for managing singleton
%%% storing completed configuration steps.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_milestones).
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
-export([mark_configured/1, mark_not_configured/1, is_configured/1, get/0]).

-type record() :: #onepanel_milestones{}.
-type milestone() :: atom().

-export_type([milestone/0]).

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
    create(#onepanel_milestones{key = ?KEY, configured = gb_sets:new()}).


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
-spec mark_configured(Milestone :: milestone()) -> ok.
mark_configured(Milestone) ->
    ?MODULE:update(?KEY, fun(#onepanel_milestones{configured = C} = OM) ->
        OM#onepanel_milestones{configured = gb_sets:add_element(Milestone, C)}
    end).


%%--------------------------------------------------------------------
%% @doc Marks configuration step as waiting for user decision.
%% @end
%%--------------------------------------------------------------------
-spec mark_not_configured(Milestone :: milestone()) -> ok.
mark_not_configured(Milestone) ->
    ?MODULE:update(?KEY, fun(#onepanel_milestones{configured = C} = OM) ->
        OM#onepanel_milestones{configured = gb_sets:del_element(Milestone, C)}
    end).


%%--------------------------------------------------------------------
%% @doc Checks if given configuration step was completed by the user.
%% @end
%%--------------------------------------------------------------------
-spec is_configured(Milestone :: milestone()) -> boolean().
is_configured(Milestone) ->
    case ?MODULE:get(?KEY) of
        {ok, #onepanel_milestones{configured = C}} ->
            gb_sets:is_member(Milestone, C);
        _ -> false
    end.

-spec get() -> [milestone()].
get() ->
    case ?MODULE:get(?KEY) of
        {ok, #onepanel_milestones{configured = C}} ->
            gb_sets:to_list(C);
        _ -> []
    end.

