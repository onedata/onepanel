%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This behaviour provides a common models API.
%%% @end
%%%--------------------------------------------------------------------
-module(model_behaviour).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").

-type key() :: term().
-type diff() :: fun((record()) -> record()) | #{key() => term()}.
-type record() :: #onepanel_user{} | #onepanel_session{}
    | #onepanel_deployment{} | #onepanel_kv{} | #service{}.

%% Implicit version of old-style model records not wrapped in #document{}: 0
%% Version of the above wrapped in #document{}: 1
%% Any higher versions are caused by changes introduced to the model schema
-type version() :: non_neg_integer().

-export_type([key/0, version/0, diff/0, record/0]).

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Returns model attributes.
%%--------------------------------------------------------------------
-callback get_fields() -> list(atom()).


%%--------------------------------------------------------------------
%% Provides custom model initialization.
%%--------------------------------------------------------------------
-callback seed() -> any().


%%--------------------------------------------------------------------
%% Creates the model instance. Returns an error if the instance already exists.
%%--------------------------------------------------------------------
-callback create(Record :: record()) ->
    {ok, model_behaviour:key()} | {error, _} | no_return().


%%--------------------------------------------------------------------
%% Saves the model instance.
%%--------------------------------------------------------------------
-callback save(Record :: record()) -> ok | no_return().


%%--------------------------------------------------------------------
%% Updates the model instance.
%%--------------------------------------------------------------------
-callback update(Key :: key(), Diff :: diff()) -> ok | no_return().


%%--------------------------------------------------------------------
%% Returns a model instance.
%%--------------------------------------------------------------------
-callback get(Key :: key()) ->
    {ok, Record :: record()} | {error, _} | no_return().


%%--------------------------------------------------------------------
%% Returns 'true' if the model instance exists, otherwise 'false'.
%%--------------------------------------------------------------------
-callback exists(Key :: key()) -> boolean() | no_return().


%%--------------------------------------------------------------------
%% Deletes the model instance.
%%--------------------------------------------------------------------
-callback delete(Key :: key()) -> ok | no_return().


%%--------------------------------------------------------------------
%% Returns a list of all the model instances.
%%--------------------------------------------------------------------
-callback list() -> Records :: [model_behaviour:record()] | no_return().


%%--------------------------------------------------------------------
%% Upgrades record
%%--------------------------------------------------------------------
-callback upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    {NewVsn :: model_behaviour:version(), NewRecord :: tuple()}.