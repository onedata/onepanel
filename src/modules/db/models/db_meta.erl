%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(db_meta).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("db/models.hrl").

%% Model behaviour callbacks
-export([fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

%% API
-export([get_timestamp/0, set_timestamp/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see model_behaviour:fields/0
%%--------------------------------------------------------------------
-spec fields() -> list(atom()).
fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:create/1
%%--------------------------------------------------------------------
-spec create(Record :: model_behaviour:record()) ->
    ok | {error, Reason :: term()}.
create(Record) ->
    model_logic:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:save/1
%%--------------------------------------------------------------------
-spec save(Record :: model_behaviour:record()) -> ok | {error, Reason :: term()}.
save(Record) ->
    model_logic:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:update/2
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | {error, Reason :: term()}.
update(Key, Diff) ->
    model_logic:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:get/1
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | {error, Reason :: term()}.
get(Key) ->
    model_logic:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:exists/1
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | {error, Reason :: term()}.
exists(Key) ->
    model_logic:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:delete/1
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | {error, Reason :: term()}.
delete(Key) ->
    model_logic:delete(?MODULE, Key).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set_timestamp() -> ok | {error, Reason :: term()}.
set_timestamp() ->
    db_meta:create(#db_meta{id = <<>>, created = erlang:system_time()}).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp() -> Timestamp :: integer().
get_timestamp() ->
    {ok, #db_meta{created = Timestamp}} = db_meta:get(<<>>),
    Timestamp.