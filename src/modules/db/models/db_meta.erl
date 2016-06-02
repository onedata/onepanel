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

%% API
-export([get_timestamp/0, set_timestamp/0]).

%% Model callbacks
-export([fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

%%%===================================================================
%%% Model callbacks
%%%===================================================================

fields() ->
    record_info(fields, ?MODULE).

create(Record) ->
    model_logic:create(?MODULE, Record).

save(Record) ->
    model_logic:save(?MODULE, Record).

update(Key, Diff) ->
    model_logic:update(?MODULE, Key, Diff).

get(Key) ->
    model_logic:get(?MODULE, Key).

exists(Key) ->
    model_logic:exists(?MODULE, Key).

delete(Key) ->
    model_logic:delete(?MODULE, Key).

%%%===================================================================
%%% API functions
%%%===================================================================

set_timestamp() ->
    db_meta:create(#db_meta{id = <<>>, created = erlang:system_time()}).

get_timestamp() ->
    {ok, #db_meta{created = Timestamp}} = db_meta:get(<<>>),
    Timestamp.

%%%===================================================================
%%% Internal functions
%%%===================================================================