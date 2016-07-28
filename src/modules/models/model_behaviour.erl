%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This behaviour provides a common models API.
%%%--------------------------------------------------------------------
-module(model_behaviour).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

-type key() :: term().
-type diff() :: fun((record()) -> record()) | #{}.
-type record() :: tuple().

-export_type([key/0, diff/0, record/0]).

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Returns model attributes.
%%--------------------------------------------------------------------
-callback get_fields() -> list(atom()).


%%--------------------------------------------------------------------
%% Creates model instance.
%%--------------------------------------------------------------------
-callback create(Record :: record()) -> ok | #error{} | no_return().


%%--------------------------------------------------------------------
%% Saves model instance.
%%--------------------------------------------------------------------
-callback save(Record :: record()) -> ok | no_return().


%%--------------------------------------------------------------------
%% Upates model instance.
%%--------------------------------------------------------------------
-callback update(Key :: key(), Diff :: diff()) -> ok | no_return().


%%--------------------------------------------------------------------
%% Returns model instance.
%%--------------------------------------------------------------------
-callback get(Key :: key()) ->
    {ok, Record :: record()} | #error{} | no_return().


%%--------------------------------------------------------------------
%% Returns 'true' if model instance exists, otherwise 'false'.
%%--------------------------------------------------------------------
-callback exists(Key :: key()) -> boolean() | no_return().


%%--------------------------------------------------------------------
%% Deletes model instance.
%%--------------------------------------------------------------------
-callback delete(Key :: key()) -> ok | no_return().
