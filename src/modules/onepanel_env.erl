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
-module(onepanel_env).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").

%% API
-export([read/2, write/3, get/1, get/2, find/1, find/2, set/2, set/3]).

-type key() :: atom().
-type keys() :: key() | [key()].
-type value() :: term().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys()) -> Value :: value().
get(Keys) ->
    get(Keys, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), AppName :: atom()) -> Value :: value().
get(Keys, AppName) ->
    {ok, Value} = find(Keys, AppName),
    Value.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec find(Keys :: keys()) -> {ok, Value :: value()} | #error{} | no_return().
find(Keys) ->
    find(Keys, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec find(Keys :: keys(), AppName :: atom()) ->
    {ok, Value :: value()} | #error{} | no_return().
find(Keys, AppName) when is_atom(AppName) ->
    select(Keys, application:get_all_env(AppName)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value()) -> ok.
set(Keys, Value) ->
    set(Keys, Value, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value(), AppName :: atom()) -> ok.
set(Keys, Value, AppName) ->
    lists:foreach(fun({K, V}) ->
        application:set_env(AppName, K, V)
    end, store(Keys, Value, application:get_all_env())).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec read(Keys :: keys(), Path :: file:name()) ->
    {ok, Value :: value()} | #error{} | no_return().
read(Keys, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} -> select(Keys, AppConfigs);
        {error, Reason} -> ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: keys(), Value :: value(), Path :: file:name_all()) ->
    ok | no_return().
write(Keys, Value, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} ->
            NewAppConfigs = store(Keys, Value, AppConfigs),
            case file:write_file(Path, io_lib:fwrite("~p.", [NewAppConfigs])) of
                ok -> ok;
                {error, Reason} -> ?throw(Reason)
            end;
        {error, Reason} ->
            ?throw(Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec select(Keys :: keys(), Terms :: proplists:proplist()) ->
    {ok, Value :: value()} | #error{}.
select([], Terms) ->
    {ok, Terms};

select([Key | Keys], Terms) ->
    case lists:keyfind(Key, 1, Terms) of
        {Key, NewTerms} -> select(Keys, NewTerms);
        false -> ?error(?ERR_NOT_FOUND)
    end;

select(Key, Terms) ->
    select([Key], Terms).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec store(Keys :: keys(), Value :: value(), Terms :: proplists:proplist()) ->
    NewTerms :: proplists:proplist().
store([], Value, _Terms) ->
    Value;

store([Key | Keys], Value, Terms) ->
    NewValue = case select(Key, Terms) of
        {ok, NewTerms} -> store(Keys, Value, NewTerms);
        #error{reason = ?ERR_NOT_FOUND} -> store(Keys, Value, [])
    end,
    lists:keystore(Key, 1, Terms, {Key, NewValue});

store(Key, Value, Terms) ->
    store([Key], Value, Terms).