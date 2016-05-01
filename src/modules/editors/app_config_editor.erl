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
-module(app_config_editor).
-author("Krzysztof Trzepla").

%% API
-export([read/2, write/3, get/2, set/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec read(Keys :: [atom()], Path :: file:name()) ->
    {ok, Value :: term()} | {error, Reason :: term()}.
read(Keys, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} -> get(Keys, AppConfigs);
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: [atom()], Value :: term(), Path :: file:name_all()) ->
    ok | {error, Reason :: term()}.
write(Keys, Value, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} ->
            NewAppConfigs = set(Keys, Value, AppConfigs),
            file:write_file(Path, [io_lib:fwrite("~p", [NewAppConfigs]), $.]);
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: [atom()], Terms :: proplists:proplist()) ->
    {ok, Value :: term()} | {error, Reason :: term()}.
get([], Terms) ->
    {ok, Terms};
get([Key | Keys], Terms) ->
    case lists:keyfind(Key, 1, Terms) of
        {Key, NewTerms} -> get(Keys, NewTerms);
        false -> {error, not_found}
    end;
get(Key, Terms) ->
    get([Key], Terms).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: [atom()], Value :: term(), Terms :: proplists:proplist()) ->
    NewTerms :: proplists:proplist().
set([], Value, _Terms) ->
    Value;
set([Key | Keys], Value, Terms) ->
    NewValue = case get(Key, Terms) of
        {ok, NewTerms} -> set(Keys, Value, NewTerms);
        {error, not_found} -> set(Keys, Value, [])
    end,
    lists:keystore(Key, 1, Terms, {Key, NewValue}).

%%%===================================================================
%%% Internal functions
%%%===================================================================