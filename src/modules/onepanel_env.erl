%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for a application configuration
%%% file (app.config) management. It also allows for modification of application
%%% variables loaded into application memory.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_env).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").

%% API
-export([get/1, get/2, find/1, find/2, set/2, set/3, set/4]).
-export([read/2, write/2, write/3, write/4]).

-type key() :: atom().
-type keys() :: key() | [key()].
-type value() :: term().

-export_type([key/0, keys/0, value/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv get(Keys, ?APP_NAME)
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys()) -> Value :: value().
get(Keys) ->
    get(Keys, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from application's memory.
%% Throws an exception when value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), AppName :: atom()) -> Value :: value().
get(Keys, AppName) ->
    {ok, Value} = find(Keys, AppName),
    Value.


%%--------------------------------------------------------------------
%% @doc @equiv find(Keys, ?APP_NAME)
%% @end
%%--------------------------------------------------------------------
-spec find(Keys :: keys()) -> {ok, Value :: value()} | #error{} | no_return().
find(Keys) ->
    find(Keys, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from application's memory.
%% Returns error if value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec find(Keys :: keys(), AppName :: atom()) ->
    {ok, Value :: value()} | #error{} | no_return().
find(Keys, AppName) when is_atom(AppName) ->
    onepanel_lists:get(Keys, application:get_all_env(AppName)).


%%--------------------------------------------------------------------
%% @doc @equiv set(Keys, Value, ?APP_NAME)
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value()) -> ok.
set(Keys, Value) ->
    set(Keys, Value, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's memory.
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value(), AppName :: atom()) -> ok.
set(Keys, Value, AppName) ->
    lists:foreach(fun({K, V}) ->
        application:set_env(AppName, K, V)
    end, onepanel_lists:store(Keys, Value, application:get_all_env())).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's memory on given
%% nodes.
%% @end
%%--------------------------------------------------------------------
-spec set(Nodes :: [node()], Keys :: keys(), Value :: value(), AppName :: atom()) ->
    Results :: onepanel_rpc:results() | no_return().
set(Nodes, Keys, Value, AppName) ->
    onepanel_rpc:call_all(Nodes, ?MODULE, set, [Keys, Value, AppName]).


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from application's configuration
%% file. Returns error if value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec read(Keys :: keys(), Path :: file:name()) ->
    {ok, Value :: value()} | #error{} | no_return().
read(Keys, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} -> onepanel_lists:get(Keys, AppConfigs);
        {error, Reason} -> ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc @equiv write(Keys, Value, onepanel_env:get(app_config_path))
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: keys(), Value :: value()) -> ok | no_return().
write(Keys, Value) ->
    write(Keys, Value, onepanel_env:get(app_config_path)).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's configuration file.
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: keys(), Value :: value(), Path :: file:name_all()) ->
    ok | no_return().
write(Keys, Value, Path) ->
    case file:consult(Path) of
        {ok, [AppConfigs]} ->
            NewAppConfigs = onepanel_lists:store(Keys, Value, AppConfigs),
            case file:write_file(Path, io_lib:fwrite("~p.", [NewAppConfigs])) of
                ok -> ok;
                {error, Reason} -> ?throw_error(Reason)
            end;
        {error, Reason} ->
            ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's configuration file
%% on given nodes.
%% @end
%%--------------------------------------------------------------------
-spec write(Nodes :: [node()], Keys :: keys(), Value :: value(),
    Path :: file:name_all()) -> Results :: onepanel_rpc:results() | no_return().
write(Nodes, Keys, Value, Path) ->
    onepanel_rpc:call_all(Nodes, ?MODULE, write, [Keys, Value, Path]).