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
-include_lib("ctool/include/logging.hrl").

%% API
-export([get/1, get/2, get/3, find/1, find/2, set/2, set/3, set/4]).
-export([typed_get/2, typed_get/3]).
-export([read/2, read_effective/2, read_effective/3]).
-export([write/2, write/3, write/4]).
-export([get_remote/3, find_remote/3, set_remote/4]).
-export([migrate_generated_config/2, migrate_generated_config/3,
    legacy_config_exists/1, get_config_path/2]).
-export([migrate/3, migrate/4]).
-export([get_cluster_type/0]).

-type key() :: atom().
-type keys() :: key() | [key()].
-type value() :: term().

-export_type([key/0, keys/0, value/0]).

-define(DO_NOT_MODIFY_HEADER,
    "% MACHINE GENERATED FILE. DO NOT MODIFY.\n"
    "% Use overlay.config for custom configuration.\n").
-define(FILE_EXT, ".config").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv get(Keys, ?APP_NAME)
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys()) -> Value :: value() | no_return().
get(Keys) ->
    get(Keys, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from application's memory.
%% Throws an exception when value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), AppName :: atom()) -> Value :: value() | no_return().
get(Keys, AppName) ->
    case find(Keys, AppName) of
        {ok, Value} -> Value;
        #error{reason = ?ERR_NOT_FOUND} -> ?throw_error(?ERR_NOT_FOUND, [Keys, AppName])
    end.


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from application's memory.
%% Returns the Default when value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), AppName :: atom(), Default :: value()) ->
    Value :: value() | no_return().
get(Keys, AppName, Default) ->
    case find(Keys, AppName) of
        {ok, Value} -> Value;
        #error{reason = ?ERR_NOT_FOUND} -> Default
    end.


%%--------------------------------------------------------------------
%% @doc Returns cluster type of this Onepanel instance.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_type() -> onedata:cluster_type().
get_cluster_type() ->
    ?MODULE:get(cluster_type).


%%--------------------------------------------------------------------
%% @doc Returns value of a application variable from from memory of application
%% on another node.
%% Throws an exception when value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec get_remote(Node :: node(), Keys :: keys(), AppName :: atom()) ->
    Value :: value().
get_remote(Node, Keys, AppName) ->
    case find_remote(Node, Keys, AppName) of
        {ok, Value} -> Value;
        #error{reason = ?ERR_NOT_FOUND} -> ?throw_error(?ERR_NOT_FOUND, [Node, Keys, AppName])
    end.


-spec typed_get(Keys :: keys(), Type :: onepanel_utils:type()) -> value().
typed_get(Keys, Type) ->
    onepanel_utils:convert(?MODULE:get(Keys), Type).


-spec typed_get(Keys :: keys(), AppName :: atom(), Type :: onepanel_utils:type()) -> value().
typed_get(Keys, AppName, Type) ->
    onepanel_utils:convert(?MODULE:get(Keys, AppName), Type).


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
%% @doc Returns value of an application variable from memory
%% of an application running on given Node.
%% @end
%%--------------------------------------------------------------------
-spec find_remote(Node :: node(), Keys :: keys(), AppName :: atom()) ->
    {ok, Value :: value()} | #error{} | no_return().
find_remote(Node, Keys, AppName) ->
    Env = rpc:call(Node, application, get_all_env, [AppName]),
    onepanel_lists:get(Keys, Env).


%%--------------------------------------------------------------------
%% @doc @equiv set(Keys, Value, ?APP_NAME)
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value()) -> ok.
set(Keys, Value) ->
    set(Keys, Value, ?APP_NAME).


%%--------------------------------------------------------------------
%% @doc Sets in-memory value of a application env variable
%% on the onepanel node.
%% @end
%%--------------------------------------------------------------------
-spec set(Keys :: keys(), Value :: value(), AppName :: atom()) -> ok.
set(Keys, Value, AppName) ->
    lists:foreach(fun({K, V}) ->
        application:set_env(AppName, K, V)
    end, onepanel_lists:store(Keys, Value, application:get_all_env())).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's memory on given
%% onepanel nodes.
%% @end
%%--------------------------------------------------------------------
-spec set(Nodes :: [node()], Keys :: keys(), Value :: value(), AppName :: atom()) ->
    Results :: onepanel_rpc:results() | no_return().
set(Nodes, Keys, Value, AppName) ->
    onepanel_rpc:call_all(Nodes, ?MODULE, set, [Keys, Value, AppName]).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable on given node(s).
%% The nodes can be of any service.
%% @end
%%--------------------------------------------------------------------
-spec set_remote(Node :: node() | [node()], Keys :: keys(), Value :: value(),
    AppName :: atom()) -> ok | no_return().
set_remote(Node, Keys, Value, AppName) when is_atom(Node) ->
    set_remote([Node], Keys, Value, AppName);
set_remote(Nodes, Keys, Value, AppName) ->
    lists:map(fun(Node) ->
        NewEnv = case rpc:call(Node, application, get_all_env, [AppName]) of
            {badrpc, _} = Error -> ?throw_error(Error);
            Result -> onepanel_lists:store(Keys, Value, Result)
        end,

        lists:foreach(fun({K, V}) ->
            ok = rpc:call(Node, application, set_env, [AppName, K, V])
        end, NewEnv)
    end, Nodes),
    ok.


%%--------------------------------------------------------------------
%% @doc Returns value of an application variable from application's configuration
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
%% @doc
%% Reads value of an application variable from the first configuration
%% file containing it, in order of overlays priority.
%% Contrary to read/2, reading all config variables by passing [] or [AppName]
%% as Keys is not possible.
%% @end
%%--------------------------------------------------------------------
-spec read_effective(Keys :: keys(), ServiceName :: service:name()) ->
    {ok, Value :: value()} | #error{}.
read_effective([_AppName, _EnvName | _] = Keys, ServiceName) ->
    onepanel_lists:foldl_while(fun(Path, Prev) ->
        try read(Keys, Path) of
            {ok, Val} -> {halt, {ok, Val}};
            _ -> {cont, Prev}
        catch
            _:_ -> {cont, Prev}
        end
    end, ?make_error(?ERR_NOT_FOUND), get_config_paths(ServiceName));

read_effective(_, _) ->
    error(badarg).


%%--------------------------------------------------------------------
%% @doc
%% Reads value of an application variable from the first configuration
%% file containing it, in order of overlays priority.
%% If no file specifies the variable, default value is returned.
%% @end
%%--------------------------------------------------------------------
-spec read_effective(Keys :: keys(), ServiceName :: service:name(),
    Default :: value()) -> Value :: value().
read_effective(Keys, ServiceName, Default) ->
    case read_effective(Keys, ServiceName) of
        {ok, Value} -> Value;
        #error{} -> Default
    end.


%%--------------------------------------------------------------------
%% @doc @equiv write(Keys, Value, get_config_path(?APP_NAME, generated))
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: keys(), Value :: value()) -> ok | no_return().
write(Keys, Value) ->
    write(Keys, Value, get_config_path(?APP_NAME, generated)).


%%--------------------------------------------------------------------
%% @doc Sets value of a application variable in application's configuration file.
%% @end
%%--------------------------------------------------------------------
-spec write(Keys :: keys(), Value :: value(), Path :: file:name_all()) ->
    ok | no_return().
write(Keys, Value, Path) ->
    AppConfigs = case file:consult(Path) of
        {ok, [Configs]} -> Configs;
        _ -> []
    end,

    NewConfigs = onepanel_lists:store(Keys, Value, AppConfigs),
    NewConfigsStr = io_lib:fwrite("~s~n~p.", [?DO_NOT_MODIFY_HEADER, NewConfigs]),
    case file:write_file(Path, NewConfigsStr) of
        ok -> ok;
        {error, Reason} -> ?throw_error(Reason)
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


%%--------------------------------------------------------------------
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% Variables missing from the source file are skipped.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:name(), Variables :: [keys()]) ->
    ok | no_return().
migrate_generated_config(ServiceName, Variables) ->
    migrate_generated_config(ServiceName, Variables, false).

%%--------------------------------------------------------------------
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% Variables missing from the source file are skipped.
%% If SetInRuntime is enabled values being copied are also set
%% in the live config.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:name(), Variables :: [keys()],
    SetInRuntime :: boolean()) -> ok | no_return().
migrate_generated_config(ServiceName, Variables, SetInRuntime) ->
    Src = get_config_path(ServiceName, legacy),
    Dst = get_config_path(ServiceName, generated),
    case file:consult(Src) of
        {ok, [LegacyConfigs]} ->
            ?info("Migrating app config from '~s' to '~s'", [Src, Dst]),
            Values = lists:filtermap(fun(Variable) ->
                case onepanel_lists:get(Variable, LegacyConfigs) of
                    {ok, Val} -> {true, {Variable, Val}};
                    #error{} -> false
                end
            end, Variables),

            lists:foreach(fun({Variable, Val}) ->
                write(Variable, Val, Dst),
                case SetInRuntime of
                    true ->
                        [AppName | Key] = Variable,
                        onepanel_env:set(Key, Val, AppName);
                    false -> ok
                end
            end, Values),

            case file:rename(Src, [Src, ".bak"]) of
                ok -> ok;
                {error, Reason} -> ?throw_error(Reason, [])
            end;
        {error, Reason} -> ?throw_error(Reason, [])
    end.


%%--------------------------------------------------------------------
%% @doc Checks if an old app.config which should be migrated exists.
%% @end
%%--------------------------------------------------------------------
-spec legacy_config_exists(service:name()) -> boolean().
legacy_config_exists(ServiceName) ->
    filelib:is_regular(get_config_path(ServiceName, legacy)).


%%--------------------------------------------------------------------
%% @doc Returns path to given layer of config file for given service.
%% Available layers are:
%% - app - the basic file bundled with a release
%% - generated - file written by onepanel
%% - overlay - file for custom overrides
%% - legacy - app config file from older versions, with onepanel modifications
%%
%% Relies on the paths being provided in predictable variables in
%% onepanel app config, for example op_worker_app_config_file where
%% op_worker is the service name.
%% @end
%%--------------------------------------------------------------------
-spec get_config_path(ServiceName :: service:name(), ConfigLayer) ->
    Path :: string() | no_return()
    when ConfigLayer :: app | generated | overlay | legacy.
get_config_path(ServiceName, ConfigLayer) ->
    EnvName = str_utils:format("~s_~s_config_file", [ServiceName, ConfigLayer]),
    onepanel_env:get(list_to_atom(EnvName)).


%%--------------------------------------------------------------------
%% @doc Migrates (renames) variable in the autogenerated config file.
%%
%% If variable with the old name exists, changes its name to the new one
%% and returns true. If OldKeys is missing, returns false.
%% @end
%%--------------------------------------------------------------------
-spec migrate(ServiceName :: service:name(), OldKeys :: keys(), NewKeys :: keys()) ->
    Found :: boolean().
migrate(ServiceName, OldKeys, NewKeys) ->
    % invocation via ?MODULE for meck in eunit tests
    Path = ?MODULE:get_config_path(ServiceName, generated),
    case read([], Path) of
        {ok, OldConfigs} ->
            case onepanel_lists:rename(OldKeys, NewKeys, OldConfigs) of
                OldConfigs ->
                    false;
                NewConfigs ->
                    write([], NewConfigs, Path),
                    true
            end;
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @doc Executes {@link migrate/3} on all Onepanel nodes.
%% @end
%%--------------------------------------------------------------------
-spec migrate(PanelNodes :: [node()], ServiceName :: service:name(),
    OldKeys :: keys(), NewKeys :: keys()) ->
    Results :: onepanel_rpc:results() | no_return().
migrate(PanelNodes, ServiceName, OldKeys, NewKeys) ->
    onepanel_rpc:call_all(PanelNodes, ?MODULE, migrate, [ServiceName, OldKeys, NewKeys]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Returns paths of configuration files of given service in order
%% of decreasing priority.
%% @end
%%--------------------------------------------------------------------
-spec get_config_paths(Service :: service:name()) -> [file:name()].
get_config_paths(ServiceName) ->
    Overlay = get_config_path(ServiceName, overlay),
    Generated = get_config_path(ServiceName, generated),
    App = get_config_path(ServiceName, app),
    [Overlay | list_config_dir(ServiceName)] ++ [Generated, App].


%%--------------------------------------------------------------------
%% @private
%% @doc Returns paths of files in the config.d directory which have
%% .config extension. The files are sorted by decreasing priority.
%% @end
%%--------------------------------------------------------------------
-spec list_config_dir(service:name()) -> [file:name_all()].
list_config_dir(ServiceName) ->
    EnvName = str_utils:format("~s_custom_config_dir", [ServiceName]),
    DirPath = onepanel_env:get(list_to_atom(EnvName)),
    case file:list_dir(DirPath) of
        {ok, Files} ->
            Matching = lists:filter(fun(Filename) ->
                ?FILE_EXT == filename:extension(Filename)
            end, Files),
            % files lexicographically greater have higher priority.
            Sorted = lists:reverse(lists:sort(Matching)),
            [filename:join(DirPath, File) || File <- Sorted];
        {error, _} ->
            []
    end.
