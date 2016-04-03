%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file contains functions used to overwrite batch mode configuration with
%%% environment variables.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cli_env).
-author("Krzysztof Trzepla").

-include("onepanel_cli.hrl").
-include("registered_names.hrl").

%% API
-export([get/2, get/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns value of environment variable.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: list(), Envs :: proplists:property()) -> Value :: term().
get(?CONFIG_KEY, Envs) ->
    {ok, Name} = application:get_env(?APP_NAME, application_env_config),
    onepanel_cli_env:get(Name, Envs);

get([?CLUSTER_KEY, ?NODES_KEY], Envs) ->
    get_list_env("CLUSTER_NODES", Envs);
get([?CLUSTER_KEY, ?NODES_KEY, Name, ?HOSTNAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get("CLUSTER_NODES_" ++ ValidName ++ "_HOSTNAME", Envs);
get([?CLUSTER_KEY, ?NODES_KEY, Name, ?OPEN_FILES_LIMIT_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_NODES_" ++ ValidName ++ "_OPEN_FILES_LIMIT", Envs
    );
get([?CLUSTER_KEY, ?NODES_KEY, Name, ?PROCESSES_LIMIT_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_NODES_" ++ ValidName ++ "_PROCESSES_LIMIT", Envs
    );

get([?CLUSTER_KEY, ?MANAGER_KEY, ?DEFAULT_NODE_KEY], Envs) ->
    onepanel_cli_env:get("CLUSTER_MANAGER_DEFAULT_NODE", Envs);
get([?CLUSTER_KEY, ?MANAGER_KEY, ?NODES_KEY], Envs) ->
    get_list_env("CLUSTER_MANAGER_NODES", Envs);

get([?CLUSTER_KEY, ?WORKER_KEY, ?NODES_KEY], Envs) ->
    get_list_env("CLUSTER_WORKER_NODES", Envs);

get([?CLUSTER_KEY, ?DATABASE_KEY, ?MEMORY_QUOTA_KEY], Envs) ->
    get_list_env("CLUSTER_DATABASE_MEMORY_QUOTA", Envs);
get([?CLUSTER_KEY, ?DATABASE_KEY, ?NODES_KEY], Envs) ->
    get_list_env("CLUSTER_DATABASE_NODES", Envs);

get([?CLUSTER_KEY, ?STORAGE_KEY], Envs) ->
    get_list_env("CLUSTER_STORAGE", Envs);
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?TYPE_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get("CLUSTER_STORAGE_" ++ ValidName ++ "_TYPE", Envs);
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?MOUNT_POINT_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_MOUNT_POINT", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?ACCESS_KEY_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_ACCESS_KEY", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?SECRET_KEY_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_SECRET_KEY", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?S3_HOSTNAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_S3_HOSTNAME", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?BUCKET_NAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_BUCKET_NAME", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?USERNAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get("CLUSTER_STORAGE_" ++ ValidName ++ "_USERNAME", Envs);
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?KEY_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get("CLUSTER_STORAGE_" ++ ValidName ++ "_KEY", Envs);
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?MONITOR_HOST_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_MONITOR_HOST", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?CLUSTER_NAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get(
        "CLUSTER_STORAGE_" ++ ValidName ++ "_CLUSTER_NAME", Envs
    );
get([?CLUSTER_KEY, ?STORAGE_KEY, Name, ?POOL_NAME_KEY], Envs) ->
    ValidName = ensure_valid_env_name(Name),
    onepanel_cli_env:get("CLUSTER_STORAGE_" ++ ValidName ++ "_POOL_NAME", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?OPEN_FILES_LIMIT_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_OPEN_FILES_LIMIT", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?PROCESSES_LIMIT_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_PROCESSES_LIMIT", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?WEB_PRIVATE_KEY_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_WEB_PRIVATE_KEY", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?WEB_CERTIFICATE_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_WEB_CERTIFICATE", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?WEB_CA_CERTIFICATE_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_WEB_CA_CERTIFICATE", Envs);
get([?CLUSTER_KEY, ?SETTINGS_KEY, ?OPEN_ID_AUTH_CONFIG_KEY], Envs) ->
    get_list_env("CLUSTER_SETTINGS_OPEN_ID_AUTH_CONFIG", Envs);
get([?ONEPROVIDER_KEY, ?REGISTER_KEY], Envs) ->
    onepanel_cli_env:get("ONEPROVIDER_REGISTER", Envs);
get([?ONEPROVIDER_KEY, ?NAME_KEY], Envs) ->
    onepanel_cli_env:get("ONEPROVIDER_NAME", Envs);
get([?ONEPROVIDER_KEY, ?REDIRECTION_POINT_KEY], Envs) ->
    onepanel_cli_env:get("ONEPROVIDER_REDIRECTION_POINT", Envs);
get([?ONEPROVIDER_KEY, ?GEO_LONGITUDE_KEY], Envs) ->
    onepanel_cli_env:get("ONEPROVIDER_GEO_LONGITUDE", Envs);
get([?ONEPROVIDER_KEY, ?GEO_LATITUDE_KEY], Envs) ->
    onepanel_cli_env:get("ONEPROVIDER_GEO_LATITUDE", Envs);

get([?ONEZONE_KEY, ?DOMAIN_NAME_KEY], Envs) ->
    onepanel_cli_env:get("ONEZONE_DOMAIN_NAME", Envs);
get(Key, Envs) ->
    {ok, Prefix} = application:get_env(?APP_NAME, application_env_prefix),
    Env = case string:str(Key, Prefix) of
        1 -> Key;
        _ -> string:join([Prefix, Key], "_")
    end,
    proplists:get_value(Env, Envs).

%%--------------------------------------------------------------------
%% @doc
%% Returns value of environment variable. If missing returns default value.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: list(), Envs :: proplists:property(), Default :: term()) ->
    Value :: term().
get(Keys, Envs, Default) ->
    case onepanel_cli_env:get(Keys, Envs) of
        undefined -> Default;
        Value -> Value
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Replaces whitespace characters with underscore in name of environment variable.
%% @end
%%--------------------------------------------------------------------
-spec ensure_valid_env_name(Name :: string()) -> string().
ensure_valid_env_name(Name) ->
    string:join(string:tokens(Name, " "), "_").

%%--------------------------------------------------------------------
%% @doc
%% Returns value of environment variable as a list.
%% @end
%%--------------------------------------------------------------------
-spec get_list_env(Key :: list(), Envs :: proplists:property()) -> Value :: list().
get_list_env(Key, Envs) ->
    case onepanel_cli_env:get(Key, Envs) of
        undefined -> undefined;
        ListEnv -> string:tokens(ListEnv, " ,;")
    end.