%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file contains functions used for command line interface arguments parsing.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cli_config_parser).
-author("Krzysztof Trzepla").

-include("onepanel_cli.hrl").
-include("registered_names.hrl").

-type(config() :: proplists:proplist()).

%% API
-export([parse/1, parse/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse configuration from environment variables.
%% @end
%%--------------------------------------------------------------------
-spec parse(Envs :: proplists:proplist()) -> Config :: #{}.
parse(Envs) ->
    case onepanel_cli_env:get(?CONFIG_KEY, Envs) of
        undefined -> parse_config([], Envs);
        EnvConfig -> parse_env_config(EnvConfig, Envs)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Tries to parse configuration from environment variables and in case they
%% are not specified parses file.
%% @end
%%--------------------------------------------------------------------
-spec parse(FileConfig :: string(), Envs :: proplists:proplist()) -> Config :: #{}.
parse(FileConfig, Envs) ->
    case onepanel_cli_env:get(?CONFIG_KEY, Envs) of
        undefined -> parse_file_config(FileConfig, Envs);
        EnvConfig -> parse_env_config(EnvConfig, Envs)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse configuration from environment variables.
%% @end
%%--------------------------------------------------------------------
-spec parse_env_config(EnvConfig :: string(), Envs :: proplists:proplist()) -> 
    Config :: #{}.
parse_env_config(EnvConfig, Envs) ->
    [Config] = yamerl_constr:string(EnvConfig),
    parse_config(Config, Envs).

%%--------------------------------------------------------------------
%% @doc
%% Parse configuration from file.
%% @end
%%--------------------------------------------------------------------
-spec parse_file_config(FileConfig :: string(), Envs :: proplists:proplist()) ->
    Config :: #{}.
parse_file_config(FileConfig, Envs) ->
    [Config] = yamerl_constr:file(FileConfig),
    parse_config(Config, Envs).

%%--------------------------------------------------------------------
%% @doc
%% Parse configuration.
%% @end
%%--------------------------------------------------------------------
-spec parse_config(Config :: config(), Envs :: proplists:proplist()) ->
    Config :: #{}.
parse_config(Config, Envs) ->
    #{
        cluster => parse_config([?CLUSTER_KEY], Config, Envs),
        oneprovider => parse_config([?ONEPROVIDER_KEY], Config, Envs),
        onezone => parse_config([?ONEZONE_KEY], Config, Envs),
        onepanel => parse_config([?ONEPANEL_KEY], Config, Envs)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Parse configuration.
%% @end
%%--------------------------------------------------------------------
-spec parse_config(RevKey :: list(), Config :: config(), 
    Envs :: proplists:proplist()) -> Config :: #{}.
parse_config([?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?CLUSTER_KEY, ParentConfig, []),
    #{
        domain_name => get([?DOMAIN_NAME_KEY | RevKey], string, Config, Envs),
        nodes => parse_config([?NODES_KEY | RevKey], Config, Envs),
        manager => parse_config([?MANAGER_KEY | RevKey], Config, Envs),
        worker => parse_config([?WORKER_KEY | RevKey], Config, Envs),
        database => parse_config([?DATABASE_KEY | RevKey], Config, Envs),
        storage => parse_config([?STORAGE_KEY | RevKey], Config, Envs),
        settings => parse_config([?SETTINGS_KEY | RevKey], Config, Envs)
    };

parse_config([?NODES_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?NODES_KEY, ParentConfig, []),
    {ConfigNames, _} = lists:unzip(Config),
    Names = onepanel_cli_env:get(lists:reverse(RevKey), Envs, ConfigNames),

    lists:foldl(fun(Name, Acc) ->
        maps:put(Name, parse_config([Name | RevKey], Config, Envs), Acc)
    end, #{}, Names);

parse_config([Name, ?NODES_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(Name, ParentConfig, []),
    #{
        hostname => get([?HOSTNAME_KEY | RevKey], string, Config, Envs),
        open_files_limit => get(
            [?OPEN_FILES_LIMIT_KEY | RevKey], integer, Config, Envs
        ),
        processes_limit => get(
            [?PROCESSES_LIMIT_KEY | RevKey], integer, Config, Envs
        )
    };

parse_config([?MANAGER_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?MANAGER_KEY, ParentConfig, []),
    #{
        default_node_id => get([?DEFAULT_NODE_KEY | RevKey], string, Config, Envs),
        node_ids => get([?NODES_KEY | RevKey], list, Config, Envs)
    };

parse_config([?WORKER_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?WORKER_KEY, ParentConfig, []),
    #{
        node_ids => get([?NODES_KEY | RevKey], list, Config, Envs)
    };

parse_config([?DATABASE_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?DATABASE_KEY, ParentConfig, []),
    #{
        node_ids => get([?NODES_KEY | RevKey], list, Config, Envs)
    };

parse_config([?STORAGE_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?STORAGE_KEY, ParentConfig, []),
    {ConfigNames, _} = lists:unzip(Config),
    Names = onepanel_cli_env:get(lists:reverse(RevKey), Envs, ConfigNames),

    lists:foldl(fun(Name, Acc) ->
        maps:put(list_to_binary(Name), parse_config([Name | RevKey], Config, Envs), Acc)
    end, #{}, Names);

parse_config([Name, ?STORAGE_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(Name, ParentConfig, []),
    #{
        type => get([?TYPE_KEY | RevKey], atom, Config, Envs),
        mount_point => get([?MOUNT_POINT_KEY | RevKey], binary, Config, Envs),
        access_key => get([?ACCESS_KEY_KEY | RevKey], binary, Config, Envs),
        secret_key => get([?SECRET_KEY_KEY | RevKey], binary, Config, Envs),
        s3_hostname => get([?S3_HOSTNAME_KEY | RevKey], binary, Config, Envs),
        iam_hostname => get([?IAM_HOSTNAME_KEY | RevKey], binary, Config, Envs),
        bucket_name => get([?BUCKET_NAME_KEY | RevKey], binary, Config, Envs),
        username => get([?USERNAME_KEY | RevKey], binary, Config, Envs),
        key => get([?KEY_KEY | RevKey], binary, Config, Envs),
        monitor_host => get([?MONITOR_HOST_KEY | RevKey], binary, Config, Envs),
        cluster_name => get([?CLUSTER_NAME_KEY | RevKey], binary, Config, Envs),
        pool_name => get([?POOL_NAME_KEY | RevKey], binary, Config, Envs)
    };

parse_config([?SETTINGS_KEY, ?CLUSTER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?SETTINGS_KEY, ParentConfig, []),
    #{
        open_files_limit => get(
            [?OPEN_FILES_LIMIT_KEY | RevKey], integer, Config, Envs
        ),
        processes_limit => get(
            [?PROCESSES_LIMIT_KEY | RevKey], integer, Config, Envs
        ),
        web_private_key => get(
            [?WEB_PRIVATE_KEY_KEY | RevKey], string, Config, Envs
        ),
        web_certificate => get(
            [?WEB_CERTIFICATE_KEY | RevKey], string, Config, Envs
        ),
        web_ca_certificate => get(
            [?WEB_CA_CERTIFICATE_KEY | RevKey], string, Config, Envs
        ),
        open_id_auth_config => get(
            [?OPEN_ID_AUTH_CONFIG_KEY | RevKey], string, Config, Envs
        ),
        dns_config => get([?DNS_CONFIG_KEY | RevKey], string, Config, Envs)
    };

parse_config([?ONEPROVIDER_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?ONEPROVIDER_KEY, ParentConfig, []),
    #{
        register => get([?REGISTER_KEY | RevKey], atom, Config, Envs),
        name => get([?NAME_KEY | RevKey], binary, Config, Envs),
        redirection_point => get(
            [?REDIRECTION_POINT_KEY | RevKey], binary, Config, Envs
        ),
        geo_longitude => get(
            [?GEO_LONGITUDE_KEY | RevKey], float, Config, Envs
        ),
        geo_latitude => get([?GEO_LATITUDE_KEY | RevKey], float, Config, Envs)
    };

parse_config([?ONEZONE_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?ONEZONE_KEY, ParentConfig, []),
    #{
        name => get([?NAME_KEY | RevKey], string, Config, Envs),
        domain_name => get([?DOMAIN_NAME_KEY | RevKey], string, Config, Envs)
    };

parse_config([?ONEPANEL_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?ONEPANEL_KEY, ParentConfig, []),
    #{
        users => parse_config([?USERS_KEY | RevKey], Config, Envs)
    };
parse_config([?USERS_KEY, ?ONEPANEL_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(?USERS_KEY, ParentConfig, []),
    {ConfigNames, _} = lists:unzip(Config),
    Names = onepanel_cli_env:get(lists:reverse(RevKey), Envs, ConfigNames),

    lists:foldl(fun(Name, Acc) ->
        maps:put(Name, parse_config([Name | RevKey], Config, Envs), Acc)
    end, #{}, Names);

parse_config([Name, ?USERS_KEY, ?ONEPANEL_KEY] = RevKey, ParentConfig, Envs) ->
    Config = proplists:get_value(Name, ParentConfig, []),
    #{
        password => get([?PASSWORD_KEY | RevKey], string, Config, Envs),
        role => get([?ROLE_KEY | RevKey], atom, Config, Envs)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Gets value from config.
%% @end
%%--------------------------------------------------------------------
-spec get(RevKey :: list(), Type :: atom(), Config :: #{}, Envs :: proplists:proplist()) -> 
    Value :: term().
get([Name | _] = RevKey, Type, Config, Envs) ->
    Key = lists:reverse(RevKey),
    ConfigValue = proplists:get_value(Name, Config),
    case onepanel_cli_env:get(Key, Envs, ConfigValue) of
        undefined -> undefined;
        EnvValue -> convert(EnvValue, Type)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Converts value from one type to another.
%% @end
%%--------------------------------------------------------------------
-spec convert(Value :: term(), Type :: atom()) -> Value :: term().
convert(Value, Type) when is_atom(Value) -> convert(Value, atom, Type);
convert(Value, Type) when is_list(Value) -> convert(Value, string, Type);
convert(Value, Type) when is_binary(Value) -> convert(Value, binary, Type);
convert(Value, Type) when is_integer(Value) -> convert(Value, integer, Type);
convert(Value, Type) when is_float(Value) -> convert(Value, float, Type).

%%--------------------------------------------------------------------
%% @doc
%% Converts value from one to another.
%% @end
%%--------------------------------------------------------------------
-spec convert(Value :: term(), From :: atom(), To :: atom()) -> Value :: term().
convert(Value, string, To) ->
    convert(Value, list, To);
convert(Value, From, string) ->
    convert(Value, From, list);
convert(Value, Type, Type) ->
    Value;
convert(Value, From, To) ->
    Function = list_to_atom(atom_to_list(From) ++ "_to_" ++ atom_to_list(To)),
    apply(erlang, Function, [Value]).