%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility installation functions.
%% @end
%% ===================================================================
-module(installer_utils).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_system_limit/2, get_system_limits_cmd/1]).
-export([get_global_config/0, get_timestamp/0, set_timestamp/0]).
-export([add_node_to_config/3, remove_node_from_config/1, overwrite_config_args/4]).

%% ====================================================================
%% API functions
%% ====================================================================

%% set_system_limit/2
%% ====================================================================
%% @doc Sets system limit on local host.
%% @end
-spec set_system_limit(Type :: atom(), Value :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_system_limit(Type, Value) ->
    try
        Host = onepanel_utils:get_host(node()),
        case file:consult(?ULIMITS_CONFIG_PATH) of
            {ok, Limits} ->
                case lists:keymember(Type, 1, Limits) of
                    true ->
                        ok;
                    _ ->
                        ok = file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n", [{Type, integer_to_list(Value)}]), [append])
                end;
            _ ->
                ok = file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n", [{Type, integer_to_list(Value)}]), [append])
        end,
        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{Type, Value}])
    catch
        _:Reason ->
            ?error("Cannot set ulimits: ~p", [Reason]),
            {error, Reason}
    end.


%% get_system_limits_cmd/0
%% ====================================================================
%% @doc Returns ulimits command required during database or oneprovider
%% node installation.
%% @end
-spec get_system_limits_cmd(Host :: string()) -> Result when
    Result :: string().
%% ====================================================================
get_system_limits_cmd(Host) ->
    try
        {ok, #?LOCAL_CONFIG_RECORD{open_files = OpenFiles, process_limit = Processes}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
        "ulimit -n " ++ integer_to_list(OpenFiles) ++ " ; ulimit -u " ++ integer_to_list(Processes)
    catch
        _:Reason ->
            ?error("Cannot get ulimits configuration: ~p. Returning default values.", [Reason]),
            "ulimit -n " ++ integer_to_list(?OPEN_FILES) ++ " ; ulimit -u " ++ integer_to_list(?PROCESSES)
    end.


%% add_node_to_config/3
%% ====================================================================
%% @doc Adds a node to configured_nodes.cfg.
%% @end
-spec add_node_to_config(Type :: atom(), Name :: string(), Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_node_to_config(Type, Name, Path) ->
    try
        Entries = case file:consult(?CONFIGURED_NODES_PATH) of
                      {ok, Content} -> Content;
                      {error, enoent} -> [];
                      Other -> throw(Other)
                  end,
        case lists:keyfind(Type, 1, Entries) of
            true -> ok;
            _ -> save_nodes_in_config(Entries ++ [{Type, Name, Path}])
        end
    catch
        _:Reason ->
            ?error("Cannot add ~p node to ~s: ~p", [Name, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% remove_node_from_config/1
%% ====================================================================
%% @doc Removes a node from configured_nodes.cfg.
%% @end
-spec remove_node_from_config(Type :: atom()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_node_from_config(Type) ->
    try
        {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
        ToDelete = case lists:keyfind(Type, 1, Entries) of
                       false -> ?warning("Node ~p not found among configured nodes.", [Type]);
                       Term -> Term
                   end,
        save_nodes_in_config(Entries -- [ToDelete])
    catch
        _:Reason ->
            ?error("Cannot delete ~p from ~s: ~p", [Type, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% overwrite_config_args/4
%% ====================================================================
%% @doc Overwrites a parameter in config.args.
%% @end
-spec overwrite_config_args(Path :: string(), Parameter :: binary(), Pattern :: binary(), NewValue :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
overwrite_config_args(Path, Parameter, Pattern, NewValue) ->
    try
        {ok, FileContent} = file:read_file(Path),
        {match, [{From, Through}]} = re:run(FileContent, <<Parameter/binary, Pattern/binary>>),
        Beginning = binary:part(FileContent, 0, From),
        End = binary:part(FileContent, From + Through, size(FileContent) - From - Through),
        file:write_file(Path, <<Beginning/binary, Parameter/binary, NewValue/binary, End/binary>>),
        ok
    catch
        _:Reason ->
            ?error("Cannot overwrite config args: ~p", [Reason]),
            {error, Reason}
    end.


%% get_global_config/0
%% ====================================================================
%% @doc Returns global installation configuration.
%% @end
-spec get_global_config() -> Result when
    Result :: list().
%% ====================================================================
get_global_config() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, Record} ->
            Fields = record_info(fields, ?GLOBAL_CONFIG_RECORD),
            [_ | Values] = tuple_to_list(Record),
            lists:zip(Fields, Values);
        _ -> []
    end.


%% get_timestamp/0
%% ====================================================================
%% @doc Returns timestamp (microseconds since epoch) of last global 
%% configuration change.
%% @end
-spec get_timestamp() -> Result when
    Result :: integer().
%% ====================================================================
get_timestamp() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        #?GLOBAL_CONFIG_RECORD{timestamp = Timestamp} ->
            Timestamp;
        _ ->
            0
    end.


%% set_timestamp/0
%% ====================================================================
%% @doc Sets timestamp for global configuration to microsecond since 
%% epoch.
%% @end
-spec set_timestamp() -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = 1000000000000 * MegaSecs + 1000000 * Secs + MicroSecs,
    dao:update_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID, [{timestamp, Timestamp}]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% save_nodes_in_config/1
%% ====================================================================
%% @doc Saves list of nodes in configured_nodes.cfg.
%% @end
-spec save_nodes_in_config(Name :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
save_nodes_in_config(NodeList) ->
    try
        file:write_file(?CONFIGURED_NODES_PATH, ""),
        lists:foreach(
            fun(Node) ->
                file:write_file(?CONFIGURED_NODES_PATH, io_lib:fwrite("~p.\n", [Node]), [append])
            end, NodeList),
        ok
    catch
        _:Reason ->
            ?error("Cannot write to ~s: ~p", [?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.