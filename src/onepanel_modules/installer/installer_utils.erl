%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains utility installation functions.
%% @end
%% ===================================================================
-module(installer_utils).

-include("registered_names.hrl").
-include("onepanel_modules/db/common.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([set_ulimits/2, get_ulimits_cmd/1]).
-export([get_main_ccm/0, get_global_config/0]).
-export([add_node_to_config/3, remove_node_from_config/1, overwrite_config_args/3]).

%% ====================================================================
%% API functions
%% ====================================================================


%% set_ulimits/2
%% ====================================================================
%% @doc Sets system limits for open files and processes on local host.
%% @end
-spec set_ulimits(OpenFiles :: integer(), Processes :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
set_ulimits(OpenFiles, Processes) ->
    try
        Host = onepanel_utils:get_host(node()),
        ok = file:write_file(?ULIMITS_CONFIG_PATH, io_lib:fwrite("~p.\n~p.\n", [{open_files, integer_to_list(OpenFiles)}, {process_limit, integer_to_list(Processes)}])),
        ok = dao:update_record(?LOCAL_CONFIG_TABLE, Host, [{open_files_limit, integer_to_list(OpenFiles)}, {processes_limit, integer_to_list(Processes)}])
    catch
        _:Reason ->
            ?error("Cannot set ulimits: ~p", [Reason]),
            {error, Reason}
    end.


%% get_ulimits_cmd/0
%% ====================================================================
%% @doc Returns ulimits command required during database or veil node installation.
%% @end
-spec get_ulimits_cmd(Host :: string()) -> Result when
    Result :: string().
%% ====================================================================
get_ulimits_cmd(Host) ->
    try
        {ok, #?LOCAL_CONFIG_RECORD{open_files_limit = OpenFiles, processes_limit = Processes}} = dao:get_record(?LOCAL_CONFIG_TABLE, Host),
        "ulimit -n " ++ OpenFiles ++ " ; ulimit -u " ++ Processes
    catch
        _:Reason ->
            ?error("Cannot get ulimits configuration: ~p. Returning default values.", [Reason]),
            "ulimit -n " ++ ?DEFAULT_OPEN_FILES ++ " ; ulimit -u " ++ ?DEFAULT_PROCESSES
    end.


%% add_node_to_config/3
%% ====================================================================
%% @doc Adds a node to configured_nodes.cfg.
-spec add_node_to_config(Type :: atom(), Name :: string(), Path :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
add_node_to_config(Type, Name, Path) ->
    try
        {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
        save_nodes_in_config(Entries ++ [{Type, Name, Path}])
    catch
        _:Reason ->
            ?error("Cannot add ~p node to ~s: ~p", [Name, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% remove_node_from_config/1
%% ====================================================================
%% @doc Removes a node from configured_nodes.cfg.
-spec remove_node_from_config(Name :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
remove_node_from_config(Name) ->
    try
        {ok, Entries} = file:consult(?CONFIGURED_NODES_PATH),
        ToDelete = case lists:keyfind(Name, 1, Entries) of
                       false -> ?warning("Node ~p not found among configured nodes.", [Name]);
                       Term -> Term
                   end,
        save_nodes_in_config(Entries -- [ToDelete])
    catch
        _:Reason ->
            ?error("Cannot delete ~p node from ~s: ~p", [Name, ?CONFIGURED_NODES_PATH, Reason]),
            {error, Reason}
    end.


%% save_nodes_in_config/1
%% ====================================================================
%% @doc Saves list of nodes in configured_nodes.cfg.
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


%% overwrite_config_args/3
%% ====================================================================
%% @doc Overwrites a parameter in config.args.
-spec overwrite_config_args(Path :: string(), Parameter :: string(), NewValue :: string()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
overwrite_config_args(Path, Parameter, NewValue) ->
    try
        FileContent = case file:read_file(Path) of
                          {ok, DataRead} ->
                              binary_to_list(DataRead);
                          _ ->
                              throw("Could not read config.args file")
                      end,

        {match, [{From, Through}]} = re:run(FileContent, Parameter ++ ":.*\n"),
        Beginning = string:substr(FileContent, 1, From),
        End = string:substr(FileContent, From + Through, length(FileContent) - From - Through + 1),
        file:write_file(Path, list_to_binary(Beginning ++ Parameter ++ ": " ++ NewValue ++ End)),
        ok
    catch
        _:Reason ->
            ?error("Cannot overwrite config args: ~p", [Reason]),
            {error, Reason}
    end.


%% get_main_ccm/0
%% ====================================================================
%% @doc Returns configured main CCM.
-spec get_main_ccm() -> Result when
    Result :: undefined | string().
%% ====================================================================
get_main_ccm() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, #?GLOBAL_CONFIG_RECORD{main_ccm = MainCCM}} ->
            MainCCM;
        _ ->
            undefined
    end.


%% get_global_config/0
%% ====================================================================
%% @doc Returns global installation configuration.
-spec get_global_config() -> list().
%% ====================================================================
get_global_config() ->
    case dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID) of
        {ok, Record} ->
            Fields = record_info(fields, ?GLOBAL_CONFIG_RECORD),
            [_ | Values] = tuple_to_list(Record),
            lists:zip(Fields, Values);
        _ -> []
    end.