%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This file contains utility functions used to modify application config.
%%% @end
%%%--------------------------------------------------------------------
-module(app_config).
-author("Krzysztof Trzepla").

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([path/1, get/2, set/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns application config path.
%% @end
%%--------------------------------------------------------------------
-spec path(Ref :: atom() | string()) -> Path :: string().
path(?APP_NAME) ->
    {ok, EtcDir} = application:get_env(?APP_NAME, onepanel_etc_dir),
    filename:join(EtcDir, "app.config");
path(AppName) when is_atom(AppName) ->
    {ok, EtcDir} = application:get_env(?APP_NAME, platform_etc_dir),
    filename:join([EtcDir, AppName, "app.config"]);
path(Ref) when is_list(Ref) ->
    Ref.

%%--------------------------------------------------------------------
%% @doc
%% Returns value from application configuration.
%% @end
%%--------------------------------------------------------------------
-spec get(Ref :: atom() | string(), Keys :: list()) -> Value :: term().
get(Ref, Keys) when is_list(Keys) ->
    case file:consult(path(Ref)) of
        {ok, [Terms]} ->
            lists:foldl(fun
                (Key, Values) when is_list(Values) ->
                    case lists:keyfind(Key, 1, Values) of
                        {Key, NewValues} -> NewValues;
                        false -> undefined
                    end;
                (_, _) ->
                    undefined
            end, Terms, Keys);
        {error, Reason} ->
            {error, Reason}
    end;
get(Ref, Ref) when is_atom(Ref) ->
    get(Ref, [Ref]);
get(Ref, Key) when is_atom(Ref) ->
    get(Ref, [Ref, Key]).

%%--------------------------------------------------------------------
%% @doc
%% Sets value in application configuration.
%% @end
%%--------------------------------------------------------------------
-spec set(Ref :: atom() | string(), Keys :: list(), Value :: term()) ->
    ok | {error, Reason :: term()}.
set(Ref, Keys, Value) when is_list(Keys) ->
    Path = path(Ref),
    case file:consult(Path) of
        {ok, [Terms]} ->
            NewTerms = store(Keys, Value, Terms),
            file:write_file(Path, [io_lib:fwrite("~p", [NewTerms]), $.]);
        {error, Reason} ->
            {error, Reason}
    end;
set(Ref, Ref, Value) when is_atom(Ref) ->
    set(Ref, [Ref], Value);
set(Ref, Key, Value) when is_atom(Ref) ->
    set(Ref, [Ref, Key], Value).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Stores value in a deep proplists.
%% @end
%%--------------------------------------------------------------------
-spec store(Keys :: list(), Value :: term(), Terms :: list()) -> list().
store([], Value, _Terms) ->
    Value;
store([Key | Keys], Value, Terms) ->
    NewInnerTerms = case lists:keyfind(Key, 1, Terms) of
        {Key, InnerTerms} -> store(Keys, Value, InnerTerms);
        false -> store(Keys, Value, [])
    end,
    lists:keystore(Key, 1, Terms, {Key, NewInnerTerms});
store(Key, Value, Terms) ->
    store([Key], Value, Terms).