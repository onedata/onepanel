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
-module(vm_config_editor).
-author("Krzysztof Trzepla").

%% API
-export([read/2, write/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec read(Key :: binary(), Path :: file:name_all()) ->
    {ok, Value :: binary()} | {error, Reason :: term()}.
read(Key, Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            get(Key, Content);
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec write(Key :: binary(), Value :: binary(), Path :: file:name_all()) ->
    ok | {error, Reason :: term()}.
write(Key, Value, Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            case set(Key, Value, Content) of
                {ok, NewContent} -> file:write_file(Path, NewContent);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: binary(), Content :: binary()) ->
    {ok, Value :: binary()} | {error, Reason :: term()}.
get(Key, Content) ->
    Pattern = <<"^\\-", Key/binary, ".*">>,
    Options = [{capture, first, binary}, multiline],
    KeySize = size(Key),
    case re:run(Content, Pattern, Options) of
        {match, [<<"-", Key:KeySize/binary, Value/binary>>]} ->
            {ok, trim(Value, both)};
        nomatch ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set(Key :: binary(), Value :: binary(), Content :: binary()) ->
    {ok, NewContent :: binary()} | {error, Reason :: term()}.
set(Key, Value, Content) ->
    case get(Key, Content) of
        {ok, _} ->
            Pattern = <<"^\\-", Key/binary, ".*">>,
            Options = [{return, binary}, multiline],
            {ok, re:replace(Content, Pattern,
                <<"-", Key/binary, " ", Value/binary>>, Options)};
        {error, not_found} ->
            {ok, <<Content/binary, "\n\n-", Key/binary, " ", Value/binary>>};
        {error, Reason} ->
            {error, Reason}
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
-spec trim(Text :: binary(), Side :: left | right | both) -> NewText :: binary().
trim(Text, left) ->
    re:replace(Text, <<"^\\s+">>, <<>>, [{return, binary}]);
trim(Text, right) ->
    re:replace(Text, <<"\\s+$">>, <<>>, [{return, binary}]);
trim(Text, both) ->
    trim(trim(Text, left), right).
