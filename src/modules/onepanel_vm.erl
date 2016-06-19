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
-module(onepanel_vm).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

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
-spec read(Key :: binary(), Path :: file:name_all()) ->
    {ok, Value :: binary()} | #error{} | no_return().
read(Key, Path) ->
    case file:read_file(Path) of
        {ok, Content} -> get(Key, Content);
        {error, Reason} -> ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec write(Key :: binary(), Value :: binary(), Path :: file:name_all()) ->
    ok | no_return().
write(Key, Value, Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, NewContent} =  set(Key, Value, Content),
            case file:write_file(Path, NewContent) of
                ok -> ok;
                {error, Reason} -> ?throw(Reason)
            end;
        {error, Reason} ->
            ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: binary(), Content :: binary()) ->
    {ok, Value :: binary()} | #error{} | no_return().
get(Key, Content) ->
    Pattern = <<"^\\-", Key/binary, ".*">>,
    Options = [{capture, first, binary}, multiline],
    KeySize = size(Key),
    case re:run(Content, Pattern, Options) of
        {match, [<<"-", Key:KeySize/binary, Value/binary>>]} ->
            {ok, trim(Value, both)};
        nomatch ->
            ?error(?ERR_NOT_FOUND);
        {error, Reason} ->
            ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec set(Key :: binary(), Value :: binary(), Content :: binary()) ->
    {ok, NewContent :: binary()} | no_return().
set(Key, Value, Content) ->
    case get(Key, Content) of
        {ok, _} ->
            Pattern = <<"^\\-", Key/binary, ".*">>,
            Options = [{return, binary}, multiline],
            {ok, re:replace(Content, Pattern,
                <<"-", Key/binary, " ", Value/binary>>, Options)};
        #error{reason = ?ERR_NOT_FOUND} ->
            {ok, <<Content/binary, "\n-", Key/binary, " ", Value/binary>>}
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
