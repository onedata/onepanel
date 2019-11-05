%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains helper functions for modules implementing
%%% {@link rest_module_behavior}.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([has_privileges/2]).
-export([get_method/1, get_bindings/1, get_params/2, get_args/2,
    get_hosts/2, get_cluster_ips/1, verify_any/2, allowed_origin/0]).

-type one_or_many(T) :: T | [T].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a client has given privilege(s).
%% @end
%%--------------------------------------------------------------------
-spec has_privileges(Client :: rest_handler:client(), RequiredPrivileges) -> boolean()
    when RequiredPrivileges :: one_or_many(rest_handler:privilege()).
has_privileges(Client, RequiredPrivilege) when is_atom(RequiredPrivilege) ->
    has_privileges(Client, [RequiredPrivilege]);

has_privileges(#client{role = member, privileges = UserPrivileges}, RequiredPrivileges) ->
    ordsets:is_subset(
        ordsets:from_list(RequiredPrivileges),
        ordsets:from_list(UserPrivileges)
    );

has_privileges(#client{}, _RequiredPrivileges) -> false.


%%--------------------------------------------------------------------
%% @doc Converts REST method from binary to an atom representation.
%% @end
%%--------------------------------------------------------------------
-spec get_method(Req :: cowboy_req:req()) ->
    {Method :: rest_handler:method_type(), Req :: cowboy_req:req()}.
get_method(#{method := MethodBin} = Req) ->
    Method = case MethodBin of
        <<"POST">> -> 'POST';
        <<"PATCH">> -> 'PATCH';
        <<"GET">> -> 'GET';
        <<"PUT">> -> 'PUT';
        <<"DELETE">> -> 'DELETE'
    end,
    {Method, Req}.


%%--------------------------------------------------------------------
%% @doc Returns a map of endpoint bindings, e.g. for endpoint /users/:username
%% and request /users/user1 returns ```#{'username' => <<"user1">>}'''
%% @end
%%--------------------------------------------------------------------
-spec get_bindings(Req :: cowboy_req:req()) ->
    {Bindings :: rest_handler:bindings(), Req :: cowboy_req:req()}.
get_bindings(Req) ->
    Bindings = cowboy_req:bindings(Req),
    NewBindings = maps:map(fun
        (host, Value) -> onepanel_utils:convert(Value, list);
        (_Key, Value) -> Value
    end, Bindings),
    {NewBindings, Req}.


%%--------------------------------------------------------------------
%% @doc Returns a map of query string name and associated value.
%% @end
%%--------------------------------------------------------------------
-spec get_params(Req :: cowboy_req:req(), ParamsSpec :: rest_handler:spec()) ->
    {Params :: rest_handler:params(), Req :: cowboy_req:req()}.
get_params(Req, ParamsSpec) ->
    Params = cowboy_req:parse_qs(Req),
    NewParams = lists:foldl(fun
        ({Key, true}, Acc) -> maps:put(Key, <<"true">>, Acc);
        ({Key, Value}, Acc) -> maps:put(Key, Value, Acc)
    end, #{}, Params),
    try
        {onepanel_parser:parse(NewParams, ParamsSpec), Req}
    catch
        throw:#error{reason = {?ERR_MISSING_KEY, Keys}} = Error ->
            ?throw_error(Error#error{reason = {?ERR_MISSING_PARAM, Keys}})
    end.


%%--------------------------------------------------------------------
%% @doc Parses request body according to provided specification.
%% @end
%%--------------------------------------------------------------------
-spec get_args(Data :: rest_handler:data(), ArgsSpec :: rest_handler:spec()) ->
    Args :: rest_handler:args() | no_return().
get_args(Data, ArgsSpec) ->
    onepanel_parser:parse(Data, ArgsSpec).


%%--------------------------------------------------------------------
%% @doc Returns lists of hosts for given service based on cluster description
%% format.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts(Keys :: [atom()], Args :: rest_handler:args()) ->
    Hosts :: [service:host()] | no_return().
get_hosts(Keys, Args) ->
    CommonSuffix = common_hostname_suffix(Args),
    Nodes = nested:get([cluster, nodes], Args),
    HostsMap = maps:fold(fun(Alias, Props, Acc) ->
        Host = <<(maps:get(hostname, Props))/binary, CommonSuffix/binary>>,
        Acc#{Alias => Host}
    end, #{}, Nodes),
    Aliases = nested:get(Keys, Args),
    AliasesList = case erlang:is_list(Aliases) of
        true -> Aliases;
        false -> [Aliases]
    end,
    lists:map(fun(Alias) ->
        case maps:find(Alias, HostsMap) of
            {ok, Host} -> onepanel_utils:convert(Host, list);
            error -> ?throw_error({?ERR_HOST_NOT_FOUND_FOR_ALIAS, Alias})
        end
    end, AliasesList).


%%--------------------------------------------------------------------
%% @doc Returns map from cluster hosts to their IPs given
%% in cluster description.
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_ips(Args :: rest_handler:args()) ->
    #{service:host() => binary()} | no_return().
get_cluster_ips(Args) ->
    CommonSuffix = common_hostname_suffix(Args),
    Nodes = nested:get([cluster, nodes], Args),
    NodesWithIPs = maps:filter(fun(_Node, Props) ->
        maps:is_key(externalIp, Props)
    end, Nodes),

    maps:fold(fun(_Alias, Props, Acc) ->
        IP = maps:get(externalIp, Props),
        Host = onepanel_utils:convert(
            <<(maps:get(hostname, Props))/binary, CommonSuffix/binary>>,
            list),
        maps:put(Host, IP, Acc)
    end, #{}, NodesWithIPs).


%%--------------------------------------------------------------------
%% @doc Checks whether at least one key from a provided list is found in a map.
%% Throws an exception if none of keys is found.
%% @end
%%--------------------------------------------------------------------
-spec verify_any(Keys :: [atom()], Args :: rest_handler:args()) -> ok | no_return().
verify_any(Keys, Args) ->
    case lists:any(fun(Key) -> maps:is_key(Key, Args) end, Keys) of
        true -> ok;
        _ -> ?throw_error({?ERR_MISSING_ANY_KEY, Keys})
    end.


%%--------------------------------------------------------------------
%% @doc Returns domain hosting GUI which is allowed to perform
%% requests to the current cluster.
%% @end
%%--------------------------------------------------------------------
-spec allowed_origin() -> binary() | undefined.
allowed_origin() ->
    allowed_origin(onepanel_env:get_cluster_type()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec allowed_origin(onedata:cluster_type()) -> binary() | undefined.
allowed_origin(oneprovider) ->
    case service_oneprovider:is_registered() of
        true -> list_to_binary("https://" ++ service_oneprovider:get_oz_domain());
        false -> undefined
    end;

allowed_origin(onezone) ->
    case service:exists(?SERVICE_OZW) of
        true -> <<"https://", (service_oz_worker:get_domain())/binary>>;
        false -> undefined
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns suffix to be appended to each node's "hostname" field
%% to create full node hostname.
%% @end
%%--------------------------------------------------------------------
-spec common_hostname_suffix(Args :: rest_handler:args()) -> binary().
common_hostname_suffix(Args) ->
    case nested:get([cluster, domainName], Args, <<>>) of
        <<>> -> <<>>;
        DomainName -> <<".", DomainName/binary>>
    end.
