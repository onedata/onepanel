%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for fetching information about remote Onezone
%%% and Oneprovider clusters.
%%% @end
%%%--------------------------------------------------------------------
-module(remotes).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([fetch_zone_info/1, fetch_provider_info/2]).
-export([root_auth/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns publicly available information about a Onezone.
%% @end
%%--------------------------------------------------------------------
-spec fetch_zone_info(Domain :: binary()) -> #{atom() := atom() | binary()}.
fetch_zone_info(Domain) ->
    Url = configuration_url(Domain),
    CaCerts = cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir()),
    Opts = [{ssl_options, [{secure, true}, {cacerts, CaCerts}]}],
    case http_client:get(Url, #{}, <<>>, Opts) of
        {ok, _, _, Response} ->
            Map = onepanel_utils:convert(json_utils:decode(Response), {keys, atom}),
            #{version := OzVersion, compatibleOneproviderVersions := CompatOps} = Map,
            onepanel_maps:get_store_multiple([
                {version, version},
                {name, name},
                {subdomainDelegationSupported, subdomainDelegationSupported}
            ], Map, #{
                online => true,
                domain => Domain,
                compatible => is_compatible(OzVersion, CompatOps)
            });
        {error, _Reason} ->
            #{
                online => false,
                domain => Domain
            }
    end.


%%--------------------------------------------------------------------
%% @doc Fetch information about a provider. User must belong to its
%% cluster.
%% @end
%%--------------------------------------------------------------------
-spec fetch_provider_info(Auth :: rest_handler:zone_auth(), ProviderId :: binary()) ->
    #{binary() := term()}.
fetch_provider_info({rpc, Client}, ProviderId) ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {ok, ProviderData} = rpc:call(
        OzNode, provider_logic, get_protected_data, [Client, ProviderId]
    ),
    format_provider_info(ProviderData);

fetch_provider_info({rest, RestAuth}, ProviderId) ->
    URN = "/providers/" ++ binary_to_list(ProviderId),
    {ok, 200, _, BodyJson} = oz_endpoint:request(RestAuth, URN, get),
    format_provider_info(json_utils:decode(BodyJson)).


%%--------------------------------------------------------------------
%% @doc Auth term describing the root user.
%% @end
%%--------------------------------------------------------------------
-spec root_auth() -> rest_handler:zone_auth().
root_auth() ->
    case onepanel_env:get_release_type() of
        onezone ->
            case service_oz_worker:get_logic_client(root) of
                {ok, Client} -> {rpc, Client};
                Error -> ?throw_error(Error)
            end;
        oneprovider -> {rest, provider}
    end.


%%%===================================================================
%%% Private functions
%%%===================================================================

%% @private
-spec is_compatible(OzVersion :: binary(), CompatOpVersions :: [binary()]) ->
    boolean().
is_compatible(OzVersion, CompatOpVersions) ->
    {_, OurRelease} = onepanel_app:get_build_and_version(),
    CompatOzVersions = service_op_worker:get_compatible_onezones(),

    lists:member(OzVersion, CompatOzVersions)
        orelse lists:member(OurRelease, CompatOpVersions).


%% @private
-spec configuration_url(Domain :: binary() | string()) -> binary().
configuration_url(Domain) ->
    str_utils:format_bin("https://~s~s",
        [Domain, onepanel_env:get(onezone_configuration_uri)]).


%% @private
-spec format_provider_info(OzResponse :: #{binary() => term()}) ->
    #{binary() => term()}.
format_provider_info(OzResponse) ->
    onepanel_maps:get_store_multiple([
        {<<"providerId">>, <<"id">>},
        {<<"name">>, <<"name">>},
        {<<"domain">>, <<"domain">>},
        {<<"longitude">>, <<"geoLongitude">>},
        {<<"latitude">>, <<"geoLatitude">>},
        {<<"cluster">>, <<"cluster">>},
        {<<"online">>, <<"online">>}
    ], OzResponse).
