%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for interacting with the Onezone.
%%% @end
%%%--------------------------------------------------------------------
-module(onezone_client).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/aai/aai.hrl").

%% API
-export([fetch_zone_info/1, register_provider/2]).
-export([root_auth/0]).

-define(OPTS, [{ssl_options, [
    {cacerts, cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir())}
]}]).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns publicly available information about a Onezone.
%% @end
%%--------------------------------------------------------------------
-spec fetch_zone_info(Domain :: binary()) -> #{binary() := atom() | binary()}.
fetch_zone_info(Domain) ->
    Url = configuration_url(Domain),
    case http_client:get(Url, #{}, <<>>, ?OPTS) of
        {ok, ?HTTP_200_OK, _, Response} ->
            ResponseMap = json_utils:decode(Response),
            SelectedFields = maps:with([
                <<"version">>, <<"name">>, <<"subdomainDelegationSupported">>
            ], ResponseMap),
            SelectedFields#{
                <<"online">> => true,
                <<"domain">> => Domain,
                <<"compatible">> => is_compatible(Domain, ResponseMap)
            };
        _ ->
            #{
                <<"online">> => false,
                <<"domain">> => Domain
            }
    end.


-spec register_provider(OnezoneDomain :: binary(), Payload :: json_utils:json_map()) ->
    {ok, #{provider_id := binary(), root_token := binary()}} | errors:error().
register_provider(OnezoneDomain, Payload) ->
    % Not using oz_providers:register/2 since
    % oz_plugin does not know onezone_domain before registration
    URL = onepanel_utils:join([
        "https://", OnezoneDomain, oz_plugin:get_oz_rest_api_prefix(),
        "/providers"
    ]),
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
    Body = json_utils:encode(Payload),
    case http_client:post(URL, Headers, Body, ?OPTS) of
        {ok, _Code, _ResponseHeaders, ResponseBody} ->
            case json_utils:decode(ResponseBody) of
                #{<<"error">> := Error} ->
                    errors:from_json(Error);
                #{<<"providerId">> := Id, <<"providerRootToken">> := RootToken} ->
                    {ok, #{provider_id => Id, root_token => RootToken}}
            end;
        {error, _} ->
            throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
    end.


%%--------------------------------------------------------------------
%% @doc Auth term describing the root user.
%% @end
%%--------------------------------------------------------------------
-spec root_auth() -> rest_handler:zone_credentials().
root_auth() ->
    case onepanel_env:get_cluster_type() of
        onezone -> {rpc, ?ROOT};
        oneprovider -> {rest, provider}
    end.


%%%===================================================================
%%% Private functions
%%%===================================================================

%% @private
-spec is_compatible(Domain :: binary(), json_utils:json_term()) ->
    boolean().
is_compatible(Domain, #{<<"version">> := OzVersion} = OzConfiguration) ->
    CompatOpVersions = utils:ensure_list(maps:get(<<"compatibleOneproviderVersions">>, OzConfiguration, <<"unknown">>)),

    CompatOzVersions = get_compatible_onezone_versions(Domain, OzConfiguration),

    OpVersion = service_op_worker:get_version(),
    lists:member(OzVersion, CompatOzVersions)
        orelse lists:member(OpVersion, CompatOpVersions).


%% @private
-spec get_compatible_onezone_versions(Domain :: binary(), json_utils:json_term()) ->
    [onedata:release_version()].
get_compatible_onezone_versions(Domain, OzConfiguration) ->
    TrustedCaCerts = cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir()),
    Resolver = compatibility:build_resolver(service_onepanel:get_nodes(), TrustedCaCerts),
    check_for_compatibility_registry_updates(Domain, Resolver, OzConfiguration),

    OpVersion = service_op_worker:get_version(),
    {ok, Versions} = compatibility:get_compatible_versions(Resolver, ?ONEPROVIDER, OpVersion, ?ONEZONE),
    Versions.


%% @private
-spec check_for_compatibility_registry_updates(Domain :: binary(), compatibility:resolver(), json_utils:json_term()) ->
    ok.
check_for_compatibility_registry_updates(Domain, Resolver, OzConfiguration) ->
    case maps:get(<<"compatibilityRegistryRevision">>, OzConfiguration, <<"unknown">>) of
        RemoteRevision when is_integer(RemoteRevision) ->
            LocalRevision = case compatibility:peek_current_registry_revision(Resolver) of
                {ok, R} -> R;
                _ -> 0
            end,
            case RemoteRevision > LocalRevision of
                true ->
                    compatibility:check_for_updates(Resolver, [compatibility_registry_url(Domain)]);
                false ->
                    ?debug(
                        "Local compatibility registry (v. ~ts) is not older than Onezone's (v. ~ts)",
                        [LocalRevision, RemoteRevision]
                    )
            end;
        Other ->
            utils:throttle(timer:minutes(30), fun() ->
                ?warning("Cannot check Onezone's compatibility registry revision - got '~w'", [Other])
            end)
    end.


%% @private
-spec configuration_url(Domain :: binary()) -> binary().
configuration_url(Domain) ->
    str_utils:format_bin("https://~ts~ts", [Domain, onepanel_env:get(onezone_configuration_urn)]).


%% @private
-spec compatibility_registry_url(Domain :: binary()) -> binary().
compatibility_registry_url(Domain) ->
    str_utils:format_bin("https://~ts~ts", [Domain, onepanel_env:get(onezone_compatibility_registry_urn)]).
