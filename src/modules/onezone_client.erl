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
-include_lib("ctool/include/aai/aai.hrl").

%% API
-export([fetch_zone_info/1]).
-export([root_auth/0]).

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
    CaCerts = cert_utils:load_ders_in_dir(oz_plugin:get_cacerts_dir()),
    Opts = [{ssl_options, [{cacerts, CaCerts}]}],
    case http_client:get(Url, #{}, <<>>, Opts) of
        {ok, ?HTTP_200_OK, _, Response} ->
            #{
                <<"version">> := OzVersion,
                <<"compatibleOneproviderVersions">> := CompatOps
            } = ResponseMap = json_utils:decode(Response),
            SelectedFields = maps:with([
                <<"version">>, <<"name">>, <<"subdomainDelegationSupported">>
            ], ResponseMap),
            SelectedFields#{
                <<"online">> => true,
                <<"domain">> => Domain,
                <<"compatible">> => is_compatible(OzVersion, CompatOps)
            };
        {ok, _, _, _} ->
            #{
                <<"online">> => false
            };
        {error, _Reason} ->
            #{
                <<"online">> => false,
                <<"domain">> => Domain
            }
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
-spec is_compatible(OzVersion :: binary(), CompatOpVersions :: [binary()]) ->
    boolean().
is_compatible(OzVersion, CompatOpVersions) ->
    {_, OurVersion} = onepanel:get_build_and_version(),
    CompatOzVersions = service_op_worker:get_compatible_onezones(),

    lists:member(OzVersion, CompatOzVersions)
        orelse lists:member(OurVersion, CompatOpVersions).


%% @private
-spec configuration_url(Domain :: binary() | string()) -> binary().
configuration_url(Domain) ->
    str_utils:format_bin("https://~s~s",
        [Domain, onepanel_env:get(onezone_configuration_urn)]).
