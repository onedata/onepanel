%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module mirrors functions from oz_worker's 'rpc_api' module,
%%% providing rpc access to them. Most functions come in two variants:
%%% one defaulting to any oz_worker node and other with explicit Node argument.
%%% Functions in this module throw an error upon receiving badrpc.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker_rpc).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("names.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/onedata.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

% @formatter:off
-define(NODE, element(2, {ok, _} = nodes:any(?SERVICE_OZW))).
-define(CALL(Node, Args),
    case rpc:call(Node, rpc_api, apply, [?FUNCTION_NAME, Args]) of
        {badrpc, nodedown} = __Error -> throw(?ERROR_SERVICE_UNAVAILABLE);
        {badrpc, _} = __Error -> error(__Error);
        __Result -> __Result
    end).

-define(CALL(Args), begin
        ?CALL(?NODE, Args)
    end).
% @formatter:on


-type basic_auth_password() :: binary().
-type password_hash() :: binary().
-type od_cluster_id() :: binary().
-type od_cluster_version_info() :: {
    Release :: onedata:release_version(),
    Build :: binary(),
    GuiHash :: onedata:gui_hash()
}.
-type od_group_id() :: binary().
-type gui_message_id() :: binary().
-type gui_message_map_repr() :: #{enabled := boolean(), body := binary()}.
-type od_provider_id() :: binary().
-type od_user_id() :: binary().
-type od_user_username() :: binary().

-export_type([gui_message_id/0, gui_message_map_repr/0]).

-export([authenticate_by_token/2, authenticate_by_token/3]).
-export([get_protected_provider_data/2, get_protected_provider_data/3]).
-export([deploy_static_gui_package/4, deploy_static_gui_package/5]).
-export([update_cluster_version_info/4, update_cluster_version_info/5]).
-export([set_user_password/3, set_user_password/4]).
-export([create_user/2, create_user/3]).
-export([add_user_to_group/3, add_user_to_group/4]).
-export([list_users/1, list_users/2]).
-export([user_exists/1, user_exists/2]).
-export([username_exists/1, username_exists/2]).
-export([get_user_details/1]).
-export([get_user_details/2]).
-export([migrate_onepanel_user_to_onezone/4, migrate_onepanel_user_to_onezone/5]).
-export([cluster_get_eff_user_privileges/3, cluster_get_eff_user_privileges/4]).
-export([get_protected_cluster_data/2, get_protected_cluster_data/3]).
-export([get_eff_clusters_by_user_auth/1, get_eff_clusters_by_user_auth/2]).
-export([cluster_logic_get_users/2, cluster_logic_get_eff_users/2]).
-export([cluster_logic_get_groups/2, cluster_logic_get_eff_groups/2]).
-export([cluster_logic_create_invite_token_to_onezone_for_admin/0]).
-export([reconcile_dns_config/0, reconcile_dns_config/1]).
-export([dns_config_get_ns_hosts/0, dns_config_get_ns_hosts/1]).
-export([gui_message_exists/1, gui_message_exists/2]).
-export([get_gui_message_as_map/1, get_gui_message_as_map/2]).
-export([update_gui_message/3, update_gui_message/4]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec authenticate_by_token(tokens:serialized() | tokens:token(),
    undefined | ip_utils:ip()) ->
    {true, aai:auth()} | {error, term()}.
authenticate_by_token(Token, PeerIp)  ->
    authenticate_by_token(?NODE, Token, PeerIp).

-spec authenticate_by_token(node(), tokens:serialized() | tokens:token(),
    undefined | ip_utils:ip()) ->
    {true, aai:auth()} | {error, term()}.
authenticate_by_token(Node, Token, PeerIp)  ->
    AuthCtx = #auth_ctx{
        % @TODO VFS-5913 Fully recognize peerIp, interface and consumerToken
        % auth ctx in onepanel
        ip = PeerIp,
        service = ?SERVICE(?OZ_PANEL, ?ONEZONE_CLUSTER_ID),
        % accept any session - only oz-worker requires a proper session cookie
        % as it hosts the actual GUI
        session_id = any
    },
    ?CALL(Node, [Token, AuthCtx]).


-spec get_protected_provider_data(aai:auth(), od_provider_id()) ->
    {ok, map()} | errors:error().
get_protected_provider_data(Auth, ProviderId) ->
    ?CALL([Auth, ProviderId]).

-spec get_protected_provider_data(node(), aai:auth(), od_provider_id()) ->
    {ok, map()} | errors:error().
get_protected_provider_data(Node, Auth, ProviderId) ->
    ?CALL(Node, [Auth, ProviderId]).


-spec deploy_static_gui_package(onedata:gui(), onedata:release_version(),
    file:name_all(), VerifyGuiHash :: boolean()) ->
    {ok, onedata:gui_hash()} | ?ERROR_BAD_GUI_PACKAGE |
    ?ERROR_GUI_PACKAGE_TOO_LARGE | ?ERROR_GUI_PACKAGE_UNVERIFIED(onedata:gui_hash()).
deploy_static_gui_package(GuiType, ReleaseVsn, PackagePath, VerifyGuiHash) ->
    ?CALL([GuiType, ReleaseVsn, PackagePath, VerifyGuiHash]).

-spec deploy_static_gui_package(node(), onedata:gui(), onedata:release_version(),
    file:name_all(), VerifyGuiHash :: boolean()) ->
    {ok, onedata:gui_hash()} | ?ERROR_BAD_GUI_PACKAGE |
    ?ERROR_GUI_PACKAGE_TOO_LARGE | ?ERROR_GUI_PACKAGE_UNVERIFIED(onedata:gui_hash()).
deploy_static_gui_package(Node, GuiType, ReleaseVsn, PackagePath, VerifyGuiHash) ->
    ?CALL(Node, [GuiType, ReleaseVsn, PackagePath, VerifyGuiHash]).


-spec update_cluster_version_info(aai:auth(), od_cluster_id(),
    onedata:service_type(), od_cluster_version_info()) ->
    ok | {error, term()}.
update_cluster_version_info(Auth, ClusterId, ServiceType, VersionInfo) ->
    ?CALL([Auth, ClusterId, ServiceType, VersionInfo]).

-spec update_cluster_version_info(node(), Auth :: aai:auth(),
    ClusterId :: od_cluster_id(), onedata:service_type(),
    VersionInfo :: od_cluster_version_info()) ->
    ok | {error, term()}.
update_cluster_version_info(Node, Auth, ClusterId, ServiceType, VersionInfo) ->
    ?CALL(Node, [Auth, ClusterId, ServiceType, VersionInfo]).


-spec set_user_password(aai:auth(), od_user_id(), basic_auth_password()) ->
    ok | {error, term()}.
set_user_password(Auth, UserId, NewPassword) ->
    ?CALL([Auth, UserId, NewPassword]).

-spec set_user_password(node(), aai:auth(), od_user_id(), basic_auth_password()) ->
    ok | {error, term()}.
set_user_password(Node, Auth, UserId, NewPassword) ->
    ?CALL(Node, [Auth, UserId, NewPassword]).

-spec create_user(aai:auth(), Data :: map()) ->
    {ok, od_user_id()} | errors:error().
create_user(Auth, Data) ->
    ?CALL([Auth, Data]).

-spec create_user(node(), aai:auth(), Data :: map()) ->
    {ok, od_user_id()} | errors:error().
create_user(Node, Auth, Data) ->
    ?CALL(Node, [Auth, Data]).


-spec add_user_to_group(aai:auth(), od_group_id(), od_user_id()) ->
    {ok, od_user_id()} | {error, term()}.
add_user_to_group(Auth, GroupId, UserId) ->
    ?CALL([Auth, GroupId, UserId]).

-spec add_user_to_group(node(), aai:auth(), od_group_id(), od_user_id()) ->
    {ok, od_user_id()} | {error, term()}.
add_user_to_group(Node, Auth, GroupId, UserId) ->
    ?CALL(Node, [Auth, GroupId, UserId]).


-spec list_users(aai:auth()) -> {ok, [od_user_id()]} | {error, term()}.
list_users(Auth) ->
    ?CALL([Auth]).

-spec list_users(node(), aai:auth()) -> {ok, [od_user_id()]} | {error, term()}.
list_users(Node, Auth) ->
    ?CALL(Node, [Auth]).


-spec user_exists(od_user_id()) -> boolean().
user_exists(UserId) ->
    ?CALL([UserId]).

-spec user_exists(node(), od_user_id()) -> boolean().
user_exists(Node, UserId) ->
    ?CALL(Node, [UserId]).


-spec username_exists(od_user_username()) -> boolean().
username_exists(Username) ->
    ?CALL([Username]).

-spec username_exists(node(), od_user_username()) -> boolean().
username_exists(Node, Username) ->
    ?CALL(Node, [Username]).


-spec get_user_details(aai:auth()) ->
    {ok, #user_details{}} | {error, term()}.
get_user_details(Auth) ->
    ?CALL([Auth]).

-spec get_user_details(aai:auth(), od_user_id()) ->
    {ok, #user_details{}} | {error, term()}.
get_user_details(Auth, UserId) ->
    ?CALL([Auth, UserId]).


-spec migrate_onepanel_user_to_onezone(OnepanelUserId :: binary(),
    OnepanelUsername :: binary(), password_hash(), Role :: regular | admin) ->
    {ok, od_user_id()}.
migrate_onepanel_user_to_onezone(OnepanelUserId, OnepanelUsername, PasswordHash, Role) ->
    ?CALL([OnepanelUserId, OnepanelUsername, PasswordHash, Role]).

-spec migrate_onepanel_user_to_onezone(node(), OnepanelUserId :: binary(),
    OnepanelUsername :: binary(), password_hash(), Role :: regular | admin) ->
    {ok, od_user_id()}.
migrate_onepanel_user_to_onezone(Node, OnepanelUserId, OnepanelUsername, PasswordHash, Role) ->
    ?CALL(Node, [OnepanelUserId, OnepanelUsername, PasswordHash, Role]).


-spec cluster_get_eff_user_privileges(aai:auth(), od_cluster_id(),
    od_user_id()) -> {ok, [privileges:cluster_privilege()]} | {error, term()}.
cluster_get_eff_user_privileges(Auth, ClusterId, UserId) ->
    ?CALL([Auth, ClusterId, UserId]).

-spec cluster_get_eff_user_privileges(node(), aai:auth(), od_cluster_id(),
    od_user_id()) -> {ok, [privileges:cluster_privilege()]} | {error, term()}.
cluster_get_eff_user_privileges(Node, Auth, ClusterId, UserId) ->
    ?CALL(Node, [Auth, ClusterId, UserId]).


-spec get_protected_cluster_data(aai:auth(), od_cluster_id()) ->
    {ok, map()} | {error, term()}.
get_protected_cluster_data(Auth, ClusterId) ->
    ?CALL([Auth, ClusterId]).

-spec get_protected_cluster_data(node(), aai:auth(), od_cluster_id()) ->
    {ok, map()} | {error, term()}.
get_protected_cluster_data(Node, Auth, ClusterId) ->
    ?CALL(Node, [Auth, ClusterId]).


-spec get_eff_clusters_by_user_auth(aai:auth()) -> {ok, [od_cluster_id()]} | errors:error().
get_eff_clusters_by_user_auth(Auth) ->
    ?CALL([Auth]).

-spec get_eff_clusters_by_user_auth(node(), aai:auth()) ->
    {ok, [od_cluster_id()]} | {error, term()}.
get_eff_clusters_by_user_auth(Node, Auth) ->
    ?CALL(Node, [Auth]).


-spec cluster_logic_get_users(aai:auth(), od_cluster_id()) ->
    {ok, [od_user_id()]} | {error, term()}.
cluster_logic_get_users(Auth, ClusterId) ->
    ?CALL([Auth, ClusterId]).


-spec cluster_logic_get_eff_users(aai:auth(), od_cluster_id()) ->
    {ok, [od_user_id()]} | {error, term()}.
cluster_logic_get_eff_users(Auth, ClusterId) ->
    ?CALL([Auth, ClusterId]).


-spec cluster_logic_get_groups(aai:auth(), od_cluster_id()) ->
    {ok, [od_group_id()]} | {error, term()}.
cluster_logic_get_groups(Auth, ClusterId) ->
    ?CALL([Auth, ClusterId]).


-spec cluster_logic_get_eff_groups(aai:auth(), od_cluster_id()) ->
    {ok, [od_group_id()]} | {error, term()}.
cluster_logic_get_eff_groups(Auth, ClusterId) ->
    ?CALL([Auth, ClusterId]).


% @TODO VFS-9647 Support direct addition of arbitrary users to Onezone
% cluster when Entity management GUI in Onezone panel is implemented
% (this mechanism will be then obsolete)
-spec cluster_logic_create_invite_token_to_onezone_for_admin() ->
    {ok, tokens:token()} | {error, term()}.
cluster_logic_create_invite_token_to_onezone_for_admin() ->
    ?CALL([]).


-spec reconcile_dns_config() -> ok.
reconcile_dns_config() ->
    ?CALL([]).

-spec reconcile_dns_config(node()) -> ok.
reconcile_dns_config(Node) ->
    ?CALL(Node, []).


-spec dns_config_get_ns_hosts() ->
    [{Name :: binary(), IP :: inet:ip4_address()}].
dns_config_get_ns_hosts() ->
    ?CALL([]).

-spec dns_config_get_ns_hosts(node()) ->
    [{Name :: binary(), IP :: inet:ip4_address()}].
dns_config_get_ns_hosts(Node) ->
    ?CALL(Node, []).


-spec gui_message_exists(gui_message_id()) -> boolean().
gui_message_exists(MessageId) ->
    ?CALL([MessageId]).

-spec gui_message_exists(node(), gui_message_id()) -> boolean().
gui_message_exists(Node, MessageId) ->
    ?CALL(Node, [MessageId]).


-spec get_gui_message_as_map(gui_message_id()) ->
    {ok, gui_message_map_repr()} | {error, term()}.
get_gui_message_as_map(MessageId) ->
    ?CALL([MessageId]).

-spec get_gui_message_as_map(node(), gui_message_id()) ->
    {ok, gui_message_map_repr()} | {error, term()}.
get_gui_message_as_map(Node, MessageId) ->
    ?CALL(Node, [MessageId]).


-spec update_gui_message(aai:auth(), gui_message_id(), Data :: map()) ->
    ok | {error, term()}.
update_gui_message(Auth, MessageId, Data) ->
    ?CALL([Auth, MessageId, Data]).

-spec update_gui_message(node(), aai:auth(), gui_message_id(), Data :: map()) ->
    ok | {error, term()}.
update_gui_message(Node, Auth, MessageId, Data) ->
    ?CALL(Node, [Auth, MessageId, Data]).
