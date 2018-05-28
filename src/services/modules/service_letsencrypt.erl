%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module handles management of Let's Encrypt https certificates
%%% including obtaining certificates and periodic renewal.
%%% @end
%%%--------------------------------------------------------------------
-module(service_letsencrypt).
-author("Wojciech Geisler").

-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("names.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, status/1, check_webcert/1, ensure_webcert/1,
    update/1, is_enabled/0]).
-export([is_local_cert_dirty/1]).

-define(CERT_PATH, onepanel_env:get(web_cert_file)).
-define(KEY_PATH, onepanel_env:get(web_key_file)).
-define(CACERT_PATH, onepanel_env:get(web_cacerts_dir)).

-define(CHECK_DELAY, timer:seconds(application:get_env(
    ?APP_NAME, web_cert_renewal_check_delay, 3600))).
-define(RENEW_MARGIN_SECONDS, application:get_env(
    ?APP_NAME, web_cert_renewal_days, 7) * 24 * 3600).
-define(CERTIFICATION_ATTEMPTS, application:get_env(
    ?APP_NAME, letsencrypt_attempts, 1)).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    letsencrypt.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    onepanel_cluster:hosts_to_nodes(service_onepanel:name(), get_hosts()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{letsencrypt_plugin := _} = Ctx) ->
    create(Ctx),
    [#step{function = configure}];

get_steps(update, _Ctx) ->
    IsEnabled = fun
        (#{letsencrypt_enabled := Enabled}) -> Enabled;
        (_) -> is_enabled()
    end,
    [
        % use "first" host to launch service watcher monitoring on one node only
        #step{function = update, selection = first},
        #step{function = check_webcert, selection = first, condition = IsEnabled},
        #step{function = ensure_webcert, selection = first, condition = IsEnabled,
            attempts = ?CERTIFICATION_ATTEMPTS}
    ].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Adds current host to service's hosts.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(_Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc
%% Changes letsencrypt client state between disabled and enabled.
%% Enabling starts periodic checks of the installed certificate.
%% @end
%%--------------------------------------------------------------------
-spec update(service:ctx()) -> ok | no_return().
update(#{letsencrypt_enabled := Request} = Ctx) ->
    case {Request, (get_plugin_module()):is_letsencrypt_supported(Ctx)} of
        {true, true} -> enable();
        {true, _} -> % unsuppoted or support cannot be determined
            disable(),
            ?throw_error(?ERR_LETSENCRYPT_NOT_SUPPORTED);
        {false, _} -> disable()
    end;
update(Ctx) ->
    % even when letsencrypt_enabled is not present in Ctx
    % ensure current state is valid (for example if provider is still registered)
    case {is_enabled(), (get_plugin_module()):is_letsencrypt_supported(Ctx)} of
        {true, true} -> enable(); % to register in service watcher
        {true, false} -> disable();
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running.
status(Ctx) ->
    try
        check_webcert(Ctx),
        ensure_webcert(Ctx#{renewal => true})
    catch
        _:Reason -> ?error("Certificate renewal check failed: ~p", [?make_error(Reason)])
    end,
    % always returns 'running' as there is no meaningful way of restarting this service
    running.


%%--------------------------------------------------------------------
%% @doc
%% Sets is_cert_dirty flag if certificate needs renewal.
%% @end
%%--------------------------------------------------------------------
check_webcert(Ctx) ->
    case is_enabled() of
        true ->
            Nodes = get_nodes(),
            Domain = (get_plugin_module()):get_domain(Ctx),
            Ctx2 = Ctx#{domain => Domain},

            % check all nodes to ensure consistency
            NodeCerts =
                lists:map(fun(Node) ->
                    rpc:call(Node, ?MODULE, is_local_cert_dirty, [Ctx2]) end,
                Nodes),

            case lists:any(fun(IsDirty) -> IsDirty end, NodeCerts) of
                true -> set_dirty(true);
                false -> ok
            end;
        false -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Obtains certificate if current is marked as dirty.
%% @end
%%--------------------------------------------------------------------
ensure_webcert(Ctx) ->
    case service:get(name()) of
        {ok, #service{ctx = #{is_cert_dirty := true, letsencrypt_enabled := true}}} ->
            obtain_cert(Ctx);
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Determines whether Let's Encrypt certificate renewal is enabled.
%% @end
%%--------------------------------------------------------------------
is_enabled() ->
    case service:get(name()) of
        {ok, #service{ctx = #{letsencrypt_enabled := true}}} -> true;
        _ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Obtains certificate from Let's Encrypt
%% @end
%%--------------------------------------------------------------------
-spec obtain_cert(service:ctx()) -> ok | no_return().
obtain_cert(Ctx) ->
    Plugin = get_plugin_module(),
    Domain = Plugin:get_domain(Ctx),

    case maps:get(renewal, Ctx, false) of
        false ->
            onepanel_ssl:backup_exisiting_certs();
        true ->
            ?info("Renewing Let's Encrypt certificate"),
            ok
    end,

    ok = letsencrypt_api:run_certification_flow(Domain, get_plugin_module()),
    set_dirty(false),

    service:apply_sync(service_onepanel:name(), reload_webcert, #{}),
    service:apply_sync(get_plugin_name(), reload_webcert, #{}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates default service model, preserving existing configuration
%% from older oneprovider versions.
%% @end
%%--------------------------------------------------------------------
create(#{letsencrypt_plugin := Plugin}) ->
    LegacyEnabled = service_oneprovider:pop_legacy_letsencrypt_config(),
    service:create(#service{name = name(), ctx = #{
        letsencrypt_plugin => Plugin,
        letsencrypt_enabled => LegacyEnabled,
        % is_cert_dirty marks certificate as needing renewal.
        % Later can be set to 'false' only by successfully obtaining certificate
        is_cert_dirty => not(LegacyEnabled)
    }}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Changes client state from disabled to enabled.
%% @end
%%--------------------------------------------------------------------
-spec enable() -> ok.
enable() ->
    ok = service_watcher:register_service(name(), ?CHECK_DELAY),
    service:update(name(), fun(#service{ctx = ServiceCtx} = Record) ->
        Enabling = not(maps:get(letsencrypt_enabled, ServiceCtx, false)),
        
        Record#service{ctx = ServiceCtx#{
            letsencrypt_enabled => true,
            is_cert_dirty => Enabling
        }}
    end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures certificate renewal is disabled.
%% @end
%%--------------------------------------------------------------------
-spec disable() -> ok.
disable() ->
    letsencrypt_api:clean_keys(),
    ok = service_watcher:unregister_service(name()),
    service:update(name(), fun(#service{ctx = ServiceCtx} = Record) ->
        Record#service{ctx = ServiceCtx#{letsencrypt_enabled => false}}
    end).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets 'is_cert_dirty' state in the service model.
%% @end
%%--------------------------------------------------------------------
-spec set_dirty(boolean()) -> ok | no_return().
set_dirty(IsDirty) ->
    service:update(name(), fun(#service{ctx = ServiceCtx} = Record) ->
        Record#service{ctx = ServiceCtx#{is_cert_dirty => IsDirty}}
    end).


%%--------------------------------------------------------------------
%% @doc
%% Checks if certificate on the current node needs renewal.
%% @end
%%--------------------------------------------------------------------
-spec is_local_cert_dirty(service:ctx()) -> boolean().
is_local_cert_dirty(#{domain := Domain} = Ctx) ->
    case onepanel_ssl:read_cert(?CERT_PATH) of
        {ok, Cert} ->
            Domain /= onepanel_ssl:get_subject_cn(Cert)
            orelse
            (onepanel_ssl:get_seconds_till_expiration(Cert) < ?RENEW_MARGIN_SECONDS);
        {error, _} -> true
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns plugin service name.
%% @end
%%--------------------------------------------------------------------
-spec get_plugin_name() -> service:name().
get_plugin_name() ->
    {ok, #service{ctx = #{letsencrypt_plugin := Plugin}}} = service:get(name()),
    Plugin.


%%--------------------------------------------------------------------
%% @doc
%% Returns module corresponding to the plugin service.
%% @end
%%--------------------------------------------------------------------
-spec get_plugin_module() -> module().
get_plugin_module() ->
    service:get_module(get_plugin_name()).
