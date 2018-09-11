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
-include("deployment_progress.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([create/1, status/1, check_webcert/1,
    enable/1, disable/1, get_details/1]).

%% Private function exported for rpc
-export([local_cert_status/1, is_local_cert_letsencrypt/0]).

-define(CERT_PATH, onepanel_env:get(web_cert_file)).
-define(KEY_PATH, onepanel_env:get(web_key_file)).
-define(CHAIN_PATH, onepanel_env:get(web_cert_chain_file)).

-define(CHECK_DELAY, timer:seconds(application:get_env(
    ?APP_NAME, web_cert_renewal_check_delay, 3600))).
-define(RENEW_MARGIN_SECONDS, application:get_env(
    ?APP_NAME, web_cert_renewal_days, 7) * 24 * 3600).
-define(CERTIFICATION_ATTEMPTS, application:get_env(
    ?APP_NAME, letsencrypt_attempts, 1)).

-type status() :: regenerating | valid | near_expiration
| expired | domain_mismatch | unknown.

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
    service_onepanel:get_hosts().


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    service_onepanel:get_nodes().


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{letsencrypt_plugin := _} = _Ctx) ->
    [#step{function = create, selection = first}];

get_steps(resume, _Ctx) ->
    % failsafe against unlikely case of failing to reset the "regenerating" flag
    update_ctx(#{regenerating => false}),

    % ensure service is added to service_watcher if necessary
    [#steps{action = update}];

get_steps(update, Ctx) ->
    mark_configured(Ctx),
    IsEnabled = fun is_enabled/1,
    IsDisabled = fun(FunCtx) -> not is_enabled(FunCtx) end,
    [
        % always use "first" host to launch service watcher on one node only
        #step{function = check_webcert, selection = first, condition = IsEnabled,
            attempts = ?CERTIFICATION_ATTEMPTS},

        #step{function = enable, selection = first, condition = IsEnabled},
        #step{function = disable, selection = first, condition = IsDisabled}
    ];

get_steps(get_details, _Ctx) ->
    [#step{function = get_details, selection = first}].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates default service model, preserving existing configuration
%% from older oneprovider versions.
%% @end
%%--------------------------------------------------------------------
-spec create(service:ctx()) -> ok.
create(#{letsencrypt_plugin := Plugin}) ->
    LegacyEnabled = service_oneprovider:pop_legacy_letsencrypt_config(),
    case service:create(#service{name = name(),
        ctx = #{
            letsencrypt_plugin => Plugin,
            letsencrypt_enabled => LegacyEnabled
        }}) of
        {ok, _} -> ok;
        #error{reason = ?ERR_ALREADY_EXISTS} -> ok
    end.


%%--------------------------------------------------------------------
%% @doc
%% Callback needed for service_watcher.
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> healthy.
status(Ctx) ->
    try
        check_webcert(Ctx#{renewal => true})
    catch
        _:Reason -> ?error("Certificate renewal check failed: ~p", [?make_stacktrace(Reason)])
    end,
    % always returns 'healthy' as there is no meaningful way of restarting this service
    healthy.


%%--------------------------------------------------------------------
%% @doc
%% Obtains certificate if current is marked as dirty.
%% @end
%%--------------------------------------------------------------------
check_webcert(Ctx) ->
    case should_obtain(Ctx) of
        true ->
            update_ctx(#{regenerating => true}),
            try
                obtain_cert(Ctx)
            catch _:Error ->
                update_ctx(#{
                    regenerating => false,
                    last_failure => time_utils:system_time_seconds()
                }),
                % stores original stacktrace
                ?throw_stacktrace(Error)
            end,

            update_ctx(#{
                regenerating => false,
                last_success => time_utils:system_time_seconds()
            });
        false -> ok
    end.


get_details(_Ctx) ->
    {ok, #service{ctx = Ctx}} = service:get(name()),
    Enabled = is_enabled(Ctx),
    Status = global_cert_status(Ctx),

    {ok, Cert} = onepanel_cert:read(?CERT_PATH),
    {Since, Until} = onepanel_cert:get_times(Cert),
    Domain = onepanel_cert:get_subject_cn(Cert),
    Issuer = onepanel_cert:get_issuer_cn(Cert),

    Optional = case Enabled of
        true -> #{
            lastRenewalSuccess => date_or_null(last_success, Ctx),
            lastRenewalFailure => date_or_null(last_failure, Ctx)
        };
        false -> #{}
    end,


    Optional#{
        letsEncrypt => is_enabled(#{}),
        creationTime => time_utils:epoch_to_iso8601(Since),
        expirationTime => time_utils:epoch_to_iso8601(Until),
        paths => #{
            cert => filename:absname(onepanel_utils:convert(?CERT_PATH, binary)),
            key => filename:absname(onepanel_utils:convert(?KEY_PATH, binary)),
            chain => filename:absname(onepanel_utils:convert(?CHAIN_PATH, binary))
        },
        domain => Domain,
        issuer => Issuer,
        status => Status
    }.


%%--------------------------------------------------------------------
%% @doc
%% Determines whether Let's Encrypt certificate renewal is enabled.
%% @end
%%--------------------------------------------------------------------
is_enabled(#{letsencrypt_enabled := Enabled}) -> Enabled;
is_enabled(_Ctx) ->
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
        false -> onepanel_cert:backup_exisiting_certs();
        true ->
            ?info("Renewing Let's Encrypt certificate"),
            ok
    end,

    ok = letsencrypt_api:run_certification_flow(Domain, get_plugin_module()),

    service:apply_sync(service_onepanel:name(), reload_webcert, #{}),
    service:apply_sync(get_plugin_name(), reload_webcert, #{}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Changes client state from disabled to enabled.
%% @end
%%--------------------------------------------------------------------
-spec enable(service:ctx()) -> ok.
enable(_Ctx) ->
    ok = service_watcher:register_service(name(), ?CHECK_DELAY),
    update_ctx(#{letsencrypt_enabled => true}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures certificate renewal is disabled.
%% @end
%%--------------------------------------------------------------------
-spec disable(service:ctx()) -> ok.
disable(_Ctx) ->
    ok = service_watcher:unregister_service(name()),
    update_ctx(#{letsencrypt_enabled => false}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if Let's Encrypt certification is in progress.
%% @end
%%--------------------------------------------------------------------
is_regenerating() ->
    case service:get(name()) of
        {ok, #service{ctx = #{regenerating := true}}} -> true;
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks cert status on all nodes until finding an invalid cert
%% or ensuring validity of all certs.
%% @end
%%--------------------------------------------------------------------
-spec global_cert_status(service:ctx()) -> status().
global_cert_status(Ctx) ->
    case is_regenerating() of
        true -> regenerating;
        _ ->
            Nodes = get_nodes(),
            Domain = (get_plugin_module()):get_domain(Ctx),
            lists:foldl(fun(Node, PrevStatus) ->
                case PrevStatus of
                    valid -> rpc:call(Node, ?MODULE, local_cert_status, [Domain]);
                    Problem -> Problem
                end
            end, valid, Nodes)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks cert status on the current node.
%% @end
%%--------------------------------------------------------------------
-spec local_cert_status(ExpectedDomain :: binary()) -> status().
local_cert_status(ExpectedDomain) ->
    case onepanel_cert:read(?CERT_PATH) of
        {ok, Cert} -> cert_status(Cert, ExpectedDomain);
        {error, _} -> unknown
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks status of given cert.
%% @end
%%--------------------------------------------------------------------
-spec cert_status(Cert :: onepanel_cert:cert(), ExpectedDomain :: binary()) ->
    status().
cert_status(Cert, ExpectedDomain) ->
    Remaining = onepanel_cert:get_seconds_till_expiration(Cert),
    Margin = ?RENEW_MARGIN_SECONDS,

    case onepanel_cert:verify_hostname(Cert, ExpectedDomain) of
        error -> unknown;
        invalid -> domain_mismatch;
        valid ->
            if
                Remaining < Margin -> near_expiration;
                Remaining < 0 -> expired;
                true -> valid
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If key exists in the Map and is an integer (timestamp),
%% returns its iso8601 string representation. Otherwise 'null'.
%% @end
%%--------------------------------------------------------------------
-spec date_or_null(Key :: term(), Map :: map()) -> binary() | null.
date_or_null(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Epoch} when is_integer(Epoch) -> time_utils:epoch_to_iso8601(Epoch);
        _ -> null
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if Let's Encrypt certificates should be obtained
%% to replace exisitng foreign certificates.
%% @end
%%--------------------------------------------------------------------
-spec first_run(Ctx :: service:ctx()) -> boolean().
first_run(Ctx) ->
    Enabling = (is_enabled(Ctx) and not is_enabled(#{})),
    Enabling andalso not are_all_certs_letsencrypt().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if cert should be obtained.
%% @end
%%--------------------------------------------------------------------
-spec should_obtain(Ctx :: service:ctx()) -> boolean().
should_obtain(Ctx) ->
    first_run(Ctx) orelse case global_cert_status(Ctx) of
        valid -> false;
        regenerating -> false;
        _ -> true
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks if certificates on all hosts were issued by Let's Encrypt.
%% @end
%%--------------------------------------------------------------------
-spec are_all_certs_letsencrypt() -> boolean().
are_all_certs_letsencrypt() ->
    Nodes = get_nodes(),
    lists:all(fun(Node) ->
        true == rpc:call(Node, ?MODULE, is_local_cert_letsencrypt, [])
    end, Nodes).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the issuer of certificate on the current node
%% matches Let's Encrypt's issuer Common Name.
%% @end
%%--------------------------------------------------------------------
-spec is_local_cert_letsencrypt() -> boolean().
is_local_cert_letsencrypt() ->
    case onepanel_cert:read(?CERT_PATH) of
        {ok, Cert} ->
            Regex = application:get_env(?APP_NAME, letsencrypt_issuer_regex,
                <<"^Let's Encrypt Authority X[34]$">>),
            case onepanel_cert:get_issuer_cn(Cert) of
                undefined -> false;
                Issuer -> match == re:run(Issuer, Regex, [{capture, none}])
            end;
        {error, _} -> false
    end.


%%--------------------------------------------------------------------
%% @doc
%% Marks that user explicitely configured Let's Encrypt.
%% @end
%%--------------------------------------------------------------------
mark_configured(#{letsencrypt_enabled := _}) ->
    onepanel_deployment:mark_completed(?PROGRESS_LETSENCRYPT_CONFIG);
mark_configured(_Ctx) -> ok.


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


%%--------------------------------------------------------------------
%% @doc
%% Updates service ctx in the datastore.
%% @end
%%--------------------------------------------------------------------
-spec update_ctx(Diff) -> ok | no_return()
    when Diff :: fun((map()) -> map()) | map().
update_ctx(Diff) when is_map(Diff) ->
    update_ctx(fun(Ctx) -> maps:merge(Ctx, Diff) end);
update_ctx(Diff) when is_function(Diff, 1) ->
    service:update(name(), fun(#service{ctx = Ctx} = S) ->
        S#service{ctx = Diff(Ctx)}
    end).
