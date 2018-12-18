%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C): 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Module handling DNS checking for op_worker and oz_worker.
%%% There are two kinds of checks:
%%% "domain"  - verifying that cluster's domain is resolved to
%%%             the external IPs of worker nodes
%%% "dnsZone" - relevant to Onezone, checking that DNS zone delegation
%%%             is correctly set up
%%%
%%% The check is performed periodically every ?REFRESH_INTERVAL and
%%% on demand when forced by REST request. Result of last check
%%% is stored in the "ctx" of op/oz worker's service model for a maximum
%%% of ?CACHE_TTL seconds.
%%%-------------------------------------------------------------------
-module(dns_check).
-author("Wojciech Geisler").

-include("modules/models.hrl").
-include("modules/errors.hrl").
-include("modules/onepanel_dns.hrl").
-include("deployment_progress.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(REFRESH_INTERVAL, onepanel_env:get(dns_check_interval)). % 6 hours

% Set to multiple of ?REFRESH_INTERVAL to allow some failed tests
-define(CACHE_TTL, ?REFRESH_INTERVAL * 3). % 18 hours

-define(TIMESTAMP_KEY, dns_check_attempt_timestamp).
-define(CACHE_KEY, dns_check).

% Check names match expected fields in REST API.
-type check() :: domain | dnsZone.
-type worker_service() :: op_worker | oz_worker.

% @formatter:off
-type result() :: #{
    timestamp := non_neg_integer(),
    check() => #dns_check{}
}.
% @formatter:on

-export_type([result/0, check/0]).


%% API
-export([get/2, should_update_cache/1, invalidate_cache/1,
    update_cache/1, async_update_cache/1]).
-export([get_configuration/0, configure_dns_check/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns results of DNS check. Uses cache if its available and ForceRefresh
%% is not set.
%% @end
%%--------------------------------------------------------------------
-spec get(worker_service(), Refresh :: boolean()) -> result() | no_return().
get(Service, _ForceRefresh = true) ->
    update_cache(Service);

get(Service, _ForceRefresh = false) ->
    case retrieve_cached(Service) of
        {ok, Cached} -> Cached;
        error -> update_cache(Service)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Performs the DNS check. On success stores results in cache.
%% @end
%%--------------------------------------------------------------------
-spec update_cache(worker_service()) -> result() | no_return().
update_cache(Service) ->
    service:update_ctx(Service, #{?TIMESTAMP_KEY => get_timestamp()}),
    Checks = get_checks(Service),
    Result = compute_results(Service, Checks),
    service:update_ctx(Service, #{?CACHE_KEY => Result}),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% Performs the DNS check in a separate process. Handles
%% logging of errors in the process.
%% @end
%%--------------------------------------------------------------------
-spec async_update_cache(worker_service()) -> ok.
async_update_cache(Service) ->
    spawn(fun() ->
        try
            update_cache(Service)
        catch
            throw:#error{reason = ?ERR_DNS_CHECK_ERROR(Message)} ->
                ?error("DNS check refresh failed: ~s", [Message]);
            Type:Error ->
                % Catch all as a process failure with exception
                % causes an ugly error log
                ?error("DNS check refresh failed: ~p:~p", [Type, Error])
        end
    end),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Checks if time since last DNS check is more than ?REFRESH_INTERVAL.
%% @end
%%--------------------------------------------------------------------
-spec should_update_cache(service:name()) -> boolean().
should_update_cache(Service) ->
    Now = get_timestamp(),
    Interval = ?REFRESH_INTERVAL,
    case service:get_ctx(Service) of
        #{?TIMESTAMP_KEY := LastAttempt}
            when LastAttempt + Interval > Now ->
            false;
        _ ->
            true
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes stored results of the DNS check.
%% @end
%%--------------------------------------------------------------------
-spec invalidate_cache(worker_service()) -> ok | no_return().
invalidate_cache(Service) ->
    service:update(Service, fun(#service{ctx = Ctx} = S) ->
        Ctx2 = lists:foldl(fun maps:remove/2, Ctx, [?CACHE_KEY, ?TIMESTAMP_KEY]),
        S#service{ctx = Ctx2}
    end).


%%--------------------------------------------------------------------
%% @doc
%% Returns DNS check configuration in format expected by REST API.
%% @end
%%--------------------------------------------------------------------
-spec get_configuration() -> #{atom() := term()}.
get_configuration() ->
    #{
        builtInDnsServer => expect_delegation(oz_worker),
        dnsCheckAcknowledged => onepanel_deployment:is_completed(?DNS_CHECK_ACKNOWLEDGED),
        dnsServers => lists:map(fun onepanel_ip:ip4_to_binary/1, get_dns_servers())
    }.


%%-------------------------------------------------------------------
%% @doc
%% Sets settings used for checking DNS record validity.
%% @end
%%-------------------------------------------------------------------
-spec configure_dns_check(map()) -> ok.
configure_dns_check(#{built_in_dns_server := BuiltInDnsServer} = Ctx) ->
    onepanel_env:set(dns_expect_zone_delegation, BuiltInDnsServer),
    onepanel_env:write([?APP_NAME, dns_expect_zone_delegation], BuiltInDnsServer),
    configure_dns_check(maps:remove(built_in_dns_server, Ctx));

configure_dns_check(#{dns_servers := DnsServers} = Ctx) ->
    onepanel_env:set(dns_check_servers, DnsServers),
    onepanel_env:write([?APP_NAME, dns_check_servers], DnsServers),
    configure_dns_check(maps:remove(dns_servers, Ctx));

configure_dns_check(#{dns_check_acknowledged := Acknowledged} = Ctx) ->
    case Acknowledged of
        true -> onepanel_deployment:mark_completed(?DNS_CHECK_ACKNOWLEDGED);
        false -> onepanel_deployment:mark_not_completed(?DNS_CHECK_ACKNOWLEDGED)
    end,
    configure_dns_check(maps:remove(dns_check_acknowledged, Ctx));

configure_dns_check(_Ctx) ->
    case {service:exists(op_worker), service:exists(oz_worker)} of
        {true, _} -> invalidate_cache(op_worker);
        {_, true} -> invalidate_cache(oz_worker);
        _ -> ok
    end.


%%%===================================================================
%%% Private functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves cached result of DNS check.
%% Returns "error" if cache does not exist or is older than ?CACHE_TTL.
%% @end
%%--------------------------------------------------------------------
-spec retrieve_cached(worker_service()) -> {ok, result()} | error.
retrieve_cached(Service) ->
    Now = get_timestamp(),
    TTL = ?CACHE_TTL,

    case service:get_ctx(Service) of
        #{?CACHE_KEY := #{timestamp := Timestamp} = Cached} when
            Timestamp + TTL >= Now ->
            {ok, Cached};
        _ -> error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns list of DNS servers to be used for check.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_servers() -> [inet:ip4_address()].
get_dns_servers() -> onepanel_env:get(dns_check_servers).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selects checks to be performed.
%% When the cluster's "domain" is an IP no checks are applicable.
%% Zone delegation check is done only in Zone and only until
%% user disables it.
%% @end
%%--------------------------------------------------------------------
-spec get_checks(worker_service()) -> [check()].
get_checks(Service) ->
    Module = service:get_module(Service),
    Domain = Module:get_domain(),
    case onepanel_ip:is_ip(Domain) of
        true -> [];
        false ->
            case expect_delegation(Service) of
                true -> [domain, dnsZone];
                false -> [domain]
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns whether DNS zone delegation check is applicable.
%% @end
%%--------------------------------------------------------------------
-spec expect_delegation(worker_service()) -> boolean().
expect_delegation(op_worker) -> false;
expect_delegation(oz_worker) -> onepanel_env:get(dns_expect_zone_delegation).


%%--------------------------------------------------------------------
%% @private
%% @doc Gathers results for all requested checks.
%% @end
%%--------------------------------------------------------------------
-spec compute_results(worker_service(), [check()]) -> result().
compute_results(Service, Checks) ->
    lists:foldl(fun(Check, Acc) ->
        Acc#{Check => check(Service, Check)}
    end, #{timestamp => get_timestamp()}, Checks).


%%--------------------------------------------------------------------
%% @private
%% @doc Returns current time as unix epoch.
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp() -> non_neg_integer().
get_timestamp() ->
    time_utils:system_time_seconds().


%%--------------------------------------------------------------------
%% @private
%% @doc Performs check of given kind.
%% @end
%%--------------------------------------------------------------------
-spec check(worker_service(), check()) -> #dns_check{}.
check(Service, domain) ->
    Module = service:get_module(Service),
    Domain = Module:get_domain(),
    {_Hosts, IPs} = lists:unzip(service_cluster_worker:get_hosts_ips(#{name => Service})),
    Recommended = domain_bind_records(Domain, IPs),

    DnsServers = get_dns_servers(),

    Result = onepanel_dns:check_any(IPs, [Domain], a, DnsServers),
    Result#dns_check{
        bind_records = Recommended
    };

check(_Service = oz_worker, dnsZone) ->
    Domain = service_oz_worker:get_domain(),
    {_, IPs} = lists:unzip(service_oz_worker:get_ns_hosts()),
    DnsServers = get_dns_servers(),

    Opts = case DnsServers of
        [] -> [];
        [_ | _] -> [{nameservers, [{DnsServer, 53} || DnsServer <- DnsServers]}]
    end,

    NsNames = lists:usort(lookup_ns_names(Domain, Opts)),

    Result = case NsNames of
        [] -> onepanel_dns:make_results_stub(unresolvable, IPs);
        _ -> onepanel_dns:check_any(IPs, NsNames, a, DnsServers)
    end,

    allow_missing_records(Result#dns_check{
        bind_records = delegation_bind_records(Domain)
    }).


%%--------------------------------------------------------------------
%% @private
%% @doc Change result "missing_records" into "ok". Used for dnsZone
%% check since it is acceptable to have only some NS matching cluster IPs.
%% @end
%%--------------------------------------------------------------------
-spec allow_missing_records(#dns_check{}) -> #dns_check{}.
allow_missing_records(#dns_check{summary = missing_records} = Check) ->
    Check#dns_check{summary = ok};
allow_missing_records(#dns_check{} = Check) -> Check.


%%--------------------------------------------------------------------
%% @private
%% @doc Gathers nameserver hostnames for given domain by querying
%% NS and SOA records.
%% @end
%%--------------------------------------------------------------------
-spec lookup_ns_names(Domain :: string() | binary(), Opts :: list()) ->
    [string()].
lookup_ns_names(Domain, Opts) when is_binary(Domain) ->
    lookup_ns_names(binary_to_list(Domain), Opts);

lookup_ns_names(Domain, Opts) ->
    NsRecords = inet_res:lookup(Domain, in, ns, Opts),
    case inet_res:lookup(Domain, in, soa, Opts) of
        [] -> NsRecords;
        [{PrimaryNS, _DnsAdmin, _, _, _, _, _}] -> [PrimaryNS | NsRecords]
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Generates recommended DNS records to satisfy the "domain" check.
%% @end
%%--------------------------------------------------------------------
-spec domain_bind_records(Domain :: binary(), IPs :: [inet:ip4_address()]) -> [binary()].
domain_bind_records(Domain, IPs) ->
    lists:usort(
        [onepanel_dns:build_bind_record(<<Domain/binary, $.>>, a, IP)
            || IP <- IPs]
    ).


%%--------------------------------------------------------------------
%% @private
%% @doc Generates recommended DNS records to satisfy the "dnsZone" check.
%% @end
%%--------------------------------------------------------------------
-spec delegation_bind_records(Domain :: binary()) -> [binary()].
delegation_bind_records(Domain) ->
    OnezoneNS = service_oz_worker:get_ns_hosts(),
    lists:usort(lists:flatmap(fun({Name, IP}) -> [
        onepanel_dns:build_bind_record(<<Domain/binary, $.>>, ns, Name),
        onepanel_dns:build_bind_record(<<Name/binary, $.>>, a, IP)
    ] end, OnezoneNS)).
