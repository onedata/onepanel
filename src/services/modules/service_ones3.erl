%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains oneS3 service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ones3).
-author("Bartosz Walkowicz").

-behaviour(service_behaviour).
-behaviour(letsencrypt_plugin_behaviour).

-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/codes.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% LE callbacks
-export([
    set_txt_record/1, remove_txt_record/1,
    get_dns_server/0,
    get_domain/0,
    get_admin_email/0,
    supports_letsencrypt_challenge/1,
    reload_webcert/1
]).

%% API
-export([
    exists/0,

    get_port/0,

    create_service/1, add_service_host/1,

    set_node_ip/1,
    format_hosts_ips/0,
    get_hosts_ips/0,

    configure/1,
    start/1,
    wait_for_init/1,
    stop/1,
    status/1
]).


-type model_ctx() :: #{
    % Caches (i.e. not the primary source of truth):
    % service status cache
    status => #{service:host() => service:status()}
}.
-export_type([model_ctx/0]).


%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> service:name().
name() ->
    ?SERVICE_ONES3.


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
    nodes:all(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(create, #{hosts := Hosts}) ->
    NewHosts = lists_utils:subtract(Hosts, get_hosts()),

    [
        #step{function = create_service, selection = first},
        #step{hosts = NewHosts, function = add_service_host}
    ];

get_steps(set_cluster_ips, #{hosts := Hosts} = _Ctx) ->
    [#step{function = set_node_ip, hosts = Hosts}];
get_steps(set_cluster_ips, #{cluster_ips := HostsToIps} = Ctx) ->
    % execute only on nodes where ip is explicitly provided
    get_steps(set_cluster_ips, Ctx#{hosts => lists_utils:intersect(get_hosts(), maps:keys(HostsToIps))});
get_steps(set_cluster_ips, Ctx) ->
    % execute on all service hosts, "guessing" IP if necessary
    get_steps(set_cluster_ips, Ctx#{hosts => get_hosts()});

get_steps(resume, _Ctx) ->
    [
        #steps{action = start},
        #step{function = wait_for_init}
    ];

get_steps(start, _Ctx) ->
    [
        #step{function = configure},
        #step{function = start}
    ];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#steps{action = stop}, #steps{action = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}];

get_steps(reload_webcert, _Ctx) ->
    [#step{function = reload_webcert}].


%%%===================================================================
%%% LE callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Sets txt record in onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(Ctx :: service:step_ctx()) -> ok.
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := TTL}) ->
    BasicRecordJson = #{
        <<"name">> => Name,
        <<"content">> => Value
    },
    RecordJson = maps_utils:put_if_defined(BasicRecordJson, <<"ttl">>, TTL),
    ok = op_worker_rpc:update_txt_records(#{<<"setOneS3TxtRecord">> => RecordJson}).


%%--------------------------------------------------------------------
%% @doc
%% Removes txt record from onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_record(Ctx :: service:step_ctx()) -> ok.
remove_txt_record(#{txt_name := Name}) ->
    ok = op_worker_rpc:update_txt_records(#{<<"unsetOneS3TxtRecordName">> => Name}).


%%--------------------------------------------------------------------
%% @doc
%% Returns hostname of the dns server responsible for setting txt record.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_server() -> string().
get_dns_server() ->
    service_oneprovider:get_oz_domain().


-spec get_domain() -> binary().
get_domain() ->
    OpDomain = service_op_worker:get_domain(),
    <<"s3.", OpDomain/binary>>.


%%--------------------------------------------------------------------
%% @doc Returns the email address of the provider administrator.
%% @end
%%--------------------------------------------------------------------
-spec get_admin_email() -> binary().
get_admin_email() ->
    service_op_worker:get_admin_email().


%%--------------------------------------------------------------------
%% @doc
%% Checks if given Let's Encrypt challenge can be fulfilled.
%% @end
%%--------------------------------------------------------------------
-spec supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean().
supports_letsencrypt_challenge(Challenge) ->
    service_op_worker:supports_letsencrypt_challenge(Challenge).


-spec reload_webcert(service:step_ctx()) -> ok.
reload_webcert(Ctx) ->
    % OneS3 can not live reload certs - it must be restarted
    ok = stop(Ctx),
    ok = start(Ctx),
    ok = wait_for_init(Ctx).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec exists() -> boolean().
exists() ->
    service:exists(name()).


-spec get_port() -> undefined | inet:port_number().
get_port() ->
    onepanel_env:get(ones3_http_port, ?SERVICE_PANEL).


-spec create_service(service:step_ctx()) -> ok.
create_service(_Ctx) ->
    case service:create(#service{name = name()}) of
        {ok, _} -> ok;
        ?ONP_ERR_ALREADY_EXISTS -> ok
    end.


-spec add_service_host(service:step_ctx()) -> ok.
add_service_host(_Ctx) ->
    Host = hosts:self(),
    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc
%% Writes node Ip to app.config on the current node's panel.
%% If Ip is not given explicitly in cluster_ips map
%% and panel has none in its app config onepanel tries to determine it.
%% @end
%%--------------------------------------------------------------------
-spec set_node_ip(Ctx :: service:step_ctx()) -> ok | no_return().
set_node_ip(Ctx) ->
    Host = hosts:self(),

    {ok, Ip} = case kv_utils:find([cluster_ips, Host], Ctx) of
        {ok, NewIp} ->
            % TODO VFS-12379 ?PROGRESS_CLUSTER_IPS is set in cw also. Check if this can stay as is or should be done only once
            onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),
            ip_utils:to_ip4_address(NewIp);
        _ ->
            {ok, infer_ip()}
    end,

    onepanel_env:write([?SERVICE_PANEL, external_ip], Ip, ?SERVICE_PANEL),
    onepanel_env:set(external_ip, Ip, ?SERVICE_PANEL),
    dns_check:invalidate_cache(op_worker).


-spec format_hosts_ips() -> #{Host :: binary() => Ip :: binary()}.
format_hosts_ips() ->
    maps:from_list(lists:map(fun
        ({Host, undefined}) ->
            {onepanel_utils:convert(Host, binary), null};
        ({Host, IP}) ->
            {onepanel_utils:convert(Host, binary), onepanel_ip:ip4_to_binary(IP)}
    end, get_hosts_ips())).


-spec get_hosts_ips() -> [{service:host(), inet:ip4_address()}].
get_hosts_ips() ->
    Args = [[?SERVICE_PANEL, external_ip], ?SERVICE_PANEL],

    lists:map(fun(Host) ->
        Node = nodes:service_to_node(?SERVICE_PANEL, Host),
        {ok, Ip} = rpc:call(Node, onepanel_env, read_effective, Args),
        {Host, Ip}
    end, hosts:all(name())).


-spec configure(service:step_ctx()) -> ok | no_return().
configure(_Ctx) ->
    ConfigPath = onepanel_env:get(ones3_config_path),
    ok = file:write_file(ConfigPath, build_config()).


-spec start(service:step_ctx()) -> ok | no_return().
start(Ctx) ->
    ServiceName = name(),

    service:update_status(ServiceName, starting),
    service_cli:start(ServiceName, Ctx),
    service:update_status(ServiceName, unhealthy),
    service:register_healthcheck(ServiceName, #{hosts => [hosts:self()]}).


-spec wait_for_init(service:step_ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    StartAttempts = onepanel_env:get(ones3_wait_for_init_attempts),

    ?info("Awaiting readiness of the OneS3 server..."),
    onepanel_utils:wait_until(?MODULE, status, [Ctx], {equal, healthy}, StartAttempts),
    ?info("OneS3 server OK"),

    service:register_healthcheck(name(), #{hosts => [hosts:self()]}).


-spec stop(service:step_ctx()) -> ok | no_return().
stop(Ctx) ->
    ServiceName = name(),

    service:deregister_healthcheck(ServiceName, Ctx),
    service:update_status(ServiceName, stopping),
    service_cli:stop(ServiceName),
    % update status cache
    status(Ctx),
    ok.


-spec status(service:step_ctx()) -> service:status().
status(Ctx) ->
    ServiceName = name(),
    service:update_status(ServiceName, case service_cli:status(ServiceName, status) of
        running -> health(Ctx);
        stopped -> stopped;
        missing -> missing
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec infer_ip() -> inet:ip4_address().
infer_ip() ->
    case onepanel_env:read_effective([?SERVICE_PANEL, external_ip], ?SERVICE_PANEL) of
        {ok, {_, _, _, _} = Ip} ->
            Ip;
        {ok, IpList} when is_list(IpList) ->
            {ok, Ip} = ip_utils:to_ip4_address(IpList),
            Ip;
        _ ->
            onepanel_ip:determine_ip(name())
    end.


%% @private
-spec build_config() -> binary().
build_config() ->
    BasicOpts = [
        {"verbose_log_level", onepanel_env:get(ones3_verbose_log_level)},
        {"onezone_host", service_oneprovider:get_oz_domain()},
        {"provider_host", service_op_worker:get_domain()},
        {"ones3_https_port", get_port()},
        {"ones3_ssl_cert", get_abs_path(web_cert_full_chain_file)},
        {"ones3_ssl_key", get_abs_path(web_key_file)}
    ],
    Opts = case onepanel_env:get(treat_test_ca_as_trusted) of
        false ->
            BasicOpts;
        true ->
            [
                {"custom_ca_dir", get_abs_path(cacerts_dir)}
                | BasicOpts
            ]
    end,

    Lines = lists:map(fun({Opt, Value}) ->
        str_utils:format_bin("~ts=~ts", [Opt, str_utils:to_binary(Value)])
    end, Opts),
    str_utils:join_binary(Lines, <<"\n">>).


%% @private
-spec get_abs_path(atom()) -> binary().
get_abs_path(EnvVar) ->
    filename:absname(onepanel_utils:convert(onepanel_env:get(EnvVar), binary)).


%% @private
-spec health(service:step_ctx()) -> service:status().
health(_Ctx) ->
    Host = hosts:self(),
    Port = get_port(),
    Url = str_utils:format_bin("https://~ts:~B/.__onedata__status__", [Host, Port]),
    Opts = [{
        connect_timeout, timer:seconds(30)},
        {recv_timeout, timer:seconds(30)},
        % cacerts opt will not work as hosts:self() hostname will not validate
        {ssl_options, [{secure, false}]}
    ],

    case http_client:get(Url, #{}, <<>>, Opts) of
        {ok, ?HTTP_200_OK, _, Resp} ->
            case json_utils:decode(Resp) of
                #{<<"isOk">> := true} ->
                    healthy;
                _ ->
                    ?error(?autoformat_with_msg(
                        "OneS3 server is running but not healthy:", [Url]
                    )),
                    unhealthy
            end;
        _ ->
            ?error(?autoformat_with_msg("Cannot connect to OneS3 server:", [Url])),
            unhealthy
    end.
