%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind REST resources common
%%% for Onezone and Oneprovider panels.
%%%-------------------------------------------------------------------
-module(rest_onepanel).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("http/rest.hrl").
-include("authentication.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, is_conflict/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

%% API functions
-export([rest_to_marker_mapping/0]).

-define(SERVICE, service_onepanel:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = root}}) ->
    {true, Req};

is_authorized(Req, 'PUT', #rstate{resource = emergency_passphrase}) ->
    {not emergency_passphrase:is_set(), Req};

is_authorized(Req, 'GET', #rstate{client = #client{role = member}}) ->
    {true, Req};
is_authorized(Req, _Method, #rstate{client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_UPDATE), Req};

is_authorized(Req, 'POST', #rstate{resource = cluster}) ->
    {service_onepanel:available_for_clustering(), Req};

is_authorized(Req, 'GET', #rstate{resource = emergency_passphrase}) ->
    {true, Req};
is_authorized(Req, 'GET', #rstate{resource = configuration}) ->
    {true, Req};
is_authorized(Req, 'GET', #rstate{resource = test_image}) ->
    {true, Req};
is_authorized(Req, 'GET', #rstate{resource = node}) ->
    {true, Req};

is_authorized(Req, _Method, #rstate{}) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = host, bindings = #{host := Host}}) ->
    {lists:member(Host, service_onepanel:get_hosts()), Req};

exists_resource(Req, #rstate{resource = web_cert}) ->
    {model:exists(service) andalso service:exists(service_letsencrypt:name()), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_conflict/4}
%% @end
%%--------------------------------------------------------------------
is_conflict(Req, _Method, _Args, #rstate{resource = cluster}) ->
    {not service_onepanel:available_for_clustering(), Req};

is_conflict(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, 'PATCH', #rstate{resource = web_cert}) ->
    {service:all_healthy(), Req};

is_available(Req, _Method, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', #{clusterHost := Host} = Args, #rstate{resource = cluster}) ->
    Ctx = onepanel_maps:get_store(cookie, Args, cookie, #{
        cluster_host => onepanel_utils:convert(Host, list)
    }),

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, join_cluster, Ctx
    ))};


accept_resource(Req, 'POST', #{address := Address},
    #rstate{resource = hosts, version = ApiVersion}) ->
    {true, cowboy_req:set_resp_body(json_utils:encode(
        rest_replier:format_service_step(service_onepanel, extend_cluster,
            service_utils:throw_on_error(service:apply_sync(
                ?SERVICE, extend_cluster, #{address => Address, api_version => ApiVersion}
            )))
    ), Req)};

accept_resource(Req, 'PUT', Args, #rstate{resource = emergency_passphrase}) ->
    #{newPassphrase := NewPassphrase} = Args,
    CurrentPassphrase = maps:get(currentPassphrase, Args, undefined),
    ok = emergency_passphrase:change(CurrentPassphrase, NewPassphrase),
    {true, Req};

accept_resource(Req, 'PATCH', Args, #rstate{resource = web_cert}) ->
    Ctx = onepanel_maps:get_store(letsEncrypt, Args, letsencrypt_enabled),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE_LE, update, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = progress}) ->
    Mapping = rest_to_marker_mapping(),
    MarksToSet = lists:filtermap(fun({Field, Bool}) ->
        case lists:keyfind(Field, 1, Mapping) of
            {Field, ProgressMark} -> {true, {ProgressMark, Bool}};
            false -> false
        end
    end, maps:to_list(Args)),

    lists:foreach(fun
        ({Marker, true}) -> onepanel_deployment:set_marker(Marker);
        ({Marker, false}) -> onepanel_deployment:unset_marker(Marker)
    end, MarksToSet),
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = cookie}) ->
    {erlang:get_cookie(), Req};

provide_resource(Req, #rstate{resource = emergency_passphrase}) ->
    {#{isSet => emergency_passphrase:is_set()}, Req};

provide_resource(Req, #rstate{resource = node}) ->
    Hostname = onepanel_utils:convert(hosts:self(), binary),
    ClusterType = onepanel_env:get_cluster_type(),
    {#{hostname => Hostname, clusterType => ClusterType}, Req};

provide_resource(Req, #rstate{resource = hosts}) ->
    Hosts = service_onepanel:get_hosts(),
    {lists:sort(onepanel_utils:convert(Hosts, {seq, binary})), Req};

provide_resource(Req, #rstate{resource = configuration}) ->
    {rest_replier:format_onepanel_configuration(), Req};

provide_resource(Req, #rstate{resource = test_image}) ->
    % Dummy image in png format. Used by gui to check connectivity.
    {{binary, <<
        137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0,
        0, 1, 0, 0, 0, 1, 1, 3, 0, 0, 0, 37, 219, 86, 202, 0, 0, 0, 6, 80,
        76, 84, 69, 0, 0, 0, 255, 255, 255, 165, 217, 159, 221, 0, 0, 0, 9,
        112, 72, 89, 115, 0, 0, 14, 196, 0, 0, 14, 196, 1, 149, 43, 14, 27,
        0, 0, 0, 10, 73, 68, 65, 84, 8, 153, 99, 96, 0, 0, 0, 2, 0, 1, 244,
        113, 100, 166, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130
    >>}, Req};

provide_resource(Req, #rstate{resource = web_cert}) ->
    {rest_replier:format_service_step(service_letsencrypt, get_details,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_LE, get_details, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = progress}) ->
    {rest_replier:format_deployment_progress(), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{resource = host, bindings = #{host := Host}}) ->
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, leave_cluster, #{hosts => [Host]}
    ))}.


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Maps between rest 'progress' endpoint fields and atoms
%% used in onepanel_deployment model.
%% @end
%%--------------------------------------------------------------------
-spec rest_to_marker_mapping() -> [{atom(), onepanel_deployment:marker()}].
rest_to_marker_mapping() ->
    rest_to_marker_mapping(onepanel_env:get_cluster_type()).


%% @private
-spec rest_to_marker_mapping(onedata:cluster_type() | common) ->
    [{RestField :: atom(), ProgressMark :: onepanel_deployment:marker()}].
rest_to_marker_mapping(onezone) ->
    rest_to_marker_mapping(common);

rest_to_marker_mapping(oneprovider) ->
    [
        {storagesSetup, ?PROGRESS_STORAGE_SETUP}
        | rest_to_marker_mapping(common)
    ];

rest_to_marker_mapping(common) -> [
    {clusterNodes, ?PROGRESS_CLUSTER},
    {clusterIps, ?PROGRESS_CLUSTER_IPS},
    {webCertificate, ?PROGRESS_LETSENCRYPT_CONFIG},
    {dnsCheck, ?DNS_CHECK_ACKNOWLEDGED}
].
