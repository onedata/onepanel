%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for DNS tests.
%%% @end
%%%--------------------------------------------------------------------
-module(dns_test_utils).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    get_domain/1,
    get_k8s_service_domain/1,
    get_hostname/1,

    update_zone_subdomain_delegation/1,

    assert_panel_dns_config/2,
    update_panel_dns_config/2,

    invalidate_dns_check_cache/1,
    perform_dns_check/1,

    assert_dns_answer/4,
    assert_dns_answer/5
]).

-define(DNS_ASSERT_RETRY_COUNT, 7).
-define(DNS_ASSERT_RETRY_DELAY, timer:seconds(5)).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_domain(oct_background:entity_selector()) -> binary().
get_domain(zone) ->
    try
        oct_background:get_zone_domain()
    catch _:_ ->
        % Getting domain via oct_background fails if services are not yet deployed
        get_k8s_service_domain(?RAND_ELEMENT(oct_background:get_zone_panels()))
    end;
get_domain(ProviderSelector) ->
    try
        oct_background:get_provider_domain(ProviderSelector)
    catch _:_ ->
        % Getting domain via oct_background fails if services are not yet deployed
        get_k8s_service_domain(?RAND_ELEMENT(oct_background:get_provider_panels(ProviderSelector)))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns domain prescribed for node in its app.config by one-env/k8s when
%% starting environment.
%% NOTE: this may not be final domain as provider may register using subdomain.
%% @end
%%--------------------------------------------------------------------
-spec get_k8s_service_domain(node()) -> binary().
get_k8s_service_domain(Node) ->
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, test_web_cert_domain),
    str_utils:to_binary(Domain).


-spec get_hostname(node()) -> binary().
get_hostname(Node) ->
    str_utils:to_binary(hosts:from_node(Node)).


-spec update_zone_subdomain_delegation(boolean()) -> ok.
update_zone_subdomain_delegation(SubdomainDelegationEnabled) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(zone, <<"/zone/policies">>, #{auth => root, json => #{
            <<"subdomainDelegation">> => SubdomainDelegationEnabled
        }})
    ),
    ok.


-spec assert_panel_dns_config(oct_background:entity_selector(), json_utils:json_map()) -> ok.
assert_panel_dns_config(EntitySelector, ExpConfig) ->
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, ExpConfig},
        panel_test_rest:get(EntitySelector, <<"/dns_check/configuration">>, #{auth => root})
    ),
    ok.


-spec update_panel_dns_config(oct_background:entity_selector(), json_utils:json_map()) -> ok.
update_panel_dns_config(EntitySelector, JsonData) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(EntitySelector, <<"/dns_check/configuration">>, #{auth => root, json => JsonData})
    ),
    ok.


-spec invalidate_dns_check_cache(oct_background:entity_selector()) -> ok.
invalidate_dns_check_cache(EntitySelector) ->
    WorkerService = case EntitySelector of
        zone -> oz_worker;
        _ -> op_worker
    end,
    lists:foreach(fun(Node) ->
        panel_test_rpc:call(Node, dns_check, invalidate_cache, [WorkerService])
    end, panel_test_utils:get_panel_nodes(EntitySelector)).


-spec perform_dns_check(oct_background:entity_selector()) -> json_utils:json_map().
perform_dns_check(EntitySelector) ->
    {ok, _, _, Check} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(EntitySelector, <<"/dns_check">>, #{auth => root})
    ),
    Check.


%%--------------------------------------------------------------------
%% @doc
%% Verifies that all provided dns servers respond with expected
%% set of values. Does not verify order of received data.
%% @end
%%--------------------------------------------------------------------
-spec assert_dns_answer(
    Servers :: [inet:ip4_address()],
    Query :: string() | binary(),
    Type :: inet_res:rr_type(),
    Expected :: [inet_res:dns_data()]
) ->
    ok | no_return().
assert_dns_answer(Servers, Query, Type, Expected) ->
    assert_dns_answer(Servers, Query, Type, Expected, ?DNS_ASSERT_RETRY_COUNT).


%%--------------------------------------------------------------------
%% @doc
%% Verifies that all provided dns servers respond with expected
%% set of values. Does not verify order of received data.
%% Allows custom retries count.
%% @end
%%--------------------------------------------------------------------
-spec assert_dns_answer(
    Servers :: [inet:ip4_address()],
    Query :: string() | binary(),
    Type :: inet_res:r_type(),
    Expected :: [inet_res:dns_data()],
    Retries :: integer()
) ->
    ok | no_return().
assert_dns_answer(Servers, Query, Type, Expected, Attempts) ->
    Server = ?RAND_ELEMENT(Servers),
    QueryStr = str_utils:format("~ts", [Query]),
    SortedExpected = lists:sort(Expected),

    Opts = [{nameservers, [{Server, 53}]}],

    % there are multiple, delayed attempts because inet_res:lookup
    % displays ~20 seconds delay before returning updated results
    try
        ?assertEqual(
            SortedExpected,
            lists:sort(filter_response(Type, inet_res:resolve(QueryStr, any, Type, Opts))),
            Attempts,
            ?DNS_ASSERT_RETRY_DELAY
        )
    catch error:{Reason, _} = Error when
        Reason =:= assertEqual_failed;
        Reason =:= assertMatch_failed
    ->
        ct:pal(
            "DNS query type ~tp to server ~tp for name ~tp "
            "returned incorrect results in ~tp attempts.",
            [Type, Server, QueryStr, Attempts]
        ),
        erlang:error(Error)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters results of inet_res:resolve by record type and returns it.
%% @end
%%--------------------------------------------------------------------
-spec filter_response(Type :: atom(), Response :: {ok, #dns_rec{}} | {error, _}) ->
    [inet_res:dns_data()].
filter_response(_, {error, _}) ->
    [];
filter_response(Type, {ok, #dns_rec{
    anlist = Anlist,
    arlist = Arlist,
    nslist = Nslist
}}) ->
    lists:filtermap(fun
        (Record) when Record#dns_rr.type =:= Type -> {true, Record#dns_rr.data};
        (_) -> false
    end, Anlist ++ Arlist ++ Nslist).
