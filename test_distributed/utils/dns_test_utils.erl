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

-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    assert_dns_answer/4,
    assert_dns_answer/5
]).

-define(DNS_ASSERT_RETRY_COUNT, 7).
-define(DNS_ASSERT_RETRY_DELAY, timer:seconds(5)).


%%%===================================================================
%%% API
%%%===================================================================


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
    QueryStr = str_utils:format("~s", [Query]),
    SortedExpected = lists:sort(Expected),

    Opts = [{nameservers, [{Server, 53}]}],

    % there are multiple, delayed attempts because inet_res:lookup
    % displays ~20 seconds delay before returning updated results
    try
        ?assertEqual(
            SortedExpected,
            filter_response(Type, inet_res:resolve(QueryStr, any, Type, Opts)),
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
%% Filters results of inet_res:resolve by record type and returns it sorted.
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
    lists:sort(lists:filtermap(fun
        (Record) when Record#dns_rr.type =:= Type -> {true, Record#dns_rr.data};
        (_) -> false
    end, Anlist ++ Arlist ++ Nslist)).
