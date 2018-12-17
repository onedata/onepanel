%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module contains utilities for verifying DNS configuration.
%%% It is used to ensure that DNS contains records necessary
%%% for the cluster to work correctly.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_dns).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("modules/onepanel_dns.hrl").
-include_lib("ctool/include/logging.hrl").

-export([check_any/4, check_all/4, make_results_stub/2]).
-export([build_bind_record/3]).

% allowed query types
-type dns_type() :: a | txt.
% names used in queries
-type dns_name() :: inet_res:dns_name().
% IP tuple for A queries, binary for TXT queries
-type dns_value() :: inet:ip4_address() | binary().
% Result type
-type dns_check() :: #dns_check{}.

%% Possible results of dns check:
%% unresolvable - no values, expected or not, could be resolved
%% bad_recods - none of expected values are found in the resolved ones
%% missing_records - some, but not all, expected IPs are present in resolved ones
%% ok - all of expected values are found in the resolved ones
-type summary() :: ok | unresolvable | bad_records | missing_records.

-export_type([summary/0, dns_check/0, dns_type/0, dns_name/0, dns_value/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Queries all given DNS servers and compares result with the expected
%% list. Returns the most optimistic result among DNS servers.
%% @end
%%--------------------------------------------------------------------
-spec check_any(Expected :: [dns_value()], Names :: [dns_name()], Type :: dns_type(),
    Servers :: [inet:ip4_address()]) -> dns_check().
check_any(Expected, Names, Type, Servers) ->
    Diffs = check(Expected, Names, Type, Servers),
    Best = lists:last(lists:sort(fun compare/2, Diffs)),
    log_result(Names, Type, Servers, Best),
    Best.


%%--------------------------------------------------------------------
%% @doc
%% Queries all given DNS servers and compares result with the expected
%% list. Returns the most pessimistic result among responsive DNS servers.
%% @end
%%--------------------------------------------------------------------
-spec check_all(Expected :: [dns_value()], Names :: [dns_name()], Type :: dns_type(),
    Servers :: [inet:ip4_address()]) -> dns_check().
check_all(Expected, Names, Type, Servers) ->
    Diffs = check(Expected, Names, Type, Servers),
    Worst = hd(lists:sort(fun compare/2, Diffs)),
    log_result(Names, Type, Servers, Worst),
    Worst.


%%--------------------------------------------------------------------
%% @doc
%% Creates dns_check record for when no actual results could be
%% obtained.
%% @end
%%--------------------------------------------------------------------
-spec make_results_stub(Summary :: summary(), Expected :: [dns_value()]) -> dns_check().
make_results_stub(Summary, Expected) ->
    Unique = lists:usort(Expected),
    #dns_check{summary = Summary, expected = Unique, missing = Unique}.


%%--------------------------------------------------------------------
%% @doc
%% Build DNS record in the popular BIND server zone format.
%% @end
%%--------------------------------------------------------------------
-spec build_bind_record(Domain :: binary(), Type :: atom(),
    Value :: inet:ip4_address () | binary()) -> binary().
build_bind_record(Domain, a, IP) when is_tuple(IP) ->
    build_bind_record(Domain, a, onepanel_ip:ip4_to_binary(IP));

build_bind_record(Domain, txt, Value) ->
    QuotedValue = <<$", Value/binary, $">>,
    onepanel_utils:join([Domain, <<"IN">>, <<"TXT">>, QuotedValue], <<" ">>);

build_bind_record(Domain, Type, Value) ->
    TypeBin = string:uppercase(onepanel_utils:convert(Type, binary)),
    onepanel_utils:join([Domain, <<"IN">>, TypeBin, Value], <<" ">>).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Performs DNS check on each DNS server. Filters out servers
%% which could not be contacted.
%% @end
%%--------------------------------------------------------------------
-spec check(Expected :: [dns_value()], Names :: [dns_name()], Type :: dns_type(),
    Servers :: [inet:ip4_address() | default]) -> [dns_check()].
check(_Expected, _Names, _Type, []) ->
    check(_Expected, _Names, _Type, [default]);

check([], [], _Type, _Servers) ->
    [#dns_check{summary = ok, expected = [], got = []}];

check(Expected, Names, Type, Servers) ->
    Results = utils:pmap(fun(Server) ->
        check_on_server(Expected, Names, Type, Server)
    end, Servers),
    WithoutErrors = lists:filter(fun
        (error) -> false;
        (#dns_check{}) -> true
    end, Results),

    case WithoutErrors of
        [] -> ?throw_error(?ERR_DNS_CHECK_ERROR(
            io_lib:format("No DNS server responded to DNS check. Tried: ~p", [Servers])));
        _ -> WithoutErrors
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Queries given Server for given Names using DNS query type Type.
%% Compares results with the list of Expected values.
%% @end
%%--------------------------------------------------------------------
-spec check_on_server(Expected :: [dns_value()], Names :: [dns_name()], Type :: dns_type(),
    Servers :: inet:ip4_address() | default) -> dns_check() | error.
check_on_server(Expected, Names, Type, ServerIP) ->
    case lookup(Names, Type, ServerIP) of
        error -> error;
        Resolved ->
            Correct = onepanel_lists:intersect(Resolved, Expected),
            Missing = onepanel_lists:subtract(Expected, Resolved),
            Additional = onepanel_lists:subtract(Resolved, Expected),

            Summary = if
                Resolved == [] -> unresolvable;
                Correct == [] -> bad_records;
                Missing /= [] -> missing_records;
                true -> ok
            end,

            #dns_check{summary = Summary,
                expected = lists:usort(Expected), got = lists:usort(Resolved),
                missing = Missing, excessive = Additional}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns aggregated results of DNS queries for all Names.
%% If any of the queries causes connection error, all results
%% are discarded and 'error' is returned.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Quries :: [dns_name()], Type :: dns_type(),
    DnsServerIP :: inet:ip4_address() | default) -> [inet_res:dns_data()] | error.
lookup(Names, Type, DnsServerIP) ->
    Results = lists:flatten(utils:pmap(fun(Name) ->
        NameStr = onepanel_utils:convert(Name, list),
        Opts = case DnsServerIP of
            default -> [];
            IP -> [{nameservers, [{IP, 53}]}]
        end,
        Resolved = inet_res:resolve(NameStr, in, Type, Opts),
        case Resolved of
            {error, {_Reason, _DnsMsg}} -> [];
            {error, Reason} ->
                ?warning("Error querying server ~p for DNS check ~p of name ~p: ~p",
                    [DnsServerIP, Type, NameStr, Reason]),
                error;
            {ok, Msg} ->
                Answers = inet_dns:msg(Msg, anlist),
                % filter_answer will convert TXT query results from string to binary
                % preventing them from being caught by lists:flatten.
                filter_answer(Type, Answers)
        end
    end, Names)),
    case lists:any(fun(error) -> true; (_) -> false end, Results) of
        true -> error;
        false -> Results
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters query response by expected class and type.
%% This is similar to inet_res:lookup, which was not used
%% in order to expose server connection errors.
%% @end
%%--------------------------------------------------------------------
-spec filter_answer(Type :: dns_type(), Anlist :: [DnsRR :: term()]) -> [dns_value()].
filter_answer(Type, Anlist) ->
    [normalize_dns_data(Type, inet_dns:rr(RR, data)) || RR <- Anlist,
        inet_dns:rr(RR, class) == in,
        inet_dns:rr(RR, type) == Type].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts dns_data value returned by inet_res to expected format.
%% @end
%%--------------------------------------------------------------------
-spec normalize_dns_data(Type :: dns_type(), Data :: term()) -> dns_value().
normalize_dns_data(txt, Data) -> list_to_binary(Data);
normalize_dns_data(a, IPTuple) -> IPTuple.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Comparator for the check results.
%% When summaries are equal, result with higher number of excessive
%% records is considered worse.
%% @end
%%--------------------------------------------------------------------
-spec compare(dns_check(), dns_check()) -> boolean().
compare(#dns_check{summary = SameSummary, excessive = Exc1},
    #dns_check{summary = SameSummary, excessive = Exc2}) ->
    length(Exc1) >= length(Exc2);
compare(#dns_check{summary = Result1}, #dns_check{summary = Result2}) ->
    summary_to_integer(Result1) < summary_to_integer(Result2).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Provides results summary ordering with lower numbers indicating
%% more problematic results.
%% @end
%%--------------------------------------------------------------------
-spec summary_to_integer(summary()) -> integer().
summary_to_integer(unresolvable) -> 1;
summary_to_integer(bad_records) -> 2;
summary_to_integer(missing_records) -> 3;
summary_to_integer(ok) -> 4.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logs message describing negative results of the check.
%% @end
%%--------------------------------------------------------------------
-spec log_result(Names :: [dns_name()], Type :: dns_type(),
    Servers :: [inet:ip4_address()], Result :: dns_check()) -> ok.
log_result(_Names, _Type, _Servers, #dns_check{summary = ok}) -> ok;

log_result(Names, Type, Servers,
    #dns_check{summary = Summary, expected = Expected, got = Got}) ->
    ?warning("DNS check for records ~s named ~p failed with \"~s\":~n"
    "Servers used: ~p~nExpected values: ~p~nObtained values: ~p",
        [string:uppercase(atom_to_list(Type)), Names, Summary, Servers, Expected, Got]).

