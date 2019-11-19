%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Ceph pool management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_pool).
-author("Wojciech Geisler").

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

-define(POOL_TYPE, onepanel_env:get(ceph_pool_application_type)).

-define(LIST_CACHE_KEY, ceph_pools).
-define(DETAILS_CACHE_KEY(PoolName), {ceph_pool, PoolName}).
-define(CACHE_TIMEOUT, 60 * 60 * 1000). % 1 hour

% @formatter:off
-type name() :: binary().
-type spec() :: #{
    name := name(),
    copiesNumber => non_neg_integer(),
    minCopiesNumber => non_neg_integer()
}.
-type bytes() :: non_neg_integer().
-type usage() :: #{
    used := bytes(),
    maxAvailable := bytes()
}.
% @formatter:on

-export_type([name/0, usage/0]).

%% API functions
-export([get/1, get_all/0, list/0, exists/1]).
-export([pool_params/0, validate_copies_number/1, validate_copies_number/2,
    insert_default_values/1]).
-export([parse_data_usage/1]).

%% Step functions
-export([create/1, delete/1, set_replication/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns details of given pool.
%% @end
%%--------------------------------------------------------------------
-spec get(name()) -> spec() | {error, _}.
get(Name) ->
    case simple_cache:get(?DETAILS_CACHE_KEY(Name), fun() ->
        Params = get_params(Name, [
            {copiesNumber, <<"size">>},
            {minCopiesNumber, <<"min_size">>}
        ]),
        case Params of
            Map when is_map(Map) ->
                {true, Params#{name => Name}, ?CACHE_TIMEOUT};
            {error, _} = Error ->
                Error
        end
    end) of
        {ok, Params} -> Params;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns list of pool details.
%% @end
%%--------------------------------------------------------------------
-spec get_all() -> [#{atom() := term()}].
get_all() ->
    lists:map(fun(Name) ->
        #{} = ?MODULE:get(Name)
    end, list()).


%%--------------------------------------------------------------------
%% @doc
%% Lists pool names in the Ceph cluster.
%% @end
%%--------------------------------------------------------------------
-spec list() -> [name()].
list() ->
    {ok, Names} = simple_cache:get(?LIST_CACHE_KEY, fun() ->
        {ok, Node} = nodes:onepanel_with(?SERVICE_CEPH),
        PoolsList = rpc:call(Node, ceph_cli, list_pools, []),
        Names = [Name || #{name := Name} <- PoolsList],
        {true, Names, ?CACHE_TIMEOUT}
    end),
    Names.


%%--------------------------------------------------------------------
%% @doc Checks if pool with given name exists
%% @end
%%--------------------------------------------------------------------
-spec exists(name()) -> boolean().
exists(Name) ->
    lists:member(Name, list()).


%%--------------------------------------------------------------------
%% @doc
%% Ensures given copies number is valid with relation to the number
%% of deployed OSDs.
%% @end
%%--------------------------------------------------------------------
-spec insert_default_values(spec()) -> spec().
insert_default_values(Ctx) ->
    insert_default_values(Ctx, service_ceph_osd:count()).


%% @private
-spec insert_default_values(spec(), OsdsCount :: non_neg_integer()) ->
    spec().
insert_default_values(_Ctx, Osds) when Osds =< 0 ->
    throw(?ERROR_NO_SERVICE_NODES(?SERVICE_CEPH_OSD));

insert_default_values(Ctx, 1) ->
    maps:merge(#{copiesNumber => 1, minCopiesNumber => 1}, Ctx);

insert_default_values(#{copiesNumber := 1} = Ctx, _) ->
    % minCopiesNumber cannot be higher than copiesNumber
    maps:merge(#{minCopiesNumber => 1}, Ctx);

insert_default_values(Ctx, Osds) when Osds >= 2 ->
    maps:merge(#{copiesNumber => 2, minCopiesNumber => 2}, Ctx).


%%--------------------------------------------------------------------
%% @doc
%% Ensures pool params are valid with relation to the number
%% of deployed OSDs.
%% @end
%%--------------------------------------------------------------------
-spec validate_copies_number(service:ctx()) -> ok | no_return().
validate_copies_number(Ctx) ->
    maps:map(fun ceph_pool:validate_copies_number/2,
        maps:with(pool_params(), Ctx)),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Ensures given copies number is valid with relation to the number
%% of deployed OSDs.
%% @end
%%--------------------------------------------------------------------
-spec validate_copies_number(Variable :: atom(),
    CopiesNumber :: integer()) -> ok | no_return().
validate_copies_number(Variable, Number) when Number =< 0 ->
    throw(?ERROR_BAD_VALUE_TOO_LOW(Variable, 1));
validate_copies_number(Variable, Number) ->
    case service_ceph_osd:count() of
        OsdCount when Number =< OsdCount -> ok;
        OsdCount -> throw(?ERROR_BAD_VALUE_TOO_HIGH(Variable, OsdCount))
    end.


%%%===================================================================
%%% Step functions
%%% Must be executed on service_ceph hosts
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a Ceph pool. Takes a name and optionally
%% number of copies for data redundancy.
%% @end
%%--------------------------------------------------------------------
-spec create(spec()) -> ok.
create(#{name := Name}) ->
    ceph_cli:pool_create(Name),
    % set application type to silence warning in cluster health
    ceph_cli:pool_set_application(Name, ?POOL_TYPE),
    clear_cache(Name).


%%--------------------------------------------------------------------
%% @doc
%% Deletes Ceph pool - causes DATA LOSS.
%% @end
%%--------------------------------------------------------------------
-spec delete(#{name := name()}) -> ok.
delete(#{name := Name}) ->
    ceph_cli:pool_delete(Name),
    clear_cache(Name).


%%--------------------------------------------------------------------
%% @doc
%% Sets parameters controlling number of redundant object replicas.
%% @end
%%--------------------------------------------------------------------
-spec set_replication(spec()) -> ok.
set_replication(#{name := Name, copiesNumber := _} = Ctx) ->
    {Size, Ctx2} = maps:take(copiesNumber, Ctx),
    validate_copies_number(copiesNumber, Size),
    ceph_cli:set_pool_param(Name, <<"size">>, Size),
    set_replication(Ctx2);

set_replication(#{name := Name, minCopiesNumber := _} = Ctx) ->
    {MinSize, Ctx2} = maps:take(minCopiesNumber, Ctx),
    validate_copies_number(minCopiesNumber, MinSize),
    ceph_cli:set_pool_param(Name, <<"min_size">>, MinSize),
    set_replication(Ctx2);

set_replication(#{name := Name}) ->
    clear_cache(Name).


%%--------------------------------------------------------------------
%% @doc List of available pool parameters.
%% @end
%%--------------------------------------------------------------------
-spec pool_params() -> [atom()].
pool_params() ->
    [copiesNumber, minCopiesNumber].


%%--------------------------------------------------------------------
%% @doc
%% Translates data usage statistics as returned by "ceph df" command.
%% @end
%%--------------------------------------------------------------------
-spec parse_data_usage(#{binary() := integer(), _ => _}) -> usage().
parse_data_usage(#{<<"bytes_used">> := Used, <<"max_avail">> := MaxAvail}) ->
    #{
        used => Used,
        maxAvailable => MaxAvail
    }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_params(name(), [{Key, Param :: binary()}]) ->
    #{Key => Value :: term()} | {error, _} when Key :: atom().
get_params(PoolName, Params) ->
    {ok, Node} = nodes:onepanel_with(?SERVICE_CEPH),
    onepanel_lists:foldl_while(fun({Key, Param}, Acc) ->
        case rpc:call(Node, ceph_cli, get_pool_param, [PoolName, Param]) of
            {ok, Value} -> {cont, Acc#{Key => Value}};
            Error -> {halt, ?make_error(Error)}
        end
    end, #{}, Params).


%% @private
-spec clear_cache(name()) -> ok.
clear_cache(Name) ->
    Nodes = nodes:all(?SERVICE_PANEL),
    rpc:multicall(Nodes, simple_cache, clear, [?DETAILS_CACHE_KEY(Name)]),
    rpc:multicall(Nodes, simple_cache, clear, [?LIST_CACHE_KEY]),
    ok.
