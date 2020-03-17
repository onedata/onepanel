%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Copy of type definitions from the cluster-worker's gs_protocol module.
%%% @end
%%%-------------------------------------------------------------------
-module(gs_protocol).
-author("Wojciech Geisler").


-type entity_type() :: atom().
-type entity_id() :: undefined | binary().

-type operation() :: create | get | update | delete.
% The resource the request operates on (creates, gets, updates or deletes).
-type entity() :: undefined | term().
-type revision() :: pos_integer().
-type versioned_entity() :: {entity(), revision()}.
-type scope() :: private | protected | shared | public | auto.
-type data() :: undefined | json_utils:json_map() | binary() | integer().
-type data_format() :: undefined | resource | value.

% Authorization hint that indicates the context needed to access shared
% resources or disambiguate issuer of an operation.
-type auth_hint() :: undefined.

-type graph_create_result() :: ok | {ok, value, term()} |
{ok, resource, {gri:gri(), {term(), revision()}} | {gri:gri(), auth_hint(), {term(), revision()}}} |
errors:error().
-type graph_get_result() :: {ok, {term(), revision()}} |
{ok, gri:gri(), {term(), revision()}} | {ok, value, term()} | errors:error().
-type graph_delete_result() :: ok | {ok, value, term()} | errors:error().
-type graph_update_result() :: ok | errors:error().


-export_type([
    auth_hint/0,
    data/0,
    data_format/0,
    entity/0,
    entity_id/0,
    entity_type/0,
    graph_create_result/0,
    graph_get_result/0,
    graph_delete_result/0,
    graph_update_result/0,
    operation/0,
    revision/0,
    scope/0,
    versioned_entity/0
]).
