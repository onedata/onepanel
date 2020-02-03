%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Bartosz Walkowicz
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour should be implemented by modules that implement middleware
%%% operations. Every middleware plugin serves as a link between
%%% API and onepanel internals in the context of specific
%%% entity type (onp_xxx records).
%%%
%%% NOTE !!!
%%% The link between entity type and plugin module that should handle request
%%% should be added to middleware:get_plugin function.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_plugin).


%%--------------------------------------------------------------------
%% @doc
%% Determines if given operation is supported based on operation, aspect and
%% scope (entity type is known based on the plugin itself).
%% @end
%%--------------------------------------------------------------------
-callback operation_supported(
    middleware:operation(), gri:aspect(), middleware:scope()
) ->
    boolean().


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of required availability checks for the request
%% execution to proceed.
%% @end
%%--------------------------------------------------------------------
-callback required_availability(
    middleware:operation(), gri:aspect(), middleware:scope()
) ->
    [middleware:availability_level()].


%%--------------------------------------------------------------------
%% @doc
%% Retrieves an entity and its revision from datastore based on EntityId.
%% Should return ?ERROR_NOT_FOUND if the entity does not exist.
%% Should return 'undefined' if fetch is not applicable for this operation.
%% @end
%%--------------------------------------------------------------------
-callback fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().


%%--------------------------------------------------------------------
%% @doc
%% Determines if requesting client is authorized to perform given operation,
%% based on middleware request and prefetched entity.
%% @end
%%--------------------------------------------------------------------
-callback authorize(middleware:req(), middleware:entity()) -> boolean().


%%--------------------------------------------------------------------
%% @doc
%% Determines if given request can be further processed
%% (e.g. checks whether space is supported locally).
%% Should throw custom error if not (e.g. ?ERROR_SPACE_NOT_SUPPORTED).
%% @end
%%--------------------------------------------------------------------
-callback validate(middleware:req(), middleware:entity()) -> ok | no_return().


%%--------------------------------------------------------------------
%% @doc
%% Creates a resource (aspect of entity) based on middleware request.
%% @end
%%--------------------------------------------------------------------
-callback create(middleware:req()) -> middleware:create_result().


%%--------------------------------------------------------------------
%% @doc
%% Retrieves a resource (aspect of entity) based on middleware request and
%% prefetched entity.
%% @end
%%--------------------------------------------------------------------
-callback get(middleware:req(), middleware:entity()) -> middleware:get_result().


%%--------------------------------------------------------------------
%% @doc
%% Updates a resource (aspect of entity) based on middleware request.
%% @end
%%--------------------------------------------------------------------
-callback update(middleware:req()) -> middleware:update_result().


%%--------------------------------------------------------------------
%% @doc
%% Deletes a resource (aspect of entity) based on middleware request.
%% @end
%%--------------------------------------------------------------------
-callback delete(middleware:req()) -> middleware:delete_result().
