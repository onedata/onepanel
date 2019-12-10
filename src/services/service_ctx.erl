%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides utility functions for service contex management.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ctx).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([get/2, get/3, get/4, get_domain/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv get(Name, Ctx, list)
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: onepanel_env:key(), Ctx :: service:ctx()) ->
    Value :: onepanel_env:value().
get(Key, Ctx) ->
    get(Key, Ctx, list).


%%--------------------------------------------------------------------
%% @doc Returns a value from the service context. If it's missing returns a value
%% from the application configuration.
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: onepanel_env:key(), Ctx :: service:ctx(),
    Type :: onepanel_utils:type()) -> Value :: onepanel_env:value().
get(Key, Ctx, Type) ->
    Value = case maps:find(Key, Ctx) of
        {ok, V} -> V;
        error -> onepanel_env:get(Key)
    end,
    onepanel_utils:convert(Value, Type).


%%--------------------------------------------------------------------
%% @doc Returns a value from the service context or the default one if it's
%% missing.
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: onepanel_env:key(), Ctx :: service:ctx(),
    Type :: onepanel_utils:type(), Default :: onepanel_env:value()) ->
    Value :: onepanel_env:value().
get(Key, Ctx, Type, Default) ->
    Value = case maps:find(Key, Ctx) of
        {ok, V} -> V;
        error -> Default
    end,
    onepanel_utils:convert(Value, Type).


%%--------------------------------------------------------------------
%% @doc Returns a domain name from the service context. If it's missing fetches
%% it from the system using 'hostname' shell program.
%% @end
%%--------------------------------------------------------------------
-spec get_domain(Key :: onepanel_env:key(), Ctx :: service:ctx()) ->
    Domain :: string() | no_return().
get_domain(Key, Ctx) ->
    case maps:find(Key, Ctx) of
        {ok, Domain} -> onepanel_utils:convert(Domain, list);
        error ->
            Hostname = onepanel_shell:get_success_output(["hostname", "-f"]),
            case string:split(Hostname, ".", all) of
                [_] -> error({short_hostname, Hostname});
                [_ | DomainTokens] ->
                    unicode:characters_to_list(lists:join(".", DomainTokens))
            end
    end.