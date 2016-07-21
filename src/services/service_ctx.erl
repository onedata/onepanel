%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
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
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: service:name(), Ctx :: service:ctx()) -> Value :: term().
get(Name, Ctx) ->
    get(Name, Ctx, list).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: service:name(), Ctx :: service:ctx(),
    Type :: onepanel_utils:type()) -> Value :: term().
get(Name, Ctx, Type) ->
    Value = case maps:find(Name, Ctx) of
        {ok, V} -> V;
        error -> onepanel_env:get(Name)
    end,
    onepanel_utils:convert(Value, Type).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Name :: service:name(), Ctx :: service:ctx(),
    Type :: onepanel_utils:type(), Default :: term()) -> Value :: term().
get(Name, Ctx, Type, Default) ->
    Value = case maps:find(Name, Ctx) of
        {ok, V} -> V;
        error -> Default
    end,
    onepanel_utils:convert(Value, Type).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_domain(Key :: atom(), Ctx :: service:ctx()) ->
    Domain :: string() | no_return().
get_domain(Key, Ctx) ->
    case maps:find(Key, Ctx) of
        {ok, Domain} -> Domain;
        error ->
            Hostname = onepanel_shell:check_output(["hostname", "-f"]),
            case string:tokens(Hostname, ".") of
                [_] -> ?throw({short_hostname, Hostname});
                [_ | Domain] -> string:join(Domain, ".")
            end
    end.
