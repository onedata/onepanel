%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains models definitions.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_MODELS_HRL).
-define(ONEPANEL_MODELS_HRL, 1).

% extend as needed
-define(MATCH_SPECS, '_' | '$1').

-define(MODELS, [onepanel_user, onepanel_session, onepanel_deployment, service]).
-define(WRAPPER_RECORD, document).

%% Wrapper for all document records. Allows generic tracking of model versions.
-record(document, {
    key :: model_behavior:key(),
    version = 1 :: model_behavior:version(),
    value :: model_behaviour:record()
}).


-record(onepanel_user, {
    username :: onepanel_user:name() | ?MATCH_SPECS,
    password_hash :: undefined | onepanel_user:password_hash() | ?MATCH_SPECS,
    role :: onepanel_user:role() | ?MATCH_SPECS,
    uuid :: onepanel_user:uuid() | ?MATCH_SPECS
}).

-record(onepanel_session, {
    id :: undefined | onepanel_session:id() | ?MATCH_SPECS,
    username :: undefined | onepanel_user:name() | ?MATCH_SPECS,
    last_refresh = 0 :: non_neg_integer() | ?MATCH_SPECS,
    nonce = <<"">> :: binary() | ?MATCH_SPECS,
    previous_nonce = <<"">> :: binary() | ?MATCH_SPECS,
    %% Tokens, from newest at head to oldest at tail
    rest_tokens :: [{onepanel_session:rest_api_token(), Expires :: non_neg_integer()}] |
        undefined | ?MATCH_SPECS
}).

-record(service, {
    name :: module(),
    hosts = [] :: [service:host()],
    ctx = #{} :: maps:map()
}).

-record(onepanel_deployment, {
    id :: atom(),
    completed = gb_sets:empty() :: gb_sets:set()
}).

-endif.