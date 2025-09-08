%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility generic functions tests.
%%% @end
%%%--------------------------------------------------------------------
-module(tokens_test_utils).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").


%% API
-export([
    create_provider_registration_token/1
]).

% Time caveat is required in temporary tokens
-define(DEFAULT_TEMP_CAVEAT_TTL, 360000).


%%%===================================================================
%%% API
%%%===================================================================


-spec create_provider_registration_token(od_user:id()) -> tokens:serialized().
create_provider_registration_token(AdminUserId) ->
    Now = ozw_test_rpc:timestamp_seconds(),

    Token = ozw_test_rpc:create_user_temporary_token(?USER(AdminUserId), AdminUserId, #{
        <<"type">> => ?INVITE_TOKEN(?REGISTER_ONEPROVIDER, AdminUserId),
        <<"caveats">> => [#cv_time{valid_until = Now + ?DEFAULT_TEMP_CAVEAT_TTL}]
    }),
    {ok, SerializedToken} = tokens:serialize(Token),
    SerializedToken.
