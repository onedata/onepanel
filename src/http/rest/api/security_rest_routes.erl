%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for security.
%%% @end
%%%--------------------------------------------------------------------
-module('security_rest_routes').
-author("Wojciech Geisler").

-include("http/rest.hrl").

%% API
-export([routes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples consisting of a path, a handler module and
%% an initial request state.
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), #rest_req{}}].
routes() ->
    [
        %% Get emergency passphrase status
        {<<"/emergency_passphrase">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = emergency_passphrase,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Set emergency passphrase
        {<<"/emergency_passphrase">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = emergency_passphrase,
                scope = private
            },
            data_spec = (rest_model:emergency_passphrase_change_request_model())
        }}

    ].
