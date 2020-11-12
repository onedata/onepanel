%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for dns.
%%% @end
%%%--------------------------------------------------------------------
-module(dns_rest_routes).
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
        %% Check correctness of DNS entries for the cluster's domain
        {<<"/dns_check">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% If true the DNS check cache is overridden and check is
                %% performed during handling of the request.
                forceCheck => {boolean, {optional, false}}
            }
        }},

        %% Return settings used when performing the DNS check
        {<<"/dns_check/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Configure dns check
        {<<"/dns_check/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check_configuration,
                scope = private
            },
            %% The configuration changes.
            data_spec = (rest_model:dns_check_configuration_model())
        }}

    ].
