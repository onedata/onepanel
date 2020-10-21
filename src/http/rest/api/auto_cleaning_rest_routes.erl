%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for auto_cleaning.
%%% @end
%%%--------------------------------------------------------------------
-module(auto_cleaning_rest_routes).
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
        %% Cancel space auto-cleaning
        {<<"/provider/spaces/:id/auto-cleaning/cancel">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = cancel_auto_cleaning,
                scope = private
            }
        }},

        %% Configure space auto-cleaning mechanism
        {<<"/provider/spaces/:id/auto-cleaning/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_configuration,
                scope = private
            },
            produces = [<<"application/json">>],
            %% New configuration of space auto-cleaning mechanism.
            data_spec = (rest_model:space_auto_cleaning_configuration_model())
        }},

        %% Get the report from a space auto-cleaning run
        {<<"/provider/spaces/:id/auto-cleaning/reports/:report_id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = {auto_cleaning_report, ?BINDING(report_id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Ids of of the space auto-cleaning reports
        {<<"/provider/spaces/:id/auto-cleaning/reports">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_reports_list,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Allows to skip N first report Ids.
                offset => {integer, {optional, 0}},
                %% Allows to limit the number of returned report Ids up to N
                %% last reports. By default, all report Ids will be returned.
                limit => {integer, optional},
                %% Allows to list the report Ids starting from the specific
                %% report.
                index => {string, optional}
            }
        }},

        %% Get status of space auto-cleaning mechanism
        {<<"/provider/spaces/:id/auto-cleaning/status">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_status,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get space auto-cleaning configuration
        {<<"/provider/spaces/:id/auto-cleaning/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_cleaning_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Trigger space auto-cleaning
        {<<"/provider/spaces/:id/auto-cleaning/start">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = start_auto_cleaning,
                scope = private
            }
        }}

    ].
