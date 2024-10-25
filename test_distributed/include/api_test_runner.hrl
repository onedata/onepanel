%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of macros and records used in API (REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Bartosz Walkowicz").

-ifndef(API_TEST_UTILS_HRL).
-define(API_TEST_UTILS_HRL, 1).

-include("authentication.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


-record(api_client, {
    %% Roles:
    %% guest - unauthenticated client, on endpoints with noauth enabled
    %% user - Onezone user not belonging to the cluster
    %% member - Onezone user belonging to the cluster, governed by privileges
    %% peer - Onepanel authorized with #authorization_nonce{}
    %% root - client authenticated with the emergency passphrase
    role = guest :: guest | user | member | peer | root,
    privileges = undefined :: undefined | [privileges:cluster_privilege()],
    basic_credentials = undefined :: undefined | {Username :: binary(), Password :: binary()},
    token = undefined :: undefined | binary()
}).

-define(ONENV_EMERGENCY_PASSPHRASE, <<"password">>). % emergency passphrase as set by one-env

-define(API_GUEST, #api_client{role = guest}).
-define(API_PEER, #api_client{role = peer}).
-define(API_ROOT, #api_client{
    role = root,
    basic_credentials = {?LOCAL_USERNAME, ?ONENV_EMERGENCY_PASSPHRASE}
}).

-define(API_ROOT_BAD_PASSPHRASE, #api_client{
    role = root,
    basic_credentials = {?LOCAL_USERNAME, <<"badPassphrase">>}
}).
-define(API_ROOT_BAD_USERNAME, #api_client{
    role = root,
    basic_credentials = {<<"badUsername">>, ?ONENV_EMERGENCY_PASSPHRASE}
}).
-define(API_USER_BAD_TOKEN, #api_client{role = user, token = <<"badToken">>}).

-define(INVALID_API_CLIENTS_AND_AUTH_ERRORS, [
    {?API_ROOT_BAD_PASSPHRASE, ?ERROR_BAD_BASIC_CREDENTIALS},
    {?API_ROOT_BAD_USERNAME, ?ERROR_BAD_BASIC_CREDENTIALS},
    {?API_USER_BAD_TOKEN, ?ERROR_BAD_TOKEN}
]).

-define(INVALID_API_CLIENTS, [
    ?API_ROOT_BAD_PASSPHRASE,
    ?API_ROOT_BAD_USERNAME,
    ?API_USER_BAD_TOKEN
]).

-record(client_spec, {
    correct = [] :: [api_test_runner:api_client_or_placeholder()],
    unauthorized = [] :: [
        api_test_runner:api_client_or_placeholder() |
        {api_test_runner:api_client_or_placeholder(), Reason :: errors:error()}
    ],
    forbidden = [] :: [
        api_test_runner:api_client_or_placeholder()|
        {api_test_runner:api_client_or_placeholder(), Reason :: errors:error()}
    ]
}).

-record(data_spec, {
    required = [] :: [Key :: binary() | {Key :: binary(), erorrs:error()}],
    optional = [] :: [Key :: binary()],
    at_least_one = [] :: [Key :: binary()],
    correct_values = #{} :: #{Key :: binary() => Values :: [binary()]},
    bad_values = [] :: [{Key :: binary(), Value :: term(), errors:error()}],

    % flag telling whether some optional value should be added to required only data sets
    at_least_one_optional_value_in_data_sets = false :: boolean(),
    % by default (`relaxed`) datasets from the optional values are generated as follows:
    % - one dataset for each key-value pair
    % - one dataset containing each key with randomly selected value;
    % e.g:
    % given
    %   optional = [<<"key1">>, <<"key2">>]
    %   correct_values = #{<<"key1">> => [<<"value1">>, <<"value2">>], <<"key2">> => [<<"value1">>]}
    % following datasets could be generated:
    %  #{<<"key1">> => <<"value1">>},
    %  #{<<"key1">> => <<"value2">>},
    %  #{<<"key2">> => <<"value1">>},
    %  #{<<"key1">> => <<"value1">>, #{<<"key2">> => <<"value1">>}
    % with `all_combinations` option following datasets will be generated (empty dataset is omitted):
    %  #{<<"key1">> => <<"value1">>},
    %  #{<<"key1">> => <<"value2">>},
    %  #{<<"key2">> => <<"value1">>},
    %  #{<<"key1">> => <<"value1">>, #{<<"key2">> => <<"value1">>}
    %  #{<<"key1">> => <<"value2">>, #{<<"key2">> => <<"value1">>}
    % NOTE: calculation of all combinations can take same time (because of naïve implementation),
    % so it is not recommended to use with more than 10 total correct values for optional keys.
    optional_values_data_sets = relaxed :: relaxed | all_combinations,
    % Control how correct value is chosen for each parameter when building bad data sets
    % (data sets with all values correct but one with substituted bad value)
    selecting_correct_values_for_bad_data_sets_policy = random :: first | random
}).

-record(rest_args, {
    method :: get | patch | post | put | delete,
    path :: binary(),
    headers = #{} :: #{Key :: binary() => Value :: binary()},
    body = <<>> :: binary()
}).

-record(api_test_ctx, {
    scenario_name :: binary(),
    scenario_type :: api_test_runner:scenario_type(),
    node :: node(),
    client :: api_test_runner:api_client(),
    data :: map()
}).

-record(scenario_spec, {
    name :: binary(),
    type :: api_test_runner:scenario_type(),
    target_nodes :: api_test_runner:target_nodes(),
    client_spec :: api_test_runner:client_spec(),

    setup_fun = fun() -> ok end :: api_test_runner:setup_fun(),
    teardown_fun = fun() -> ok end :: api_test_runner:teardown_fun(),
    verify_fun = fun(_, _) -> true end :: api_test_runner:verify_fun(),

    prepare_args_fun :: api_test_runner:prepare_args_fun(),
    validate_result_fun :: api_test_runner:validate_call_result_fun(),

    data_spec = undefined :: undefined | api_test_runner:data_spec(),

    % When enabled, REST requests to onepanel will be made randomly
    % on the native onepanel endpoint (port 9443), or via the proxy hosted by op-worker/oz-worker.
    % When disabled, only the native endpoint will be tested (use for tests on undeployed environment)
    test_proxied_onepanel_rest_endpoint = true :: boolean()
}).

% Template used to create scenario_spec(). It contains scenario specific data
% while args/params repeated for all scenarios of some type/group can be
% extracted and kept in e.g. suite_spec().
-record(scenario_template, {
    name :: binary(),
    type :: api_test_runner:scenario_type(),
    prepare_args_fun :: api_test_runner:prepare_args_fun(),
    validate_result_fun :: api_test_runner:validate_call_result_fun(),
    test_proxied_onepanel_rest_endpoint = true :: boolean()
}).

% Record used to group scenarios having common parameters like target nodes,
% client spec or check functions. List of scenario_spec() will be created from
% that common params as well as scenario specific data contained in each
% scenario_template().
-record(suite_spec, {
    target_nodes :: api_test_runner:target_nodes(),
    client_spec :: api_test_runner:client_spec(),

    setup_fun = fun() -> ok end :: api_test_runner:setup_fun(),
    teardown_fun = fun() -> ok end :: api_test_runner:teardown_fun(),
    verify_fun = fun(_, _) -> true end :: api_test_runner:verify_fun(),

    scenario_templates = [] :: [api_test_runner:scenario_template()],
    test_case_generation_policy = full :: api_test_runner:test_case_generation_policy(),

    test_proxied_onepanel_rest_endpoint = true :: boolean(),

    data_spec = undefined :: undefined | api_test_runner:data_spec()
}).

-define(SCENARIO_NAME, atom_to_binary(?FUNCTION_NAME, utf8)).

-define(REST_ERROR(__ERROR), #{<<"error">> => errors:to_json(__ERROR)}).

-define(REST_PATH_PREFIX, "/api/v3/onepanel/").

-endif.
