%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Framework for running API (REST) tests. It takes as an input test
%%% specification, which includes:
%%% - target nodes - list of nodes to which api calls can be directed,
%%% - client spec - specification of which clients are allowed, forbidden
%%%                 and unauthorized to perform operation,
%%% - data spec - specification of operation parameters,
%%% - hooks - functions called before/after test case execution to perform
%%%           setup/teardown or verification,
%%% - callback functions - functions called to prepare args for given scenario
%%%                        and validate obtained result.
%%% Based on it test cases are constructed and run. Not all combinations are
%%% tested but only some randomly chosen subset (e.g. instead of repeating call
%%% on every node one is randomly chosen. Then in verification hook it can be
%%% checked that changes are visible also on other nodes).
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_runner).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/test/test_utils.hrl").


-export([run_tests/1]).

-type scenario_type() :: rest.
-type target_nodes() :: [node()].

-type api_client() :: #api_client{}.
-type api_client_placeholder() ::
    root | peer | guest | user |
    member | {member, Privileges :: [privileges:cluster_privilege()]}.
-type api_client_or_placeholder() :: api_client() | api_client_placeholder().

-type client_spec() :: #client_spec{}.
-type data_spec() :: #data_spec{}.

-type rest_args() :: #rest_args{}.

-type api_test_ctx() :: #api_test_ctx{}.

% Hook called before testcase. Can be used to create test environment.
-type setup_fun() :: fun(() -> ok).
% Hook called after testcase. Can be used to clear up environment.
-type teardown_fun() :: fun(() -> ok).
% Hook called after testcase. Can be used to check if test had desired effect
% on environment (e.g. check if resource deleted during test was truly deleted).
% First argument tells whether request made during testcase should succeed
-type verify_fun() :: fun(
    (RequestResultExpectation :: expected_success | expected_failure, api_test_ctx()) -> boolean()
).

% Function called during testcase to prepare call/request arguments.
% If test cannot be run due to e.g invalid combination of client,
% data and provider 'skip' atom should be returned instead to skip that
% specific testcase.
-type prepare_args_fun() :: fun((api_test_ctx()) -> skip | rest_args()).
% Function called after testcase to validate returned call/request result.
-type validate_call_result_fun() :: fun((api_test_ctx(), Result :: term()) -> ok | no_return()).

-type scenario_spec() :: #scenario_spec{}.
-type scenario_template() :: #scenario_template{}.
-type suite_spec() :: #suite_spec{}.

-export_type([
    scenario_type/0, target_nodes/0,
    api_client/0, api_client_placeholder/0, api_client_or_placeholder/0,
    client_spec/0, data_spec/0,
    rest_args/0,
    api_test_ctx/0,
    setup_fun/0, teardown_fun/0, verify_fun/0,
    prepare_args_fun/0, validate_call_result_fun/0,
    scenario_spec/0, scenario_template/0, suite_spec/0
]).

-define(NO_DATA, undefined).

% Time caveat is required in temporary tokens, a default one is added if there isn't any
-define(DEFAULT_TEMP_CAVEAT_TTL, 36000).

-define(REQUIRED_KEYS(Required),  lists:map(fun(
    {Key, _Error}) -> Key;
    (Key)-> Key
end, Required)).


%%%===================================================================
%%% API
%%%===================================================================


-spec run_tests([scenario_spec() | suite_spec()]) ->
    boolean().
run_tests(Specs) ->
    lists:foldl(fun
        (#scenario_spec{} = ScenarioSpec, AllTestsPassed) ->
            AllTestsPassed and run_suite(scenario_spec_to_suite_spec(ScenarioSpec));
        (#suite_spec{} = SuiteSpec, AllTestsPassed) ->
            AllTestsPassed and run_suite(SuiteSpec)
    end, true, Specs).


%%%===================================================================
%%% Run scenario test cases combinations functions
%%%===================================================================


%% @private
run_suite(#suite_spec{
    client_spec = ClientSpecWithPlaceholders,
    target_nodes = PanelNodes
} = SuiteSpec0) ->
    try
        ClientSpec = replace_client_placeholders_with_valid_clients(
            ClientSpecWithPlaceholders, PanelNodes
        ),
        SuiteSpec = SuiteSpec0#suite_spec{client_spec = ClientSpec},

        run_invalid_clients_test_cases(unauthorized, SuiteSpec)
        and run_invalid_clients_test_cases(forbidden, SuiteSpec)
        and run_malformed_data_test_cases(SuiteSpec)
        and run_missing_required_data_test_cases(SuiteSpec)
        and run_expected_success_test_cases(SuiteSpec)
    catch
        throw:fail ->
            false;
        Type:Reason ->
            ct:pal("Unexpected error while running test suite ~p:~p ~p", [
                Type, Reason, erlang:get_stacktrace()
            ]),
            false
    end.


%% @private
replace_client_placeholders_with_valid_clients(#client_spec{
    correct = CorrectClientsAndPlaceholders,
    unauthorized = UnauthorizedClientsAndPlaceholders,
    forbidden = ForbiddenClientsAndPlaceholders
}, PanelNodes) ->
    #client_spec{
        correct = replace_client_placeholders(
            CorrectClientsAndPlaceholders, PanelNodes
        ),
        unauthorized = replace_client_placeholders(
            UnauthorizedClientsAndPlaceholders, PanelNodes
        ),
        forbidden = replace_client_placeholders(
            ForbiddenClientsAndPlaceholders, PanelNodes
        )
    }.


%% @private
replace_client_placeholders(ClientsAndPlaceholders, PanelNodes) ->
    lists:map(fun
        ({ClientOrPlaceholder, {error, _} = Error}) ->
            {get_or_create_client(ClientOrPlaceholder, PanelNodes), Error};
        (ClientOrPlaceholder) ->
            get_or_create_client(ClientOrPlaceholder, PanelNodes)
    end, ClientsAndPlaceholders).


%% @private
get_or_create_client(guest, _PanelNodes) ->
    ?API_GUEST;

get_or_create_client(user, _PanelNodes) ->
    OzNode = hd(oct_background:get_zone_nodes()),

    UserId = ozw_test_rpc:create_user(),
    Token = create_oz_temp_token(OzNode, UserId),

    #api_client{role = user, token = Token};

get_or_create_client(member, PanelNodes) ->
    get_or_create_client({member, privileges:cluster_member()}, PanelNodes);

get_or_create_client({member, Privileges}, _PanelNodes) ->
    OzNode = hd(oct_background:get_zone_nodes()),
    UserId = ozw_test_rpc:create_user(),

    lists:foreach(fun(PanelNode) ->
        ClusterId = panel_test_rpc:get_cluster_id(PanelNode),
        ozw_test_rpc:add_user_to_cluster(ClusterId, UserId, Privileges)
    end, oct_background:get_all_panels()),


    Token = create_oz_temp_token(OzNode, UserId),

    #api_client{role = member, privileges = Privileges, token = Token};

get_or_create_client(peer, PanelNodes) ->
    Node = lists_utils:random_element(PanelNodes),
    Token = panel_test_rpc:create_invite_token(Node),
    #api_client{role = peer, token = Token};

get_or_create_client(root, PanelNodes) ->
    Username = ?LOCAL_USERNAME,
    Password = ?ONENV_EMERGENCY_PASSPHRASE,

    Hash = base64:encode(<<Username/binary, ":", Password/binary>>),
    BasicAuthHeaders = #{?HDR_AUTHORIZATION => <<"Basic ", Hash/binary>>},

    Node = lists_utils:random_element(PanelNodes),
    Token = obtain_local_token(Node, BasicAuthHeaders),

    #api_client{
        role = root,
        basic_credentials = {Username, Password},
        token = Token
    };

get_or_create_client(#api_client{} = ApiClient, _PanelNodes) ->
    ApiClient.


%% @private
run_invalid_clients_test_cases(InvalidClientsType, #suite_spec{
    target_nodes = TargetNodes,

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    scenario_templates = ScenarioTemplates,

    data_spec = DataSpec
} = SuiteSpec) ->

    ValidDataSets = required_data_sets(DataSpec),
    InvalidClientsAndReasons = get_invalid_clients(InvalidClientsType, SuiteSpec),

    TestCaseFun = fun(TargetNode, InvalidClientAndReason, DataSet, ScenarioTemplate) ->
        {InvalidClient, ExpError} = get_scenario_specific_error_for_invalid_clients(
            InvalidClientsType, InvalidClientAndReason
        ),
        run_exp_error_testcase(
            TargetNode, InvalidClient, DataSet, ExpError,
            VerifyFun, ScenarioTemplate
        )
    end,

    SetupFun(),
    TestsPassed = run_scenarios(
        ScenarioTemplates, TargetNodes, InvalidClientsAndReasons, ValidDataSets,
        TestCaseFun
    ),
    TeardownFun(),

    TestsPassed.


%% @private
get_invalid_clients(unauthorized, #suite_spec{client_spec = #client_spec{
    unauthorized = UnauthorizedClients
}}) ->
    UnauthorizedClients;
get_invalid_clients(forbidden, #suite_spec{client_spec = #client_spec{
    forbidden = ForbiddenClients
}}) ->
    ForbiddenClients.


%% @private
get_scenario_specific_error_for_invalid_clients(unauthorized, {Client, AuthError}) ->
    {Client, ?ERROR_UNAUTHORIZED(AuthError)};
get_scenario_specific_error_for_invalid_clients(unauthorized, Client) ->
    {Client, ?ERROR_UNAUTHORIZED};
get_scenario_specific_error_for_invalid_clients(forbidden, {Client, Error}) ->
    {Client, Error};
get_scenario_specific_error_for_invalid_clients(forbidden, Client) ->
    {Client, ?ERROR_FORBIDDEN}.


%% @private
run_malformed_data_test_cases(#suite_spec{
    target_nodes = TargetNodes,
    client_spec = #client_spec{correct = CorrectClients},

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    scenario_templates = ScenarioTemplates,

    data_spec = DataSpec
}) ->
    TestCaseFun = fun
        (_TargetNode, _Client, ?NO_DATA, _) ->
            % operations not requiring any data cannot be tested against
            % malformed data
            true;
        (TargetNode, Client, {DataSet, _BadKey, Error}, #scenario_template{
            name = ScenarioName,
            type = ScenarioType
        } = ScenarioTemplate) ->
            case is_data_error_applicable_to_scenario(Error, ScenarioType) of
                false ->
                    true;
                true ->
                    TestCaseCtx = build_test_ctx(
                        ScenarioName, ScenarioType, TargetNode, Client, DataSet
                    ),
                    run_exp_error_testcase(
                        TargetNode, Client, DataSet,
                        get_expected_malformed_data_error(Error, ScenarioType, TestCaseCtx),
                        VerifyFun, ScenarioTemplate
                    )
            end
    end,

    SetupFun(),
    TestsPassed = run_scenarios(
        ScenarioTemplates, TargetNodes, CorrectClients, bad_data_sets(DataSpec),
        TestCaseFun
    ),
    TeardownFun(),

    TestsPassed.


%% @private
is_data_error_applicable_to_scenario({error, _}, _)           -> true;
is_data_error_applicable_to_scenario({Scenario, _}, Scenario) -> true;
is_data_error_applicable_to_scenario(_, _)                    -> false.


%% @private
get_expected_malformed_data_error({error, _} = Error, _, _) ->
    Error;
get_expected_malformed_data_error({error_fun, ErrorFun}, _, TestCaseCtx) ->
    ErrorFun(TestCaseCtx);
get_expected_malformed_data_error({_ScenarioType, {error, _} = ScenarioSpecificError}, _, _) ->
    ScenarioSpecificError;
get_expected_malformed_data_error({_ScenarioType, {error_fun, ErrorFun}}, _, TestCaseCtx) ->
    ErrorFun(TestCaseCtx).


%% @private
run_missing_required_data_test_cases(#suite_spec{data_spec = undefined}) ->
    true;
run_missing_required_data_test_cases(#suite_spec{data_spec = #data_spec{
    required = [],
    at_least_one = []
}}) ->
    true;
run_missing_required_data_test_cases(#suite_spec{
    target_nodes = TargetNodes,
    client_spec = #client_spec{correct = CorrectClients},

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    scenario_templates = ScenarioTemplates,

    data_spec = DataSpec = #data_spec{
        required = RequiredParams,
        at_least_one = AtLeastOneParams
    }
}) ->
    RequiredDataSets = required_data_sets(DataSpec),
    RequiredDataSet = hd(RequiredDataSets),

    MissingRequiredParamsDataSetsAndErrors = lists:map(fun
        ({RequiredParam, CustomError}) ->
            {maps:remove(RequiredParam, RequiredDataSet), CustomError};
        (RequiredParam) ->
            {maps:remove(RequiredParam, RequiredDataSet), ?ERROR_MISSING_REQUIRED_VALUE(RequiredParam)}
    end, RequiredParams),

    MissingAtLeastOneParamsDataSetAndError = case AtLeastOneParams of
        [] ->
            [];
        _ ->
            ExpectedError = ?ERROR_MISSING_AT_LEAST_ONE_VALUE(lists:sort(AtLeastOneParams)),
            [{maps:without(AtLeastOneParams, RequiredDataSet), ExpectedError}]
    end,

    IncompleteDataSetsAndErrors = lists:flatten([
        MissingRequiredParamsDataSetsAndErrors,
        MissingAtLeastOneParamsDataSetAndError
    ]),

    TestCaseFun = fun(TargetNode, Client, {DataSet, MissingParamError}, ScenarioTemplate) ->
        run_exp_error_testcase(
            TargetNode, Client, DataSet, MissingParamError,
            VerifyFun, ScenarioTemplate
        )
    end,

    SetupFun(),
    TestsPassed = run_scenarios(
        ScenarioTemplates, TargetNodes, CorrectClients, IncompleteDataSetsAndErrors,
        TestCaseFun
    ),
    TeardownFun(),

    TestsPassed.


%% @private
run_expected_success_test_cases(#suite_spec{
    target_nodes = TargetNodes,
    client_spec = #client_spec{correct = CorrectClients},

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    scenario_templates = ScenarioTemplates,
    randomly_select_scenarios = true,

    data_spec = DataSpec,
    data_spec_random_coverage = DataSpecRandomCoverage
}) ->
    CorrectDataSets = correct_data_sets(DataSpec, DataSpecRandomCoverage),

    lists:foldl(fun(Client, OuterAcc) ->
        CorrectDataSetsNum = length(CorrectDataSets),
        AvailableScenariosNum = length(ScenarioTemplates),
        ScenarioPerDataSet = case AvailableScenariosNum > CorrectDataSetsNum of
            true ->
                ct:fail("Not enough data sets compared to available scenarios");
            false ->
                RandomizedScenarios = lists:flatmap(
                    fun(_) -> lists_utils:shuffle(ScenarioTemplates) end,
                    lists:seq(1, CorrectDataSetsNum div AvailableScenariosNum + 1)
                ),
                lists:zip(
                    lists:sublist(RandomizedScenarios, CorrectDataSetsNum),
                    CorrectDataSets
                )
        end,
        OuterAcc and lists:foldl(fun({Scenario, DataSet}, InnerAcc) ->
            TargetNode = lists_utils:random_element(TargetNodes),

            SetupFun(),
            TestCasePassed = run_exp_success_testcase(
                TargetNode, Client, DataSet, VerifyFun, Scenario
            ),
            TeardownFun(),

            InnerAcc and TestCasePassed
        end, true, ScenarioPerDataSet)
    end, true, CorrectClients);
run_expected_success_test_cases(#suite_spec{
    target_nodes = TargetNodes,
    client_spec = #client_spec{correct = CorrectClients},

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    scenario_templates = ScenarioTemplates,
    randomly_select_scenarios = false,

    data_spec = DataSpec,
    data_spec_random_coverage = DataSpecRandomCoverage
}) ->
    TestCaseFun = fun(TargetNode, Client, DataSet, ScenarioTemplate) ->
        SetupFun(),
        TestCasePassed = run_exp_success_testcase(
            TargetNode, Client, DataSet, VerifyFun, ScenarioTemplate
        ),
        TeardownFun(),

        TestCasePassed
    end,

    run_scenarios(
        ScenarioTemplates, TargetNodes, CorrectClients, correct_data_sets(DataSpec, DataSpecRandomCoverage),
        TestCaseFun
    ).


%% @private
run_scenarios(ScenarioTemplates, TargetNodes, Clients, DataSets, TestCaseFun) ->
    lists:foldl(fun(ScenarioTemplate, PrevScenariosPassed) ->
        PrevScenariosPassed and lists:foldl(fun(Client, PrevClientsPassed) ->
            PrevClientsPassed and lists:foldl(fun(DataSet, PrevDataSetsPassed) ->
                TargetNode = lists_utils:random_element(TargetNodes),
                PrevDataSetsPassed and TestCaseFun(
                    TargetNode, Client, DataSet, ScenarioTemplate
                )
            end, true, DataSets)
        end, true, Clients)
    end, true, ScenarioTemplates).


%% @private
run_exp_error_testcase(
    TargetNode, Client, DataSet, ExpError, VerifyFun, #scenario_template{
        name = ScenarioName,
        type = ScenarioType,
        prepare_args_fun = PrepareArgsFun,
        test_proxied_onepanel_rest_endpoint = TestProxiedRestEndpoint
    }
) ->
    TestCaseCtx = build_test_ctx(ScenarioName, ScenarioType, TargetNode, Client, DataSet),

    case PrepareArgsFun(TestCaseCtx) of
        skip ->
            true;
        Args ->
            RequestResult = make_request(TargetNode, Client, TestProxiedRestEndpoint, Args),
            try
                validate_error_result(ScenarioType, ExpError, RequestResult),
                VerifyFun(expected_failure, TestCaseCtx)
            catch T:R ->
                log_failure(ScenarioName, TestCaseCtx, Args, ExpError, RequestResult, T, R),
                false
            end
    end.


%% @private
run_exp_success_testcase(TargetNode, Client, DataSet, VerifyFun, #scenario_template{
    name = ScenarioName,
    type = ScenarioType,
    prepare_args_fun = PrepareArgsFun,
    validate_result_fun = ValidateResultFun,
    test_proxied_onepanel_rest_endpoint = TestProxiedRestEndpoint
}) ->
    TestCaseCtx = build_test_ctx(ScenarioName, ScenarioType, TargetNode, Client, DataSet),
    case PrepareArgsFun(TestCaseCtx) of
        skip ->
            true;
        Args ->
            Result = make_request(TargetNode, Client, TestProxiedRestEndpoint, Args),
            try
                ValidateResultFun(TestCaseCtx, Result),
                VerifyFun(expected_success, TestCaseCtx)
            catch T:R ->
                log_failure(ScenarioName, TestCaseCtx, Args, success, Result, T, R),
                false
            end
    end.


%% @private
validate_error_result(rest, ExpError, {ok, RespCode, _RespHeaders, RespBody}) ->
    ?assertEqual(
        {errors:to_http_code(ExpError), ?REST_ERROR(ExpError)},
        {RespCode, RespBody}
    ).


%% @private
log_failure(ScenarioName, #api_test_ctx{
    node = TargetNode,
    client = Client
}, Args, Expected, Got, ErrType, ErrReason) ->
    ct:pal(
        "~s test case failed:~n"
        "Node: ~p~n"
        "Client: ~s~n"
        "Args: ~s~n"
        "Expected: ~p~n"
        "Got: ~p~n"
        "Error: ~p:~p~n"
        "Stacktrace: ~p~n",
        [
            ScenarioName,
            TargetNode,
            io_lib_pretty:print(Client, fun get_record_def/2),
            io_lib_pretty:print(Args, fun get_record_def/2),
            Expected,
            Got,
            ErrType, ErrReason,
            erlang:get_stacktrace()
        ]
    ).


%%%===================================================================
%%% Prepare data combinations functions
%%%===================================================================


% Returns data sets that are correct
correct_data_sets(undefined, _DataSpecRandomCoverage) ->
    [?NO_DATA];
correct_data_sets(DataSpec, DataSpecRandomCoverage) ->
    RequiredDataSets = required_data_sets(DataSpec),

    AllRequiredParamsDataSet = case RequiredDataSets of
        [] -> #{};
        _ -> hd(RequiredDataSets)
    end,
    AllRequiredWithOptionalDataSets = lists:map(fun(OptionalDataSet) ->
        maps:merge(AllRequiredParamsDataSet, OptionalDataSet)
    end, optional_data_sets(DataSpec)),

    AllDatasets = RequiredDataSets ++ AllRequiredWithOptionalDataSets,
    SelectedDatasetsSize = round(DataSpecRandomCoverage/100 * length(AllDatasets)),
    lists_utils:random_sublist(AllDatasets, SelectedDatasetsSize, SelectedDatasetsSize).


% Generates all combinations of "required" params and one "at_least_one" param
required_data_sets(undefined) ->
    [?NO_DATA];
required_data_sets(DataSpec) ->
    #data_spec{
        required = Required,
        at_least_one = AtLeastOne
    } = DataSpec,

    AtLeastOneWithValues = lists:flatten(lists:map(
        fun(Key) ->
            [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
        end, AtLeastOne)
    ),
    RequiredWithValues = lists:map(
        fun(Key) ->
            [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
        end, ?REQUIRED_KEYS(Required)
    ),
    RequiredCombinations = lists:foldl(
        fun(ValuesForKey, Acc) ->
            [maps:merge(A, B) || A <- ValuesForKey, B <- Acc]
        end, [#{}], RequiredWithValues
    ),
    RequiredWithOne = lists:flatten(lists:map(
        fun(ReqMap) ->
            [maps:merge(AtLeastOneMap, ReqMap) || AtLeastOneMap <- AtLeastOneWithValues]
        end, RequiredCombinations
    )),
    AllAtLeastOneMap = lists:foldl(fun maps:merge/2, #{}, AtLeastOneWithValues),
    RequiredWithAll = lists:map(
        fun(ReqMap) ->
            maps:merge(AllAtLeastOneMap, ReqMap)
        end, RequiredCombinations
    ),
    case AtLeastOne of
        [] -> RequiredCombinations;
        [_] -> RequiredWithOne;
        _ -> RequiredWithAll ++ RequiredWithOne
    end.


% Generates all combinations for optional params
optional_data_sets(undefined) ->
    [?NO_DATA];
optional_data_sets(#data_spec{optional = []}) ->
    [];
optional_data_sets(#data_spec{optional = Optional} = DataSpec) ->
    OptionalParamsWithValues = lists:flatten(lists:map(fun(Key) ->
        [#{Key => Val} || Val <- get_correct_value(Key, DataSpec)]
    end, Optional)),

    OptionalParamsCombinations = lists:usort(lists:foldl(fun(ParamWithValue, Acc) ->
        [maps:merge(Combination, ParamWithValue) || Combination <- Acc] ++ Acc
    end, [#{}], OptionalParamsWithValues)),

    lists:delete(#{}, OptionalParamsCombinations).


% Generates combinations of bad data sets by adding wrong values to
% correct data set (one set with correct values for all params).
bad_data_sets(undefined) ->
    [?NO_DATA];
bad_data_sets(#data_spec{
    required = Required,
    at_least_one = AtLeastOne,
    optional = Optional,
    bad_values = BadValues
} = DataSpec) ->

    AllCorrect = lists:foldl(fun(Param, Acc) ->
        Acc#{Param => hd(get_correct_value(Param, DataSpec))}
    end, #{}, ?REQUIRED_KEYS(Required) ++ AtLeastOne ++ Optional),

    lists:map(fun({Param, InvalidValue, ExpError}) ->
        Data = AllCorrect#{Param => InvalidValue},
        {Data, Param, ExpError}
    end, BadValues).


% Converts correct value spec into a value
get_correct_value(Key, #data_spec{correct_values = CorrectValues}) ->
    case maps:get(Key, CorrectValues) of
        Fun when is_function(Fun, 0) ->
            Fun();
        Value ->
            Value
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec create_oz_temp_token(node(), UserId :: binary()) -> tokens:serialized().
create_oz_temp_token(OzNode, UserId) ->
    Auth = ?USER(UserId),
    Now = ozw_test_rpc:timestamp_seconds(),

    Token = ozw_test_rpc:create_user_temporary_token(Auth, UserId, #{
        <<"type">> => ?ACCESS_TOKEN,
        <<"caveats">> => [#cv_time{valid_until = Now + ?DEFAULT_TEMP_CAVEAT_TTL}]
    }),

    {ok, SerializedToken} = tokens:serialize(Token),
    SerializedToken.


%% @private
-spec obtain_local_token(node(), http_client:headers()) -> binary().
obtain_local_token(Node, BasicAuthHeaders) ->
    LoginURL = get_onepanel_endpoint(Node, "login"),
    {ok, _, #{<<"set-cookie">> := CookieHeader}, _} = ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        make_rest_request(Node, post, LoginURL, BasicAuthHeaders, <<>>)
    ),

    SessionCookieKey = gui_session_plugin:session_cookie_key(),
    Cookies = hackney_cookie:parse_cookie(CookieHeader),
    {SessionCookieKey, SessionCookie} = proplists:lookup(SessionCookieKey, Cookies),

    PreauthorizeURL = get_onepanel_endpoint(Node, "gui-preauthorize"),
    CookieAuthHeaders = #{<<"cookie">> => <<SessionCookieKey/binary, "=", SessionCookie/binary>>},

    {ok, _, _, #{<<"token">> := Token}} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        make_rest_request(Node, post, PreauthorizeURL, CookieAuthHeaders, <<>>)
    ),
    Token.


%% @private
-spec scenario_spec_to_suite_spec(scenario_spec()) -> suite_spec().
scenario_spec_to_suite_spec(#scenario_spec{
    name = ScenarioName,
    type = ScenarioType,
    target_nodes = TargetNodes,
    client_spec = ClientSpec,

    setup_fun = SetupFun,
    teardown_fun = TeardownFun,
    verify_fun = VerifyFun,

    prepare_args_fun = PrepareArgsFun,
    validate_result_fun = ValidateResultFun,

    data_spec_random_coverage = DataSpecRandomCoverage,
    data_spec = DataSpec,

    test_proxied_onepanel_rest_endpoint = TestProxiedOnepanelRestEndpoint
}) ->
    #suite_spec{
        target_nodes = TargetNodes,
        client_spec = ClientSpec,

        setup_fun = SetupFun,
        teardown_fun = TeardownFun,
        verify_fun = VerifyFun,

        scenario_templates = [#scenario_template{
            name = ScenarioName,
            type = ScenarioType,
            prepare_args_fun = PrepareArgsFun,
            validate_result_fun = ValidateResultFun,
            data_spec_random_coverage = DataSpecRandomCoverage,
            test_proxied_onepanel_rest_endpoint = TestProxiedOnepanelRestEndpoint
        }],
        randomly_select_scenarios = false,

        test_proxied_onepanel_rest_endpoint = TestProxiedOnepanelRestEndpoint,
        data_spec_random_coverage = DataSpecRandomCoverage,

        data_spec = DataSpec
    }.


%% @private
-spec build_test_ctx(binary(), scenario_type(), node(), api_client(), map()) ->
    api_test_ctx().
build_test_ctx(ScenarioName, ScenarioType, TargetNode, Client, DataSet) ->
    #api_test_ctx{
        scenario_name = ScenarioName,
        scenario_type = ScenarioType,
        node = TargetNode,
        client = Client,
        data = DataSet
    }.


%% @private
-spec make_request(node(), api_client(), boolean(), rest_args()) ->
    {ok, Result :: term()} | {error, term()}.
make_request(Node, Client, TestProxiedRestEndpoint, #rest_args{
    method = Method,
    path = Path,
    headers = Headers,
    body = Body
}) ->
    URL = get_onepanel_rest_endpoint(Node, Path, TestProxiedRestEndpoint),
    HeadersWithAuth = maps:merge(Headers, get_rest_auth_headers(Client)),
    make_rest_request(Node, Method, URL, HeadersWithAuth, Body).


%% @private
-spec get_rest_auth_headers(api_client()) -> AuthHeaders :: map().
get_rest_auth_headers(#api_client{role = guest}) ->
    #{};
get_rest_auth_headers(#api_client{basic_credentials = BasicCredentials, token = Token}) ->
    Auth = lists_utils:random_element(lists:filter(fun(Elem) ->
        Elem =/= undefined
    end, [BasicCredentials, Token])),

    case Auth of
        {Username, Password} ->
            Hash = base64:encode(<<Username/binary, ":", Password/binary>>),
            #{?HDR_AUTHORIZATION => <<"Basic ", Hash/binary>>};
        AuthToken ->
            #{?HDR_X_AUTH_TOKEN => AuthToken}
    end.


%% @private
-spec make_rest_request(
    node(),
    http_client:method(),
    http_client:url(),
    http_client:headers(),
    http_client:request_body()
) ->
    {ok, RespCode :: non_neg_integer(), RespBody :: binary() | map()} |
    {error, term()}.
make_rest_request(Node, Method, URL, Headers, Body) ->
    CaCerts = panel_test_rpc:get_cert_chain_ders(Node),
    Opts = [{ssl_options, [{cacerts, CaCerts}]}, {recv_timeout, 20000}],

    case http_client:request(Method, URL, Headers, Body, Opts) of
        {ok, RespCode, RespHeaders, RespBody} ->
            case maps:get(<<"content-type">>, RespHeaders, undefined) of
                <<"application/json">> ->
                    {ok, RespCode, RespHeaders, json_utils:decode(RespBody)};
                _ ->
                    {ok, RespCode, RespHeaders, RespBody}
            end;
        {error, _} = Error ->
            Error
    end.


%% @private
-spec get_onepanel_endpoint(node(), ResourcePath :: string() | binary()) ->
    URL :: binary().
get_onepanel_endpoint(Node, ResourcePath) ->
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, test_web_cert_domain),
    str_utils:join_as_binaries(["https://", Domain, ":9443/", ResourcePath], <<>>).


%% @private
-spec get_onepanel_rest_endpoint(node(), ResourcePath :: string() | binary(), boolean()) ->
    URL :: binary().
get_onepanel_rest_endpoint(Node, ResourcePath, TestProxiedRestEndpoint) ->
    {ok, Domain} = test_utils:get_env(Node, ?APP_NAME, test_web_cert_domain),

    Port = case TestProxiedRestEndpoint of
        true ->
            % randomly select between testing direct request or proxied via Onezone/Oneprovider
            lists_utils:random_element([<<>>, <<":9443">>]);
        false ->
            <<":9443">>
    end,

    str_utils:join_as_binaries(
        ["https://", Domain, Port, ?REST_PATH_PREFIX, ResourcePath],
        <<>>
    ).


% Returns information about chosen records, such as fields,
% required to for example pretty print it
get_record_def(api_client, N) ->
    case record_info(size, api_client) - 1 of
        N -> record_info(fields, api_client);
        _ -> no
    end;
get_record_def(rest_args, N) ->
    case record_info(size, rest_args) - 1 of
        N -> record_info(fields, rest_args);
        _ -> no
    end;
get_record_def(data_spec, N) ->
    case record_info(size, data_spec) - 1 of
        N -> record_info(fields, data_spec);
        _ -> no
    end;
get_record_def(_, _) ->
    no.