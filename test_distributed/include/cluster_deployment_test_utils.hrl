%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of records used for cluster deployment configuration in tests.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(CLUSTER_DEPLOYMENT_TEST_UTILS_HRL).
-define(CLUSTER_DEPLOYMENT_TEST_UTILS_HRL, 1).


-record(node_details, {
    node :: node(),
    hostname :: binary(),
    ip :: ip_utils:ip()
}).

-record(op_cluster_config, {
    %% Cluster topology
    nodes :: #{non_neg_integer() => cluster_deployment_test_utils:node_details()},
    managers :: [non_neg_integer()],
    main_manager :: non_neg_integer(),
    workers :: [non_neg_integer()],
    databases :: [non_neg_integer()],
    ones3_nodes = [] :: [non_neg_integer()],
    ones3_port = undefined :: undefined | non_neg_integer(),

    %% Provider configuration
    name :: binary(),
    admin_email :: binary(),

    %% Registration
    register = false :: boolean(),
    registration_token :: binary() | undefined,

    %% Domain configuration
    subdomain_delegation = false :: boolean(),
    subdomain :: binary() | undefined,
    domain :: binary() | undefined,

    %% Web cert configuration
    lets_encrypt = false :: boolean()
}).


-endif.