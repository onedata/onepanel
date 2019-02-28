%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for upgrade functions of db models.
%%% @end
%%%--------------------------------------------------------------------
-module(model_upgrade_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================


onepanel_session_upgrade_1_2_test() ->
    Previous = {onepanel_session,
        <<"id">>,
        <<"username">>,
        123
    },
    Expected = {onepanel_session,
        <<"id">>,
        <<"username">>,
        0,
        <<"">>,
        <<"">>,
        <<"">>
    },
    ?assertEqual({2, Expected}, onepanel_session:upgrade(1, Previous)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.
