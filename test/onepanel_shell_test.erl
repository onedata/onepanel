%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for onepanel_shell module.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_shell_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test functions
%%%===================================================================

token_quote_test_() ->
    lists:map(fun({Token, QuotedToken}) ->
        ?_assertEqual(QuotedToken, shell_utils:quote(Token))
    end, [
        {<<"token">>, <<"'token'">>},
        {<<"white space">>, <<"'white space'">>},
        {<<"with 'quote'">>, <<"'with \\'quote\\''">>},
        {atom, <<"'atom'">>},
        {5, <<"'5'">>},
        {"list", <<"'list'">>}
    ]).


-endif.

