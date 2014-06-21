%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains custom n2o elements.
%% @end
%% ===================================================================

-ifndef(CUSTOM_ELEMENTS_HRL).
-define(CUSTOM_ELEMENTS_HRL, 1).

% Custom jquery action
-record(jquery, {?ACTION_BASE(action_jquery),
  property,
  method,
  args = [],
  right,
  format = "~s"}).

% Custom checkbox element
-record(custom_checkbox, {?ELEMENT_BASE(element_custom_checkbox),
  checked = false,
  value = "on",
  postback,
  delegate,
  disabled,
  name}).

-endif.