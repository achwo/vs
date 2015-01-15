-module(free_slot_list_tests).
-include_lib("eunit/include/eunit.hrl").

-define(L, free_slot_list).
-define(TEST_LIST, [4, 3, 2, 1]).

all_test_() -> [
    test_new()
  , test_reserveSlot()
  % , test_reserveRandomSlot()
  , test_reserveLastFreeSlot()
].

test_new() -> [
    ?_assertEqual([4, 3, 2, 1], ?L:new(4))
].

test_reserveSlot() -> [
    ?_assertEqual([4, 3, 1], ?L:reserveSlot(2, ?TEST_LIST))
  , ?_assertEqual([3, 2, 1], ?L:reserveSlot(4, ?TEST_LIST))
 % negative tests?
].

% test_reserveRandomSlot() -> [
%     {Elem, List} = ?L:reserveRandomSlot(?TEST_LIST)
%   , ?_assertEqual(3, length(List))
% ].

test_reserveLastFreeSlot() -> [
  ?_assertEqual({4, [3, 2, 1]}, ?L:reserveLastFreeSlot(?TEST_LIST))
].