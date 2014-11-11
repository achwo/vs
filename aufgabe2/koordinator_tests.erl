-module(koordinator_tests).
-include_lib("eunit/include/eunit.hrl").

create_ring_test_() ->
  [ 
   test_no_element_in_list(),
   test_one_element_in_list(),
   test_more_in_list(),
   test_no_latest_and_first(),
   test_integration()
  ].

test_no_element_in_list() ->
  ?_assertEqual(accu, koordinator:create_ring_tuples([], 4, 1, accu)).

test_one_element_in_list() ->
  [ ?_assertEqual([{4, 3, 1}], 
      koordinator:create_ring_tuples([4], 3, 1, [])),
    ?_assertEqual([{4, 3, 1}, {3, 2, 4}, {2, 1, 3}, {1, 4, 2}], 
      koordinator:create_ring_tuples([4], 3, 1, [{3, 2, 4}, {2, 1, 3}, {1, 4, 2}]))
  ].

test_more_in_list() ->
  ?_assertEqual([{4, 3, 1}, {3, 2, 4}], 
    koordinator:create_ring_tuples([3, 4], 2, 1, [])).

test_no_latest_and_first() ->
  ?_assertEqual([{3, 2, 1}, {2, 1, 3}, {1, 3, 2}], 
   koordinator:create_ring_tuples([1, 2, 3], none, none, [])).

test_integration() ->
  ?_assertEqual([{4, 3, 1}, {3, 2, 4}, {2, 1, 3}, {1, 4, 2}],
    koordinator:create_ring_tuples([1, 2, 3, 4], none, none, [])).