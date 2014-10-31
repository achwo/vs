-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

pop_test_() ->
  [test_empty_list(), 
   test_one_element(), 
   test_hole(),
   test_multiple_elements_without_holes(),
   test_only_pops_if_LastIndex_fits_to_first_element()
  ].

test_empty_list() ->
  ?_assertEqual({{nothing, nil}, []}, hbq:pop(bla, [])).

test_one_element() ->
  [?_assertEqual({{{"bla", 1}, 1}, []}, 
      hbq:pop(0, [{{"bla", 1}, 1}])),
   ?_assertEqual({{nothing,nil},[{{"bla",1}, 1}]}, 
      hbq:pop(1, [{{"bla", 1}, 1}]))
  ].

test_hole() ->
  ?_assertEqual({{nothing, nil}, [{message3, 3}]},
      hbq:pop(1, [{message3, 3}])).
    
test_multiple_elements_without_holes() ->
  [
    ?_assertEqual({{message1, 1}, [{message2, 2}]}, 
      hbq:pop(0, [{message1, 1}, {message2, 2}]))
    
  ].

test_only_pops_if_LastIndex_fits_to_first_element() ->
  ?_assertEqual({{nothing, nil}, [{message1, 1}, {message2, 2}]},
      hbq:pop(1, [{message1, 1}, {message2, 2}])).


createErrorMessage_test_() ->
  [test_hole_size_one(),
   test_larger_holes()
  ].

test_hole_size_one() ->
  [
    ?_assertEqual({{"1",1},1}, hbq:createErrorMessage(0, 2)),
    ?_assertEqual({{"2",2},2}, hbq:createErrorMessage(1, 3))
  ].

test_larger_holes() ->
  [
    ?_assertEqual({{"1 bis 2", 2}, 2}, hbq:createErrorMessage(0, 3)),
    ?_assertEqual({{"2 bis 4", 4}, 4}, hbq:createErrorMessage(1, 5))
  ].


push_messages_to_dlq_test_() ->
  application:set_env(server, dlqlimit, 10),
  [test_push_one_element(),
   test_push_multiple_elements()
  ].

test_push_one_element() ->
  [?_assertEqual({[],[{{"msg",1},1}]}, hbq:push_messages_to_dlq([{{"msg",1},1}],[])),
   ?_assertEqual({[{{"msg",2},2}], []}, hbq:push_messages_to_dlq([{{"msg",2},2}],[])),
   ?_assertEqual({[], [{{"msg",1},1}, {{"msg",2},2}]}, hbq:push_messages_to_dlq([{{"msg",2},2}],[{{"msg", 1}, 1}]))
  ].

test_push_multiple_elements() ->
  [?_assertEqual({[], [{{"msg",1},1}, {{"msg",2},2}]}, 
    hbq:push_messages_to_dlq([{{"msg", 1}, 1}, {{"msg",2},2}],[])),

   ?_assertEqual({[{{"msg",3},3}], [{{"msg",1},1}]}, 
    hbq:push_messages_to_dlq([{{"msg",1},1}, {{"msg",3},3}],[]))
  ].



close_holes_if_necessary_test_() ->
  [test_close_empty_lists(),
   test_close_one_element(),
   test_close_multiple_elements()
  ].

test_close_empty_lists() ->
  ?_assertEqual({[{{"msg",1},1}], []}, 
    hbq:close_holes_if_necessary([{{"msg",1},1}], [])).

test_close_one_element() ->
  [?_assertEqual({[{{"msg",2},2}], [{{"msg",1},1}]}, 
     hbq:close_holes_if_necessary([{{"msg",2},2}], [{{"msg",1},1}])),

   ?_assertEqual({[{{"msg",3},3}], [{{"msg",1},1}, {{"2", 2}, 2}]}, 
     hbq:close_holes_if_necessary([{{"msg",3},3}], [{{"msg",1},1}])),

   ?_assertEqual({[{{"msg",2},2}], [{{"1",1},1}]}, 
     hbq:close_holes_if_necessary([{{"msg",2},2}], []))
  ].  

test_close_multiple_elements() ->
  [?_assertEqual({[{{"msg",1},1}, {{"msg",2},2}], []}, 
    hbq:close_holes_if_necessary([{{"msg",1},1}, {{"msg",2},2}], [])),

   ?_assertEqual({[{{"msg",2},2}, {{"msg",3},3}], [{{"1",1},1}]}, 
    hbq:close_holes_if_necessary([{{"msg",2},2}, {{"msg",3},3}], [])),

   ?_assertEqual({[{{"msg",2},2}, {{"msg",4},4}], [{{"1",1},1}]}, 
    hbq:close_holes_if_necessary([{{"msg",2},2}, {{"msg",4},4}], []))
  ].

