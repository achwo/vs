-module(dlq_tests).
-include_lib("eunit/include/eunit.hrl").

createNew_test_() ->
  ?_assertEqual([], dlq:createNew()).


%get_test_() ->
%  test_get_empty_list().

test_get_empty_list() ->
  ?_assertEqual(false, dlq:get(1, [])).