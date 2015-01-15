-module(util_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIME, 1421323650360).
-define(TEST_TIME2, 1421323650039).

all_test_() -> [
    test_currentFrame()
  , test_currentFrameTime()
  , test_currentSlot()
  , test_timeTillNextSlot()
  , test_transmissionTime()
  , test_timeTillTransmission()
].

% current_time_test() ->
%   Time = ?TEST_TIME,
%   SyncManager = spawn(fun() ->
%     receive
%       {get_current_time, PID} ->
%         PID ! {current_time, Time}
%     end
%   end),
%   Time = util:current_time(SyncManager),
%   ok.

% test_() ->
  % [calculate_frame_test(), currentSlot_test()].

test_currentFrame() -> [
    ?_assertEqual(1421323650, util:currentFrame(?TEST_TIME))
  , ?_assertEqual(1421323650, util:currentFrame(?TEST_TIME2))
  , ?_assertEqual(1421331978, util:currentFrame(1421331978962))
].

test_currentFrameTime() -> [
    ?_assertEqual(360, util:currentFrameTime(?TEST_TIME))
  , ?_assertEqual(39, util:currentFrameTime(?TEST_TIME2))
].

test_currentSlot() -> [
    ?_assertEqual(1, util:currentSlot(39))
  , ?_assertEqual(2, util:currentSlot(40))
  , ?_assertEqual(10, util:currentSlot(?TEST_TIME))
  , ?_assertEqual(1, util:currentSlot(?TEST_TIME2))
  , ?_assertEqual(25, util:currentSlot(999))
  , ?_assertEqual(25, util:currentSlot(1421331978962))
].

test_timeTillNextSlot() -> [
    ?_assertEqual(1, util:timeTillNextSlot(39))
  , ?_assertEqual(40, util:timeTillNextSlot(0))
  , ?_assertEqual(40, util:timeTillNextSlot(?TEST_TIME))
  , ?_assertEqual(1, util:timeTillNextSlot(?TEST_TIME2))
  , ?_assertEqual(38, util:timeTillNextSlot(1421331978962))
].

% test_timeTillNextFrame() -> [
    % ?_assertEqual(1000, util:timeTillNextFrame(0))
  % , ?_assertEqual(1, util:timeTillNextFrame(999))
  % , ?_assertEqual(781, util:timeTillNextFrame(?TEST_TIME))
% ].

test_transmissionTime() -> [
  ?_assertEqual(1360, util:transmissionTime(10, 1))
].

test_timeTillTransmission() -> [
    ?_assertEqual(0, util:timeTillTransmission(1, 0))
  , ?_assertEqual(355, util:timeTillTransmission(10, 5)) 
  , ?_assertEqual(-20, util:timeTillTransmission(1, 20))
  , ?_assertEqual(600, util:timeTillTransmission(25, ?TEST_TIME))
  , ?_assertEqual(921, util:timeTillTransmission(25, ?TEST_TIME2))
  , ?_assertEqual(959, util:timeTillTransmission(25, 1))
  , ?_assertEqual(-39, util:timeTillTransmission(25, 999))
].