-module(free_slot_list).
-export([new/1, reserveSlot/2, reserveRandomSlot/1, reserveLastFreeSlot/1]).

new(NumberSlots) -> createFreeSlotList([], 1, NumberSlots).

createFreeSlotList(List, CurrentSlotNumber, TotalNumber) 
  when CurrentSlotNumber >= TotalNumber + 1 ->
  List;
createFreeSlotList(List, CurrentSlotNumber, TotalNumber) ->
  createFreeSlotList(
    [CurrentSlotNumber | List], 
    CurrentSlotNumber + 1, 
    TotalNumber).

reserveSlot(Slot, FreeSlotList) ->
  lists:delete(Slot, FreeSlotList).

reserveRandomSlot(FreeSlotList) -> 
  Index = random:uniform(length(FreeSlotList)),
  Slot = lists:nth(Index, FreeSlotList),
  NewFreeSlotList = lists:delete(Slot, FreeSlotList),
  {Slot, NewFreeSlotList}.

reserveLastFreeSlot([Slot|Rest]) ->
  {Slot, Rest}.
  % Slot = lists:last(FreeSlotList),
  % NewFreeSlotList = lists:delete(Slot, FreeSlotList),
  % {Slot, NewFreeSlotList}.