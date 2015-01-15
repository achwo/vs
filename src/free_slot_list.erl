-module(free_slot_list).
-export([new/1, reserveSlot/2, reserveRandomSlot/1, readdReservedSlot/2,
  slotsAfter/2]).

new(NumberSlots) -> createFreeSlotList([], 1, NumberSlots).

slotsAfter(Slot, FreeSlotList) ->
  [S || S <- FreeSlotList, S >= Slot].

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

readdReservedSlot(Slot, FreeSlotList) ->
  [Slot|FreeSlotList].