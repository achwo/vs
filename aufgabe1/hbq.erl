-module(hbq).
-import(dlq, [get_max_number/1, add/3]).
-export([createNew/0, add/4]).

% listenformat [{Nachricht, Nr}]
% TODO add(Msg, Nr, HBQ, DLQ) :: .. -> HBQ x DLQ


createNew() -> [].

add(Message, Number, HBQ, DLQ) -> 

  % Nachricht und Nummer werden in die Holdbackqueue geschrieben.
  HBQwithNewMessage = lists:append(HBQ, [{Message, Number}]), 

  %sortieren
  SortedHBQ = lists:keysort(2, HBQwithNewMessage),

  %% Danach wird geprüft, ob Lücken geschlossen werden müssen.

%  io:fwrite ("HBQwithNewMessage ~p ", [HBQwithNewMessage]),
 % io:fwrite ("SortedHBQ ~p ", [SortedHBQ]),


  {New_HBQ, New_DLQ} = close_holes_if_necessary(SortedHBQ, DLQ),
  % Nach der Überprüfung, werden die Nachrichten bis zur nächsten Lücke in die DeliveryQueue geschoben


  %io:fwrite ("New_HBQ ~p ", [New_HBQ]),
  %io:fwrite ("New_DLQ ~p~n", [New_DLQ]),
  push_messages_to_dlq(New_HBQ, New_DLQ).


close_holes_if_necessary(HBQ, DLQ) -> 
  %   - luecken muessen geschlossen werden, wenn mehr als maxsize(dlq) / 2; dann:
  {_, DLQ_max_size} = application:get_env(server, dlq_max_size),
  CloseHoles = length(HBQ) > DLQ_max_size/2,
  case CloseHoles of
    true ->
      %   - kleinsten wert in hbq holen
      HBQNumber = getFirstNumber(HBQ),
      %   - groessten wert in dlq holen
      case DLQ == [] of
        true  -> LastInDLQ = 0; 
        false -> {_, LastInDLQ} = dlq:getLastMsgNr(DLQ)
      end,
      %   - fehlernachricht wird erzeugt und in hbq getan
      HBQwithNewMessage = lists:append(HBQ, [{createErrorMessage(HBQNumber, LastInDLQ), 1}]),
      SortedHBQ = lists:keysort(2, HBQwithNewMessage);


    false -> SortedHBQ = HBQ
  end,

  % Rückgabe
  {SortedHBQ, DLQ}.

push_messages_to_dlq(HBQ, DLQ) -> 
  case DLQ == [] of
    true  -> LastInDLQ = 0; 
    false -> {_, LastInDLQ} = dlq:getLastMsgNr(DLQ)
  end,
  FirstInHBQ = getFirstNumber(HBQ),
%io:fwrite ("LastInDLQ ~p ", [LastInDLQ]),
%      io:fwrite ("FirstInHBQ ~p~n", [FirstInHBQ]),

  case FirstInHBQ - LastInDLQ of 
    1 -> push_messages_to_dlq(FirstInHBQ-1, HBQ, DLQ);
    _ -> {HBQ, DLQ}
  end.

push_messages_to_dlq(Number, HBQ, DLQ) ->
  % todo pop pruefen
  {Element, RestHBQ} = pop(Number, HBQ),

      %io:fwrite ("gepop'd ~p ", [{Number, Element, RestHBQ}]),

  case Element of
    {_, nil}  -> 
      %io:fwrite ("nil fall, RestHBQ ~p ", [RestHBQ]),
      %io:fwrite ("DLQ ~p~n", [DLQ]),
      {RestHBQ, DLQ};
    {Message, ElementNumber} -> 
      %io:fwrite ("anderer fall, RestHBQ ~p ", [RestHBQ]),
      %io:fwrite ("DLQ ~p ", [DLQ]),
      %io:fwrite ("Message ~p ", [Message]),
      %io:fwrite ("ElementNumber ~p ", [ElementNumber]),
      NewDLQ = dlq:add(Message, ElementNumber, DLQ),
      %io:fwrite ("NewDLQ ~p ", [NewDLQ]),
      %io:fwrite ("RestHBQ ~p~n", [RestHBQ]),
      push_messages_to_dlq(ElementNumber, RestHBQ, NewDLQ)
  end.

pop(_, []) -> 
  %io:fwrite ("empty list"),
  {{nothing, nil}, []};
pop(LastElement, [{Message, Number}|Rest]) when LastElement + 1 == Number -> 
    %io:fwrite ("treffer fall, Rest ~p ", [Rest]),
      %io:fwrite ("LastElement ~p ", [LastElement]),
      %io:fwrite ("Number ~p~n", [Number]),
  {{Message, Number}, Rest};
%pop(LastElement, [_|Rest]) -> 
pop(_, List) -> 
      %io:fwrite ("LastElement ~p ", [LastElement]),
      %io:fwrite ("kein treffer fall Rest ~p~n", [Rest]),
  {{nothing, nil}, List}.

getFirstNumber([{_,HBQNumber}|_]) -> HBQNumber.


%todo 1 ist gefaked
createErrorMessage(Holestart, Holeend) -> {"Holestart bis Holeend", 1}. 