-module(hbq).
-import(dlq, [get_max_number/1, add/3]).
-export([createNew/0, add/4]).

% listenformat [{Nachricht, Nr}]

createNew() -> [].

add(Message, Number, HBQ, DLQ) -> 

  % Nachricht und Nummer werden in die Holdbackqueue geschrieben.
  HBQwithNewMessage = lists:append(HBQ, [{Message, Number}]), 

  %sortieren
  SortedHBQ = lists:keysort(2, HBQwithNewMessage),

  %% Danach wird geprüft, ob Lücken geschlossen werden müssen.
  {New_HBQ, New_DLQ} = close_holes_if_necessary(SortedHBQ, DLQ),
  % Nach der Überprüfung, werden die Nachrichten bis zur nächsten Lücke in die DeliveryQueue geschoben

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
      HBQwithNewMessage = lists:append(HBQ, [createErrorMessage(HBQNumber, LastInDLQ)]),
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

  case FirstInHBQ - LastInDLQ of 
    1 -> push_messages_to_dlq(FirstInHBQ-1, HBQ, DLQ);
    _ -> {HBQ, DLQ}
  end.

push_messages_to_dlq(Number, HBQ, DLQ) ->
  {Element, RestHBQ} = pop(Number, HBQ),

  case Element of
    {_, nil}  -> 
      {RestHBQ, DLQ};
    {Message, ElementNumber} -> 
      NewDLQ = dlq:add(Message, ElementNumber, DLQ),
      push_messages_to_dlq(ElementNumber, RestHBQ, NewDLQ)
  end.

pop(_, []) -> 
  {{nothing, nil}, []};
pop(LastElement, [{Message, Number}|Rest]) when LastElement + 1 == Number -> 
  {{Message, Number}, Rest};
pop(_, List) -> 
  {{nothing, nil}, List}.

getFirstNumber([{_,HBQNumber}|_]) -> HBQNumber.

%todo test greater holes
createErrorMessage(Holestart, Holeend) -> 
  case Holeend of
    0 -> MessageNumber = 1;
    _ -> MessageNumber = Holeend + 1
  end,
  {{"Holestart bis Holeend", MessageNumber}, MessageNumber}. 