-module(hbq).
-import(werkzeug, [to_String/1]).
-export([createNew/0, add/4]).
-export([pop/2, createErrorMessage/2, push_messages_to_dlq/2, close_holes_if_necessary/2]).

% Seite 5 3.4.2.1 -> createNew() :: void -> HBQ
% Ein neues HBQ Objekt wird erstellt und zurückgegeben. Agiert wie ein Konstruktor.
createNew() -> [].

% Seite 5 3.4.2.1 -> add(Msg, Nr, HBQueue, DLQueue) :: Nachricht x Nummer x HBQ x DLQ -> HBQ x DLQ
% Die Nachricht der Queue hinzugefügt und die Queue nach Nachrichtennummer aufsteigend sortiert. 
% Wenn die Queue eine Größe erreicht die der Hälfte der Größe der DLQ entspricht werden Lücken 
% zwangsweise geschlossen und die DLQ nachgefüllt.
add(Message, Number, HBQ, DLQ) -> 
  
  %Eingang in die hbq wird dokumentiert
  New_Message = lists:concat([Message, "; HBQ In: ",werkzeug:timeMilliSecond()]),
  % Nachricht und Nummer werden in die Holdbackqueue geschrieben.
  HBQwithNewMessage = lists:append(HBQ, [{New_Message, Number}]), 
  SortedHBQ = lists:keysort(2, HBQwithNewMessage),

  % Danach wird geprueft, ob Luecken geschlossen werden muessen.
  {_, DLQ_max_size} = application:get_env(server, dlqlimit),
  % luecken muessen geschlossen werden, wenn mehr als maxsize(dlq) / 2
  case (length(SortedHBQ) > DLQ_max_size/2) of
    true -> {New_HBQ, New_DLQ} = close_holes_if_necessary(SortedHBQ, DLQ);
    false -> {New_HBQ, New_DLQ} = {SortedHBQ, DLQ}
  end,
  % Nach der Ueberpruefung, werden die Nachrichten bis zur nächsten Luecke in die DLQ geschoben
  push_messages_to_dlq(New_HBQ, New_DLQ).

% close_holes_if_necessary(HBQueue, DLQueue) :: HBQ x DLQ -> {HBQ, DLQ}
close_holes_if_necessary(HBQ, DLQ) -> 
  %   - kleinsten wert in hbq holen
  FirstInHBQ = getFirstNumber(HBQ),
  %   - groessten wert in dlq holen
  case DLQ == [] of
    true  -> LastInDLQ = 0; 
    false -> {_, LastInDLQ} = dlq:getLastMsgNr(DLQ)
  end,

  case FirstInHBQ - LastInDLQ of 
    1 -> DLQwithErrorMessage = DLQ;
    _ -> 
      %   - fehlernachricht wird erzeugt und in hbq getan
      {ErrorMessage, ErrorNumber} = createErrorMessage(LastInDLQ, FirstInHBQ),
      DLQwithErrorMessage = dlq:add(ErrorMessage, ErrorNumber, DLQ)
  end,

  % Rueckgabe
  {HBQ, DLQwithErrorMessage}.

% push_messages_to_dlq(HBQueue, DLQueue) :: HBQ x DLQ -> {HBQ , DLQ, List}
push_messages_to_dlq(HBQ, DLQ) -> 
  case DLQ == [] of
    true  -> LastInDLQ = 0; 
    false -> {_, LastInDLQ} = dlq:getLastMsgNr(DLQ)
  end,
  FirstInHBQ = getFirstNumber(HBQ),

  case FirstInHBQ - LastInDLQ of 
    1 -> push_messages_to_dlq(FirstInHBQ-1, HBQ, DLQ, []);
    _ -> {HBQ, DLQ, []}
  end.

% push_messages_to_dlq(Integer, HBQueue, DLQueue, Integer) :: MessageNumber x HBQ x DLQ x DeliveredNumbers -> {HBQ , DLQ, List}
push_messages_to_dlq(Number, HBQ, DLQ, TransferedNumbers) ->
  {Element, RestHBQ} = pop(Number, HBQ),

  case Element of
    {_, nil}  -> 
      {RestHBQ, DLQ, TransferedNumbers};
    {Message, ElementNumber} -> 
      NewTransferedNumbers = lists:concat([TransferedNumbers, [ElementNumber]]),
      NewDLQ = dlq:add(Message, ElementNumber, DLQ),
      push_messages_to_dlq(ElementNumber, RestHBQ, NewDLQ, NewTransferedNumbers)
  end.

% pop(Number, Queue) :: Integer x HBQ -> {{Message, Number}, HBQ}
% pops one message from list, stopping at holes
% LastElement = element number, after which should be popped
pop(_, []) -> 
  {{nothing, nil}, []};
pop(LastElement, [{Message, Number}|Rest]) when LastElement + 1 == Number -> 
  {{Message, Number}, Rest};
pop(_, List) -> 
  {{nothing, nil}, List}.

% getFirstNumber(Queue) :: Queue -> Number
getFirstNumber([{_,HBQNumber}|_]) -> HBQNumber.

% createErrorMessage(LastInDLQNumber, FirstInHBQ) :: Integer x Integer -> {Text, Number} 
createErrorMessage(LastInDLQ, FirstInHBQ) when FirstInHBQ - LastInDLQ == 2 ->
  HoleNumber = FirstInHBQ - 1,
  MessageText = to_String(HoleNumber),
  {MessageText, HoleNumber};
createErrorMessage(LastInDLQ, FirstInHBQ) -> 
  FirstMissing = LastInDLQ + 1,
  LastMissing = FirstInHBQ - 1, 
  MessageText = lists:concat(["**Fehlernachricht fuer Nachrichten ", to_String(FirstMissing), " bis ", to_String(LastMissing), "um ", werkzeug:timeMilliSecond()]),
  {MessageText, LastMissing}. 

