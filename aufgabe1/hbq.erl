-module(hbq).
-import(dlq, [get_max_number/1]).
-export([]).

% listenformat [{Nachricht, Nr}]
% TODO add(Msg, Nr, HBQ, DLQ) :: .. -> HBQ x DLQ


createNew() -> [].



add(Message, Number, HBQ, DLQ) -> 

  % Nachricht und Nummer werden in die Holdbackqueue geschrieben.
  HBQ_with_new_message = lists:append(HBQ, {Message, Number}), 
  %% Danach wird geprüft, ob Lücken geschlossen werden müssen.
  {New_HBQ, New_DLQ} = close_holes_if_necessary(HBQ_with_new_message, DLQ),
  % Nach der Überprüfung, werden die Nachrichten bis zur nächsten Lücke in die DeliveryQueue geschoben
  push_messages_to_dlq(New_HBQ, New_DLQ).


close_holes_if_necessary(HBQ, DLQ) -> 
  %   - luecken muessen geschlossen werden, wenn mehr als size(dlq) / 2; dann:
  Size = length(HBQ) > length(DLQ)/2,
  if Size ->
    %   - kleinsten wert in hbq holen
    HBQNumber = getFirstNumber(HBQ),
    %   - groessten wert in dlq holen
    DLQNumber = dlq:getLastMsgNr(DLQ),
    %   - fehlernachricht wird erzeugt und in hbq getan
    HBQ_with_new_message = lists:append(HBQ, createErrorMessage(HBQNumber, DLQNumber));

    true -> HBQ_with_new_message = HBQ
  end,

  % Rückgabe
  {HBQ_with_new_message, DLQ}.

push_messages_to_dlq(HBQ, DLQ) -> 
  % bis zum naechsten loch:
  %   nehme eine nachricht aus der hbq
  %   speichere sie in die dlq
  %   loesche sie aus hbq
  {HBQ, DLQ}.

getFirstNumber([{_,HBQNumber}|Rest]) -> HBQNumber.

createErrorMessage(Holestart, Holeend) -> {"Holestart bis Holeend", Holeend}. 