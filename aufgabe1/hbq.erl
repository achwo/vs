-module(hbq).
-import(dlq, [get_max_number/1]).
-export([start/0]).

% TODO loop in nachrichtendienst.erl uebernehmen
% TODO listenformat [{Nachricht, Nr}]
% TODO createNew() :: void -> HBQ
% TODO add(Msg, Nr, HBQ, DLQ) :: .. -> HBQ x DLQ

start() ->
  loop([], []).

loop(HBQ, DLQ) ->
  receive
    {dropmessage, {Message, Number}} -> 
      {New_HBQ, New_DLQ} = dropmessage(Message, Number, HBQ, DLQ)
  end,
  % todo: manipulate lists
  loop(New_HBQ, New_DLQ).

dropmessage(Message, Number, HBQ, DLQ) -> 

  % Nachricht und Nummer werden in die Holdbackqueue geschrieben.
  HBQ_with_new_message = lists:append(HBQ, {Number, Message}), 
  %% Danach wird geprüft, ob Lücken geschlossen werden müssen.  (doch nicht :))
  %{New_HBQ, New_DLQ} = close_holes_if_necessary(HBQ_with_new_message, DLQ),
  % Nach der Überprüfung, werden die Nachrichten bis zur nächsten Lücke in die DeliveryQueue geschoben
  push_messages_to_dlq(New_HBQ, New_DLQ).


close_holes_if_necessary(HBQ, DLQ) -> 
  %   - luecken muessen geschlossen werden, wenn mehr als size(dlq) / 2; dann:
  %     - kleinsten wert in hbq holen
  %     - groessten wert in dlq holen
  %     - fehlernachricht wird erzeugt und in hbq getanh
  {HBQ, DLQ}.

push_messages_to_dlq(HBQ, DLQ) -> 
  % bis zum naechsten loch:
  %   nehme eine nachricht aus der hbq
  %   speichere sie in die dlq
  %   loesche sie aus hbq
  {HBQ, DLQ}.

% hole closer is independet process