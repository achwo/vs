Entwurf:         10h
Implementation:  95h
Gesamt:         105h

--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
client.erl, 
server.erl, server_timer.erl, dispatcher.erl, 
clientlist.erl, dlq.erl, hbq.erl
werkzeug.erl

sowie:
Readme.txt; client.cfg; server.cfg
start_server.sh, start_client.sh, cleanup.sh

erlc *.erl

--------------------
Starten des Servers:
--------------------

./start_server.sh $eigene_ip

% in der server.cfg:
% {lifetime, 60}. Zeit in Sekunden, die der Server bei Leerlauf wartet, bevor er sich beendet
% {clientlifetime,5}. Zeitspanne, in der sich an den Client erinnert wird
% {servername, wk}. Name des Servers als Atom
% {dlqlimit, 13}. Größe der DLQ

Starten des Clients:
--------------------

./start_client.sh $name $eigene_ip $server_ip

% 'wk@lab33.cpt.haw-hamburg.de': Name der Server Node (z.B.: server@lab21), erhält man zB über node()
% ' wegen dem - bei haw-hamburg, da dies sonst als minus interpretiert wird.
% in der client.cfg:
% {clients, 2}.  Anzahl der Clients, die gestartet werden sollen
% {lifetime, 42}. Laufzeit der Clients
% {servername, wk}. Name des Servers
% {sendeintervall, 3}. Zeitabstand der einzelnen Nachrichten

Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Informationen zu Prozessen:
-------------
2> pman:start().
2> process_info(PID).