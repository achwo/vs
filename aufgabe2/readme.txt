0. Configs anpassen (ip)
1. nameservice
2. koordinator
3. starter


--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
client_func.erl; client_starter.erl; compileAll.erl;
server_func.erl; server_starter.erl; werkzeug.erl;

sowie:
Readme.txt; client.cfg; server.cfg

(w)erl -(s)name wk -setcookie zummsel
1> c(compileAll).
2> compileAll:start().

--------------------
Starten des Servers:
--------------------
(w)erl -(s)name wk -setcookie zummsel
1> server_starter:start( ).

% in der server.cfg:
% {lifetime, 60}. Zeit in Sekunden, die der Server bei Leerlauf wartet, bevor er sich beendet
% {clientlifetime,5}. Zeitspanne, in der sich an den Client erinnert wird
% {servername, wk}. Name des Servers als Atom
% {dlqlimit, 13}. Größe der DLQ

Starten des Clients:
--------------------
(w)erl -(s)name client -setcookie zummsel
1> client_starter:start('server@lab33.cpt.haw-hamburg.de').

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