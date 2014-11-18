0. Configs anpassen (nameservicenode, koordinatorname)
1. Nameserver starten
2. Koordinator starten: koordinator:start()
3. ./gui.sh $ip

--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
koordinator.erl ggt.erl nameservice.erl werkzeug.erl
utility.erl starter.erl
server_func.erl; server_starter.erl; werkzeug.erl;

sowie:
Readme.txt; ggt.cfg; koordinator.cfg

--------------------
Starten des Nameservice:
--------------------
erl -name ns -setcookie karl
1> nameservice:start()

--------------------
Starten des Koordinators:
--------------------
erl -name ko -setcookie karl
1> koordinator:start().

--------------------
Starten der GGT:
--------------------
erl -name ggt -setcookie karl
1> starter:start(23).

Mit Parameter Unique ID.
