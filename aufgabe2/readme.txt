--------------------
Ausführung des Funktionstests:
--------------------

0. Configs anpassen (nameservicenode, koordinatorname)
1. Nameserver starten: ./start_nameserver.sh $own_ip
2. Koordinator und Starter ausführen: ./start_stuff.sh $own_ip
3. Testdatei ausführen: ./gui.sh $own_ip

Wobei $own_ip die IP-Adresse des jeweiligen ausführenden PCs ist.

--------------------
Startreihenfolge der Erlangdateien:
--------------------
0. Configs anpassen (nameservicenode, koordinatorname)
1. Nameserver starten
2. Koordinator starten: koordinator:start()
3. ./gui.sh $ip

--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
koordinator.erl ggt.erl nameservice.erl werkzeug.erl
utility.erl starter.erl gui.erl

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