%{getsteeringval,self()}: Die Anfrage nach den steuernden Werten durch den Starter Prozess.
%{hello,Clientname}: Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
%{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr. 
%{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname und PID From informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
%reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
%step: Der Koordinator beendet die Initialphase und bildet den Ring. Er wartet nun auf den Start einer ggT-Berechnung.
%prompt: Der Koordinator erfragt bei allen ggT-Prozessen per tellmi deren aktuelles Mi ab und zeigt dies im log an.
%nudge: Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT deren Lebenszustand ab und zeigt dies im log an.
%toggle: Der Koordinator verändert den Flag zur Korrektur bei falschen Terminierungsmeldungen.
%{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit Wunsch-ggT WggT.
%kill: Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.