#!/bin/bash

./composer.sh build
mkdir -p /tmp/spawner
rm /tmp/spawner/*
rm dbg.log
# Spawn station A with zero offset

./startStations.sh eth0 224.0.0.8 15001 1 1 A 10 #> /tmp/spawner/a.log
# Spawn multiple A with offset 42
#./startStations.sh wlan0 225.10.1.2 16000 2 4 A 42 > /dev/null
# Spawn multiple B stations
#./startStations.sh wlan0 225.10.1.2 16000 4 8 B 100 > /dev/null
./startStations.sh eth0 224.0.0.8 15001 2 10 B 0 #> /tmp/spawner/b.log

# Start sniffing
ressourcen/sniffer/64bit/STDMAsniffer 224.0.0.8 15001 eth0 -adapt
killall beam.smp
killall beam