#!/bin/bash

./composer.sh build
mkdir -p /tmp/spawner
# Spawn station A with zero offset

./startStations.sh wlan0 225.10.1.2 16001 1 1 A 10 > /tmp/spawner/a.log
# Spawn multiple A with offset 42
#./startStations.sh wlan0 225.10.1.2 16000 2 4 A 42 > /dev/null
# Spawn multiple B stations
#./startStations.sh wlan0 225.10.1.2 16000 4 8 B 100 > /dev/null
./startStations.sh wlan0 225.10.1.2 16001 2 10 B 0 > /tmp/spawner/b.log

# Start sniffing
misc/64bit/./STDMAsniffer 225.10.1.2 16001 wlan0 -adapt
killall beam.smp

