#!/bin/bash
./composer.sh build
./startStations.sh wlan0 225.10.1.2 16001 1 1 A 0
killall beam.smp

