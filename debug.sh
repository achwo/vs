#!/bin/bash
./composer.sh build
./startStations.sh eth0 224.0.0.8 15001 1 1 A
killall beam.smp

