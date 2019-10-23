#!/bin/sh

# This script builds and runs the demo game.  You need
# the VICE emulatore installed, in particular VICE's x64.

# You might want a `vicerc` file like the following:
# [C64]
# VICIIDoubleScan=0
# VICIIDoubleSize=0
# KeySet1NorthWest=0
# KeySet1North=273
# KeySet1NorthEast=0
# KeySet1East=275
# KeySet1SouthEast=0
# KeySet1South=274
# KeySet1SouthWest=0
# KeySet1West=276
# KeySet1Fire=306
# KeySetEnable=1
# JoyDevice1=0
# JoyDevice2=2

../../../bin/sixtypical --run-on x64 -I ../../../include/c64/ demo-game.60p
