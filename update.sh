#! /bin/bash
cd ../libtcod-pl
make
cd ../CatPlanet
cp -r ../libtcod-pl/lib lib/
cp ../libtcod-pl/tcod.pl lib/
swipl main.pl
