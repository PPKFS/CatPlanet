#! /bin/bash
cd ../blt-pl
make blt2
cd ../CatPlanet
cp -r ../blt-pl/lib lib/
cp ../blt-pl/bearlibterminal.pl lib/
swipl main.pl
