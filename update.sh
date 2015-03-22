#! /bin/bash

cp -r ../libtcod-pl/lib lib/
cp ../libtcod-pl/*.pl lib/
swipl main.pl
