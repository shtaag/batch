#!/bin/sh

# -fhpc : run with coverage recording
# -package-conf : indicates the bin dir
# -i : indicates the modules to use
ghc HpcMain.hs -fhpc -package-conf=../../cabal-dev/packages-7.4.1.conf -i../ -i../../

chmod +x HpcMain
./HpcMain

hpc report HpcMain.tix --per-module --xml-output > hpc_report.txt

hpc markup HpcMain.tix
