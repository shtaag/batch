#!/bin/sh

#### setup ####
echo `pwd`

rm HpcMain
rm *.tix
rm HPC/*.html
rm HPC/hpc_report.txt

# rm *.o
# rm *.hi
# rm -r .hpc

#### build start ####

# -fhpc : run with coverage recording
# -package-conf : indicates the bin dir
# -i : indicates the modules to use
ghc HpcMain.hs -fhpc -package-conf=cabal-dev/packages-7.4.1.conf -itest/

#### run ####
./HpcMain

#### create report ####
hpc report HpcMain.tix --per-module --xml-output > HPC/hpc_report.txt
hpc markup HpcMain.tix --destdir=HPC
