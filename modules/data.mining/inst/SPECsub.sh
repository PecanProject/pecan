#!/bin/bash
#$ -j y -m eas -M mdietze@illinois.edu -V -cwd -S /bin/bash
echo "start"
env SITENUM=$1 NSTART=$2 /home/mdietze/bin/Rexe/bin/R --vanilla < /home/mdietze/stats/spectral/NullSpectra.v2.R run2.$1.$2.log
echo "stop"
