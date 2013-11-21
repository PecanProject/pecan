#!/bin/bash
#$ -j y -m eas -M mdietze@illinois.edu -V -cwd -S /bin/bash
echo "start"
/home/mdietze/bin/Rexe/bin/R --vanilla < /home/mdietze/stats/spectral/NACPspectral.Keenan.R > Keenan.log
echo "stop"
