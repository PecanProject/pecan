#!/bin/bash
#cd ../src
#make purge
#cd ../dev
R CMD INSTALL ..
R -d gdb -e "source('vignette.R')"
