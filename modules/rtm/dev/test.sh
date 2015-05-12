#!/bin/bash
#cd ../src
#make purge
#cd ../dev
R CMD INSTALL ..
Rscript fortinv.test.R
evince Rplots.pdf &
