#!/bin/bash
DATE=`cat DATE`
env yr0=$1 yrf=2 date=DATE R --vanilla < ./rscripts/pecan.SA.runscript.R