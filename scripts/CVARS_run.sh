#!/bin/bash
REFARG=$1
SIARG=$2
qsub -v SPECIES="Grape",REF=REFARG,SI=SIARG multirun.qsub
qsub -v SPECIES="CAAN-Red Pepper",REF=REFARG,SI=SIARG multirun.qsub
qsub -v SPECIES="LemonTree",REF=REFARG,SI=SIARG multirun.qsub
qsub -v SPECIES="ShortDatePalm",REF=REFARG,SI=SIARG multirun.qsub
qsub -v SPECIES="MandarinOrange",REF=REFARG,SI=SIARG multirun.qsub

