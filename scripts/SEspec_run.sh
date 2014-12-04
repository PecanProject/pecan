#!/bin/bash
REFARG=$1
FOLDARG=$2
qsub -v SPECIES="IC",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="LC",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="PICO",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="PILA",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="QUCH",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="QUKE",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="Peach",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="Alfalfa",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="Oats",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="Pomegranate",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="RedPepper",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="Mandarin",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="ShortDatePalm",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="MANZ",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="CADE",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub
qsub -v SPECIES="PIPO",REF=$REFARG,FOLDER=$FOLDARG multirun.qsub

