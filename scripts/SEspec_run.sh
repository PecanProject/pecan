#!/bin/bash
REFARG=$1
FOLDARG=$2
qsub -v SPECIES="ICsp",REF=$REFARG,FOLDER=$FOLDARG -N "pbi IC $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="LCsp",REF=$REFARG,FOLDER=$FOLDARG -N "pbi LC $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="PICO",REF=$REFARG,FOLDER=$FOLDARG -N "pbi PICO $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="PILA",REF=$REFARG,FOLDER=$FOLDARG -N "pbi PILA $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="QUCH",REF=$REFARG,FOLDER=$FOLDARG -N "pbi QUCH $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="QUKE",REF=$REFARG,FOLDER=$FOLDARG -N "pbi QUKE $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="Peach",REF=$REFARG,FOLDER=$FOLDARG -N "pbi Peach $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="Alfalfa",REF=$REFARG,FOLDER=$FOLDARG -N "pbi Alfalfa $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="Oats",REF=$REFARG,FOLDER=$FOLDARG -N "pbi Oats $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="Pomegranate",REF=$REFARG,FOLDER=$FOLDARG -N "pbi Pomegranate $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="RedPepper",REF=$REFARG,FOLDER=$FOLDARG -N "pbi RedPepper $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="Mandarin",REF=$REFARG,FOLDER=$FOLDARG -N "pbi Mandarin $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="ShortDatePalm",REF=$REFARG,FOLDER=$FOLDARG -N "pbi ShortDatePalm $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="MANZ",REF=$REFARG,FOLDER=$FOLDARG -N "pbi MANZ $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="CADE",REF=$REFARG,FOLDER=$FOLDARG -N "pbi CADE $REFARG $FOLDARG" multirun.qsub
qsub -v SPECIES="PIPO",REF=$REFARG,FOLDER=$FOLDARG -N "pbi PIPO $REFARG $FOLDARG" multirun.qsub

