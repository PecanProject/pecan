#!/bin/bash
## Perform an inversion. Parameters are as follows:
## [1] Spectra
## [2] Number of iterations
## [3] Folder name

SPECARG=$1
NGIBBSARG=${2:-100}
FOLDARG=${3:-testfolder}

qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=01 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=02 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=03 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=04 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=05 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=06 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=07 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=08 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=09 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=10 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=11 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=12 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=13 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=14 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=15 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub
# qsub -v SPEC=$SPECARG,NGIBBS=$NGIBBSARG,FOLDER=$FOLDARG,RUN=16 -N "pbi $SPECARG $FOLDARG $RUN" submit_prosp_nimble.qsub

