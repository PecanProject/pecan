#!/bin/bash

# load helper functions and set FQDN and PSQL
. $( dirname $0 )/add.util.sh

# Add models (all these are assumed to be local or $FQDN)
addLocalModel "ED2.2" "ED2" "46" "ed2.r46"
addLocalModel "ED2.2" "ED2" "82" "ed2.r82"
addLocalModel "SIPNET" "SIPNET" "unk" "sipnet.runk"
addLocalModel "DALEC" "DALEC" "" "dalec_seqMH"
addLocalModel "Linkages" "LINKAGES" "git" "linkages.git"

# special case for BioCro
addModelFile "${FQDN}" "BioCro" "BIOCRO" "" "biocro.Rscript" "${R_LIBS_USER}/PEcAn.biocro"

# to add remote files
#addModelFile "geo.bu.edu" "ED2" "ED2" "git" "ed_2.1-opt" "/home/dietze/ED2.git/ED/build"
