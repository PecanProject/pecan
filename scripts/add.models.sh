#!/bin/bash

# load helper functions and set FQDN and PSQL
. $( dirname $0 )/add.util.sh

# Add models (all these are assumed to be local or $FQDN)
# 1 : name of the model, shown in web interface
# 2 : type of model (ED2, SIPNET, BIOCRO, DALEC, ...)
# 3 : model revision number
# 4 : name of executable, without the path
# 5 : optionally path to executable
addLocalModel "ED2.2"     "ED2"      "46"  "ed2.r46"
addLocalModel "ED2.2"     "ED2"      "82"  "ed2.r82"
addLocalModel "ED2.2"     "ED2"      "git" "ed2.git"
addLocalModel "SIPNET"    "SIPNET"   "unk" "sipnet.runk"
addLocalModel "SIPNET"    "SIPNET"   "136" "sipnet.r136"
addLocalModel "DALEC"     "DALEC"    ""    "dalec_seqMH"
addLocalModel "Linkages"  "LINKAGES" "git" "linkages.git"
addLocalModel "MAESPA"    "MAESPA"   "git" "maespa.git"
addLocalModel "LPJ-GUESS" "LPJGUESS" "3.1" "guess.3.1"
addLocalModel "GDAY(Day)" "GDAY"     ""    "gday"

# special case for PRELES
addModelFile "${FQDN}" "Preles"   "PRELES"   ""          "true"           "/bin"

# special case for R models
addModelFile "${FQDN}" "BioCro"   "BIOCRO"   "git"       "biocro.Rscript" "${R_LIBS_USER}/PEcAn.BIOCRO"
addModelFile "${FQDN}" "LINKAGES" "LINKAGES" "R_version" ""               "${R_LIBS_USER}/linkages"

# to add remote files
#addModelFile "geo.bu.edu" "ED2" "ED2" "git" "ed_2.1-opt" "/home/dietze/ED2.git/ED/build"
