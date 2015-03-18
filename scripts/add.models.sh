#!/bin/bash

# load helper functions and set FQDN and PSQL
. $( dirname $0 )/add.util.sh

# Add models
addModelFile "${FQDN}" "ED2.2" "ED2" "46" "ed2.r46" "/usr/local/bin"
addModelFile "${FQDN}" "ED2.2" "ED2" "82" "ed2.r82" "/usr/local/bin"
addModelFile "${FQDN}" "SIPNET" "SIPNET" "unk" "sipnet.runk" "/usr/local/bin"
addModelFile "${FQDN}" "BioCro" "BIOCRO" "" "biocro.Rscript" "${R_LIBS_USER}/PEcAn.biocro"
addModelFile "${FQDN}" "DALEC" "DALEC" "" "dalec_seqMH" "/usr/local/bin"
addModelFile "${FQDN}" "Linkages" "LINKAGES" "git" "linkages.git" "/usr/local/bin"
