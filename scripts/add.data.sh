#!/bin/bash

DIRNAME=$( dirname $0 )

#FQDN="geo.bu.edu"
#DATADIR="/usr2/collab/kooper"
#FQDN="cookiemonster.ncsa.illinois.edu"
#DATADIR="/home/kooper/Work/EBI"

# load helper functions and set FQDN and PSQL
. ${DIRNAME}/add.util.sh

# subfolders in datadir
SITES="$DATADIR/sites"
DALEC="$DATADIR/dalec_EnKF_pub/input_data"
ED_INPUT="$DATADIR/ed_inputs"
FAO_INPUT="$DATADIR/faoOLD"
OGE2_INPUT="$DATADIR/oge2OLD"
LPJ_GUESS="$DATADIR/lpj-guess"

# ED inputs
addInput "1118" "41" "1800-01-01 05:50:36" "1999-01-01 06:00:00" # 294
addInputFile "${FQDN}" "${INPUT_ID}" "" "${ED_INPUT}/glu"
addInput "1118" "42" "" "" # 295
addInputFile "${FQDN}" "${INPUT_ID}" "" "${ED_INPUT}"
addInput "1118" "43" "" "" # 296
addInputFile "${FQDN}" "${INPUT_ID}" "OGE2_" "${OGE2_INPUT}"
addInput "1118" "44" "" "" # 297
addInputFile "${FQDN}" "${INPUT_ID}" "FAO_" "${FAO_INPUT}"

# LPJ-GUESS inputs
addInput "1118" "1000000020" "" "" # 1000000903
addInputFile "${FQDN}" "${INPUT_ID}" "cru_1901_2006.bin" "${LPJ_GUESS}"

# [76] EBIFARM
addInput "76" "10" "" "" # 5
addInputFile "${FQDN}" "${INPUT_ID}" "ebifarm.lat40.0lon-88.0.site" "${SITES}/ebifarm"
addInput "76" "11" "" "" # 6
addInputFile "${FQDN}" "${INPUT_ID}" "ebifarm.lat40.0lon-88.0.css" "${SITES}/ebifarm"
addInput "76" "12" "2004-01-01 00:00:00" "2009-12-31 23:59:59" # 7
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/ebifarm"
addInput "76" "15" "" "" # 8
addInputFile "${FQDN}" "${INPUT_ID}" "ebifarm.lat40.0lon-88.0.pss" "${SITES}/ebifarm"

# [758] HARVARD
addInput "758" "12" "1991-01-01 06:00:00" "2006-12-31 06:00:00" # 82
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/harvard_ems"
addInput "758" "16" "1991-01-01 06:00:00" "2006-12-31 06:00:00" # 83
addInputFile "${FQDN}" "${INPUT_ID}" "US-Ha1forcing.nc" "${SITES}/harvard_ems"
addInput "758" "10" "" "" # 136 ~
addInputFile "${FQDN}" "${INPUT_ID}" "harvard.NACP.lat42.5lon-72.5.site" "${SITES}/harvard_ems"
addInput "758" "11" "" "" # 138
addInputFile "${FQDN}" "${INPUT_ID}" "harvard.NACP.lat42.5lon-72.5.css" "${SITES}/harvard_ems"
addInput "758" "15" "" "" # 137
addInputFile "${FQDN}" "${INPUT_ID}" "harvard.NACP.lat42.5lon-72.5.pss" "${SITES}/harvard_ems"

# [676] Willow Creek
addInput "676" "24" "1998-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "wcr.clim" "${SITES}/willow"
addInput "676" "12" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 134
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/willow"
addInput "676" "16" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 135
addInputFile "${FQDN}" "${INPUT_ID}" "US-WCrforcing.nc" "${SITES}/willow"
addInput "676" "10" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 171
addInputFile "${FQDN}" "${INPUT_ID}" "WCr.NACP.lat45.5lon-90.5.site" "${SITES}/willow"
addInput "676" "11" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 169
addInputFile "${FQDN}" "${INPUT_ID}" "WCr.NACP.lat45.5lon-90.5.css" "${SITES}/willow"
addInput "676" "15" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 170
addInputFile "${FQDN}" "${INPUT_ID}" "WCr.NACP.lat45.5lon-90.5.pss" "${SITES}/willow"

# [676] Willow Creek updated by Shawn
addInput "676" "10" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 171
addInputFile "${FQDN}" "${INPUT_ID}" "US-WCr.Inv.lat45.5lon-90.site" "${SITES}/willow"
addInput "676" "11" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 169
addInputFile "${FQDN}" "${INPUT_ID}" "US-WCr.Inv.lat45.5lon-90.css" "${SITES}/willow"
addInput "676" "15" "1998-01-01 06:00:00" "2006-12-31 06:00:00" # 170
addInputFile "${FQDN}" "${INPUT_ID}" "US-WCr.Inv.lat45.5lon-90.pss" "${SITES}/willow"

# [622] Sylvania
addInput "622" "24" "2001-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "syl.clim" "${SITES}/sylvana"
addInput "622" "12" "2001-01-01 06:00:00" "2006-12-31 06:00:00" # 132
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/sylvana"
addInput "622" "16" "2001-01-01 06:00:00" "2006-12-31 06:00:00" # 133
addInputFile "${FQDN}" "${INPUT_ID}" "US-Syvforcing.nc" "${SITES}/sylvana"
addInput "622" "10" "2001-01-01 06:00:00" "2006-12-31 06:00:00" # 177
addInputFile "${FQDN}" "${INPUT_ID}" "Syl.NACP.lat46.5lon-89.5.site" "${SITES}/sylvana"
addInput "622" "11" "2001-01-01 06:00:00" "2006-12-31 06:00:00" # 175
addInputFile "${FQDN}" "${INPUT_ID}" "Syl.NACP.lat46.5lon-89.5.css" "${SITES}/sylvana"
addInput "622" "15" "2001-01-01 06:00:00" "2006-12-31 06:00:00" # 176
addInputFile "${FQDN}" "${INPUT_ID}" "Syl.NACP.lat46.5lon-89.5.pss" "${SITES}/sylvana"

# [678] Park Falls (WLEF US-PFa)
addInput "678" "24" "1997-01-01" "2005-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "lef.clim" "${SITES}/parkfalls"
addInput "678" "12" "1995-01-01 06:00:00" "2005-12-31 06:00:00" # 130
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/parkfalls"
addInput "678" "16" "1995-01-01 06:00:00" "2005-12-31 06:00:00" # 131
addInputFile "${FQDN}" "${INPUT_ID}" "US-PFaforcing.nc" "${SITES}/parkfalls"

# [679] Lost Creek
addInput "679" "24" "2001-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "lcr.clim" "${SITES}/lostcreek"
addInput "679" "12" "2000-01-01 06:00:00" "2006-12-31 06:00:00" # 92
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/lostcreek"
addInput "679" "16" "2000-01-01 06:00:00" "2006-12-31 06:00:00" # 93
addInputFile "${FQDN}" "${INPUT_ID}" "US-Losforcing.nc" "${SITES}/lostcreek"

# [772] NIWOT
addInput "772" "24" "2002-01-01" "2005-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "niwot.clim" "${SITES}/niwot"
addInput "772" "24" "1999-01-01" "2003-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "US-NR1.clim" "${SITES}/niwot"
addInput "772" "12" "1998-01-01 06:00:00" "2008-01-01 05:00:00" # 112
addInputFile "${FQDN}" "${INPUT_ID}" "ED_MET_DRIVER_HEADER" "${SITES}/niwot"
addInput "772" "16" "1998-01-01 06:00:00" "2007-12-31 06:00:00" # 113
addInputFile "${FQDN}" "${INPUT_ID}" "US-NR1forcing.nc" "${SITES}/niwot"
addInput "772" "10" "1998-01-01 06:00:00" "2007-12-31 06:00:00" # 188
addInputFile "${FQDN}" "${INPUT_ID}" "NR1.NACP.lat40.5lon-105.5.site" "${SITES}/niwot"
addInput "772" "11" "1998-01-01 06:00:00" "2007-12-31 06:00:00" # 186
addInputFile "${FQDN}" "${INPUT_ID}" "NR1.NACP.lat40.5lon-105.5.css" "${SITES}/niwot"
addInput "772" "15" "1998-01-01 06:00:00" "2007-12-31 06:00:00" # 187
addInputFile "${FQDN}" "${INPUT_ID}" "NR1.NACP.lat40.5lon-105.5.pss" "${SITES}/niwot"

# [766] Metolius
addFormat "text/plain" "DALEC meteorology"
addInput "766" "${FORMAT_ID}" "1999-01-01" "2003-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "dalec_drivers.OREGON.no_obs.dat" "${DALEC}/oregon"
