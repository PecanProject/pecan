#!/bin/bash

DIRNAME=$( dirname $0 )

# folder to data, this is assumed to be installed at the same level
# as the pecan folder. The python code is to get the absolute path
# since the MAC does not have the GNU readlink -f option.
SITES=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "${DIRNAME}/../../sites" )
DALEC=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "${DIRNAME}/../../dalec_EnKF_pub/input_data" )
ED_INPUT=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "${DIRNAME}/../../ed_inputs" )
FAO_INPUT=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "${DIRNAME}/../../faoOLD" )
OGE2_INPUT=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "${DIRNAME}/../../oge2OLD" )

# load helper functions and set FQDN and PSQL
. ${DIRNAME}/add.util.sh

# ED inputs
addInputFile "${FQDN}" "294" "" "${ED_INPUT}/glu"
addInputFile "${FQDN}" "295" "" "${ED_INPUT}"
addInputFile "${FQDN}" "296" "FAO_" "${FAO_INPUT}"
addInputFile "${FQDN}" "297" "OGE2_" "${OGE2_INPUT}"

# [76] EBIFARM
addInputFile "${FQDN}" "7" "ED_MET_DRIVER_HEADER" "${SITES}/ebifarm"
addInputFile "${FQDN}" "6" "ebifarm.lat40.0lon-88.0.css" "${SITES}/ebifarm"
addInputFile "${FQDN}" "8" "ebifarm.lat40.0lon-88.0.pss" "${SITES}/ebifarm"
addInputFile "${FQDN}" "5" "ebifarm.lat40.0lon-88.0.site" "${SITES}/ebifarm"

# [758] HARVARD
addInputFile "${FQDN}" "82" "ED_MET_DRIVER_HEADER" "${SITES}/harvard_ems"
addInputFile "${FQDN}" "83" "US-Ha1forcing.nc" "${SITES}/harvard_ems"
addInputFile "${FQDN}" "138" "harvard.NACP.lat42.5lon-72.5.css" "${SITES}/harvard_ems"
addInputFile "${FQDN}" "137" "harvard.NACP.lat42.5lon-72.5.pss" "${SITES}/harvard_ems"
addInputFile "${FQDN}" "136" "harvard.NACP.lat42.5lon-72.5.site" "${SITES}/harvard_ems"

# [676] Willow Creek
addInput "676" "24" "2002-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "wcr.clim" "${SITES}/willow"
addInputFile "${FQDN}" "134" "ED_MET_DRIVER_HEADER" "${SITES}/willow"
addInputFile "${FQDN}" "135" "US-WCrforcing.nc" "${SITES}/willow"
addInputFile "${FQDN}" "169" "WCr.NACP.lat45.5lon-90.5.css" "${SITES}/willow"
addInputFile "${FQDN}" "170" "WCr.NACP.lat45.5lon-90.5.pss" "${SITES}/willow"
addInputFile "${FQDN}" "171" "WCr.NACP.lat45.5lon-90.5.site" "${SITES}/willow"

# [676] Willow Creek updated by Shawn
addInputFile "${FQDN}" "169" "US-WCr.Inv.lat45.5lon-90.css" "${SITES}/willow"
addInputFile "${FQDN}" "170" "US-WCr.Inv.lat45.5lon-90.pss" "${SITES}/willow"
addInputFile "${FQDN}" "171" "US-WCr.Inv.lat45.5lon-90.site" "${SITES}/willow"

# [622] Sylvania
addInput "622" "24" "2002-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "syl.clim" "${SITES}/sylvana"
addInputFile "${FQDN}" "132" "ED_MET_DRIVER_HEADER" "${SITES}/sylvana"
addInputFile "${FQDN}" "133" "US-Syvforcing.nc" "${SITES}/sylvana"
addInputFile "${FQDN}" "175" "Syl.NACP.lat46.5lon-89.5.css" "${SITES}/sylvana"
addInputFile "${FQDN}" "176" "Syl.NACP.lat46.5lon-89.5.pss" "${SITES}/sylvana"
addInputFile "${FQDN}" "177" "Syl.NACP.lat46.5lon-89.5.site" "${SITES}/sylvana"

# [678] Park Falls (WLEF US-PFa)
addInput "678" "24" "1997-01-01" "2005-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "lef.clim" "${SITES}/parkfalls"
addInputFile "${FQDN}" "130" "ED_MET_DRIVER_HEADER" "${SITES}/parkfalls"
addInputFile "${FQDN}" "131" "US-PFaforcing.nc" "${SITES}/parkfalls"

# [679] Lost Creek
addInput "679" "24" "2001-01-01" "2006-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "lcr.clim" "${SITES}/lostcreek"
addInputFile "${FQDN}" "92" "ED_MET_DRIVER_HEADER" "${SITES}/lostcreek"
addInputFile "${FQDN}" "93" "US-Losforcing.nc" "${SITES}/lostcreek"

# [772] NIWOT
addInput "772" "24" "2002-01-01" "2005-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "niwot.clim" "${SITES}/niwot"
addInput "772" "24" "1999-01-01" "2003-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "US-NR1.clim" "${SITES}/niwot"
addInputFile "${FQDN}" "112" "ED_MET_DRIVER_HEADER" "${SITES}/niwot"
addInputFile "${FQDN}" "113" "US-NR1forcing.nc" "${SITES}/niwot"
addInputFile "${FQDN}" "186" "NR1.NACP.lat40.5lon-105.5.css" "${SITES}/niwot"
addInputFile "${FQDN}" "187" "NR1.NACP.lat40.5lon-105.5.pss" "${SITES}/niwot"
addInputFile "${FQDN}" "188" "NR1.NACP.lat40.5lon-105.5.site" "${SITES}/niwot"

# [766] Metolius
addFormat "text/plain" "DALEC meteorology"
addInput "766" "${FORMAT_ID}" "1999-01-01" "2003-12-31"
addInputFile "${FQDN}" "${INPUT_ID}" "dalec_drivers.OREGON.no_obs.dat" "${DALEC}/oregon"
