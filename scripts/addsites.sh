#!/bin/bash

# folder to sites, this is assumed to be installed at the same level
# as the pecan folder. The python code is to get the absolute path
# since the MAC does not have the GNU readlink -f option.
SITES=$( python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "$( dirname $0 )/../../sites" )

# fully qualified hostname
FQDN=$( hostname -f )

# command to execute to add items to BETY database
PSQL="psql -U bety bety -q -t -c"

# function to add a input, takes 4 parameters
# 1 : site id
# 2 : format id
# 3 : start date of input data
# 4 : end date of input data
addInput() {
    INPUT_ID=$( ${PSQL} "SELECT id FROM inputs WHERE site_id=$1 AND format_id=$2 AND start_date='$3' AND end_date='$4' LIMIT 1;" )
    if [ "$INPUT_ID" == "" ]; then
        ${PSQL} "INSERT INTO inputs (site_id, format_id, start_date, end_date, created_at, updated_at) VALUES ($1, $2, '$3', '$4', NOW(), NOW());"
        INPUT_ID=$( ${PSQL} "SELECT id FROM inputs WHERE site_id=$1 AND format_id=$2 AND start_date='$3' AND end_date='$4' LIMIT 1;" )
        echo "Added new input with ID=${INPUT_ID} for site=$1, format_id=$2, start=$3, end=$4"
    fi
}

# function to add a model, takes 4 parameters
# 1 : fully qualified hostname
# 2 : id of input
# 3 : name of file, without the path
# 4 : path to the file
addFile() {
    HOSTID="(SELECT id FROM machines WHERE hostname='${1}')"

    # check if input exists
    COUNT=$( ${PSQL} "SELECT COUNT(id) FROM inputs WHERE id=${2};" )
    if [ "$COUNT" -eq 0 ]; then
        echo "Input ${2} does not exist."
        return 0
    fi

    # check if file already added
    COUNT=$( ${PSQL} "SELECT COUNT(id) FROM dbfiles WHERE container_type='Input' AND container_id=${2} AND file_name='${3}' AND file_path='${4}' and machine_id=${HOSTID};" )
    if [ "$COUNT" -gt 0 ]; then
        echo "File ${4}/${3} already added to input ${2} on host ${1}."
        return 0
    fi

    # Make sure host exists
    ${PSQL} "INSERT INTO machines (hostname, created_at, updated_at)
        SELECT *, now(), now() FROM (SELECT '${1}') AS tmp WHERE NOT EXISTS ${HOSTID};"

    # Add file
    ${PSQL} "INSERT INTO dbfiles (container_type, container_id, file_name, file_path, machine_id) VALUES
            ('Input', ${2}, '${3}', '${4}', ${HOSTID});"

    echo "File ${4}/${3} added to input ${2} on host ${1}."
}

# [76] EBIFARM
addFile "${FQDN}" "7" "ED_MET_DRIVER_HEADER" "${SITES}/ebifarm"
addFile "${FQDN}" "6" "ebifarm.lat40.0lon-88.0.css" "${SITES}/ebifarm"
addFile "${FQDN}" "8" "ebifarm.lat40.0lon-88.0.pss" "${SITES}/ebifarm"
addFile "${FQDN}" "5" "ebifarm.lat40.0lon-88.0.site" "${SITES}/ebifarm"

# [758] HARVARD
addFile "${FQDN}" "82" "ED_MET_DRIVER_HEADER" "${SITES}/harvard_ems"
addFile "${FQDN}" "83" "US-Ha1forcing.nc" "${SITES}/harvard_ems"
addFile "${FQDN}" "138" "harvard.NACP.lat42.5lon-72.5.css" "${SITES}/harvard_ems"
addFile "${FQDN}" "137" "harvard.NACP.lat42.5lon-72.5.pss" "${SITES}/harvard_ems"
addFile "${FQDN}" "136" "harvard.NACP.lat42.5lon-72.5.site" "${SITES}/harvard_ems"

# [676] Willow Creek
addInput "676" "24" "2002-01-01" "2006-12-31"
addFile "${FQDN}" "${INPUT_ID}" "wcr.clim" "${SITES}/willow"
addFile "${FQDN}" "134" "ED_MET_DRIVER_HEADER" "${SITES}/willow"
addFile "${FQDN}" "135" "US-WCrforcing.nc" "${SITES}/willow"
addFile "${FQDN}" "169" "WCr.NACP.lat45.5lon-90.5.css" "${SITES}/willow"
addFile "${FQDN}" "170" "WCr.NACP.lat45.5lon-90.5.pss" "${SITES}/willow"
addFile "${FQDN}" "171" "WCr.NACP.lat45.5lon-90.5.site" "${SITES}/willow"

# [676] Willow Creek updated by Shawn
addFile "${FQDN}" "169" "US-WCr.Inv.lat45.5lon-90.css" "${SITES}/willow"
addFile "${FQDN}" "170" "US-WCr.Inv.lat45.5lon-90.pss" "${SITES}/willow"
addFile "${FQDN}" "171" "US-WCr.Inv.lat45.5lon-90.site" "${SITES}/willow"

# [622] Sylvania
addInput "622" "24" "2002-01-01" "2006-12-31"
addFile "${FQDN}" "${INPUT_ID}" "syl.clim" "${SITES}/sylvana"
addFile "${FQDN}" "132" "ED_MET_DRIVER_HEADER" "${SITES}/sylvana"
addFile "${FQDN}" "133" "US-Syvforcing.nc" "${SITES}/sylvana"
addFile "${FQDN}" "175" "Syl.NACP.lat46.5lon-89.5.css" "${SITES}/sylvana"
addFile "${FQDN}" "176" "Syl.NACP.lat46.5lon-89.5.pss" "${SITES}/sylvana"
addFile "${FQDN}" "177" "Syl.NACP.lat46.5lon-89.5.site" "${SITES}/sylvana"

# [678] Park Falls (WLEF US-PFa)
addInput "678" "24" "1997-01-01" "2005-12-31"
addFile "${FQDN}" "${INPUT_ID}" "lef.clim" "${SITES}/parkfalls"
addFile "${FQDN}" "130" "ED_MET_DRIVER_HEADER" "${SITES}/parkfalls"
addFile "${FQDN}" "131" "US-PFaforcing.nc" "${SITES}/parkfalls"

# # [679] Lost Creek
addInput "679" "24" "2001-01-01" "2006-12-31"
addFile "${FQDN}" "${INPUT_ID}" "lcr.clim" "${SITES}/lostcreek"
addFile "${FQDN}" "92" "ED_MET_DRIVER_HEADER" "${SITES}/lostcreek"
addFile "${FQDN}" "93" "US-Losforcing.nc" "${SITES}/lostcreek"

# # [772] NIWOT
addInput "772" "24" "2002-01-01" "2005-12-31"
addFile "${FQDN}" "${INPUT_ID}" "niwot.clim" "${SITES}/niwot"
addInput "772" "24" "1999-01-01" "2003-12-31"
addFile "${FQDN}" "${INPUT_ID}" "US-NR1.clim" "${SITES}/niwot"
addFile "${FQDN}" "112" "ED_MET_DRIVER_HEADER" "${SITES}/niwot"
addFile "${FQDN}" "113" "US-NR1forcing.nc" "${SITES}/niwot"
addFile "${FQDN}" "186" "NR1.NACP.lat40.5lon-105.5.css" "${SITES}/niwot"
addFile "${FQDN}" "187" "NR1.NACP.lat40.5lon-105.5.pss" "${SITES}/niwot"
addFile "${FQDN}" "188" "NR1.NACP.lat40.5lon-105.5.site" "${SITES}/niwot"
