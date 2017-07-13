#!/bin/bash

# some variables (set in cron.sh)
FQDN=${FQDN:-"$(hostname -f)"}
MYSITE=${MYSITE:-"99"}
MY_START_ID=${MY_START_ID:-"99000000001"}
MY_LAST_ID=${MY_LAST_ID:-"99999999999"}
DATABASE=${DATABASE:-"bety"}
PG_OPT=${PG_OPT:-""}

cat << EOF
<?xml version="1.0" encoding="UTF-8"?>
<catalog name="THREDDS Server Default Catalog : You must change this to fit your server!"
         xmlns="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0"
         xmlns:xlink="http://www.w3.org/1999/xlink"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.unidata.ucar.edu/namespaces/thredds/InvCatalog/v1.0
           http://www.unidata.ucar.edu/schemas/thredds/InvCatalog.1.0.6.xsd">

  <service name="all" base="" serviceType="compound">
    <service name="odap" serviceType="OpenDAP" base="/thredds/dodsC/" />
    <service name="dap4" serviceType="DAP4" base="/thredds/dap4/" />
    <service name="http" serviceType="HTTPServer" base="/thredds/fileServer/" />
    <!--service name="wcs" serviceType="WCS" base="/thredds/wcs/" /-->
    <!--service name="wms" serviceType="WMS" base="/thredds/wms/" /-->
    <service name="ncss" serviceType="NetcdfSubset" base="/thredds/ncss/" />
  </service>

  <datasetRoot path="input" location="/" />

  <dataset name="PEcAn" ID="PEcAn">

EOF

# INPUTS
echo '    <dataset name="Inputs" ID="inputs">'
IFS=$'\n'
for X in $(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT dbfiles.id, file_path || '/' || file_name FROM dbfiles, inputs, formats WHERE dbfiles.id BETWEEN ${MY_START_ID} AND ${MY_LAST_ID} AND dbfiles.container_type='Input' AND dbfiles.container_id=inputs.id AND inputs.format_id=formats.id AND formats.mimetype_id=1093;"); do
    # remove leading whitespace, and extract information
    X="${X//[[:space:]]/}"
    ID="$(echo $X | cut -d "|" -f 1)"
    DBFILE="$(echo $X | cut -d "|" -f 2)"

    # check if folder
    if [ -d "${DBFILE}" ]; then
        # datasetscan
        echo "      <datasetScan name=\"input - ${ID}\" ID=\"input_${ID}\" location=\"${DBFILE}\" path=\"${ID}\">"
        echo '        <metadata inherited="true">'
        echo '          <serviceName>all</serviceName>'
        echo '        </metadata>'
        echo '        <filter>'
        echo '          <include wildcard="*.nc"/>'
        echo '        </filter>'
        echo '      </datasetScan>'
    else
        # add single file
        echo "      <dataset name=\"input - ${ID}\" ID=\"${ID}\" urlPath=\"input${DBFILE}\" serviceName=\"all\">"
        echo '      </dataset>'
    fi
done
echo '    </dataset>'

# WORKFLOWS
echo '    <dataset name="Workflows" ID="workflows">'
for X in $(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT id, folder FROM workflows WHERE workflows.id BETWEEN ${MY_START_ID} AND ${MY_LAST_ID};"); do
    # remove leading whitespace, and extract information
    X="${X//[[:space:]]/}"
    WF_ID="$(echo $X | cut -d "|" -f 1)"
    WF_FOLDER="$(echo $X | cut -d "|" -f 2)"
    WF_FOLDER=${WF_FOLDER//\/\//\/}

    # create workflow
    echo "      <dataset name=\"workflow - ${WF_ID}\" ID=\"workflow_${WF_ID}\">"
    for Y in $(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT id, runtype FROM ensembles WHERE workflow_id=${WF_ID};"); do
        Y="${Y//[[:space:]]/}"
        ENS_ID="$(echo $Y | cut -d "|" -f 1)"
        ENS_TYPE="$(echo $Y | cut -d "|" -f 2)"
        echo "        <dataset name=\"${ENS_TYPE} - ${ENS_ID}\" ID=\"ensembles_${ENS_ID}\">"
        for RUN_ID in $(sudo -u postgres psql ${PG_OPT} -d ${DATABASE} -t -c "SELECT id FROM runs WHERE ensemble_id=${ENS_ID};"); do
            RUN_ID="${RUN_ID//[[:space:]]/}"
            echo "          <datasetScan name=\"run - ${RUN_ID}\" ID=\"runs_${RUN_ID}\" location=\"${WF_FOLDER}/out/${RUN_ID}\" path=\"${RUN_ID}\">"
            echo '            <metadata inherited="true">'
            echo '              <serviceName>all</serviceName>'
            echo '            </metadata>'
            echo '            <filter>'
            echo '              <include wildcard="*.nc"/>'
            echo '            </filter>'
            echo '          </datasetScan>'
        done
    echo '        </dataset>'
    done
    echo '      </dataset>'
done
echo '    </dataset>'

# FOOTER
echo '  </dataset>'
echo '</catalog>'
