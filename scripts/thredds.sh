#!/bin/bash

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

FQDN=$(hostname -f)
HOST_ID=$(sudo -u postgres psql -d bety -t -c "SELECT id FROM machines WHERE hostname='${FQDN}';")
HOST_START=$(sudo -u postgres psql -d bety -t -c "SELECT sync_start FROM machines WHERE hostname='${FQDN}';")
HOST_START="${HOST_START//[[:space:]]/}"
HOST_END=$(sudo -u postgres psql -d bety -t -c "SELECT sync_end FROM machines WHERE hostname='${FQDN}';")
HOST_END="${HOST_END//[[:space:]]/}"
if [ "${HOST_START}" == "" -o "${HOST_END}" == "" ]; then
    HOST_ID=99
    HOST_START="99000000001"
    HOST_END="99999999999"
fi

# INPUTS
echo '    <dataset name="Inputs" ID="inputs">'
IFS=$'\n'
for X in $(sudo -u postgres psql -d bety -t -c "SELECT dbfiles.id, file_path || '/' || file_name FROM dbfiles, inputs, formats WHERE dbfiles.id BETWEEN ${HOST_START} AND ${HOST_END} AND dbfiles.container_type='Input' AND dbfiles.container_id=inputs.id AND inputs.format_id=formats.id AND formats.mimetype_id=1093;"); do
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
for X in $(sudo -u postgres psql -d bety -t -c "SELECT id, folder FROM workflows WHERE workflows.id BETWEEN ${HOST_START} AND ${HOST_END};"); do
    # remove leading whitespace, and extract information
    X="${X//[[:space:]]/}"
    WF_ID="$(echo $X | cut -d "|" -f 1)"
    WF_FOLDER="$(echo $X | cut -d "|" -f 2)"
    WF_FOLDER=${WF_FOLDER//\/\//\/}

    # create workflow
    echo "      <dataset name=\"workflow - ${WF_ID}\" ID=\"workflow_${WF_ID}\">"
    for Y in $(sudo -u postgres psql -d bety -t -c "SELECT id, runtype FROM ensembles WHERE workflow_id=${WF_ID};"); do
        Y="${Y//[[:space:]]/}"
        ENS_ID="$(echo $Y | cut -d "|" -f 1)"
        ENS_TYPE="$(echo $Y | cut -d "|" -f 2)"
        echo "        <dataset name=\"${ENS_TYPE} - ${ENS_ID}\" ID=\"ensembles_${ENS_ID}\">"
        for RUN_ID in $(sudo -u postgres psql -d bety -t -c "SELECT id FROM runs WHERE ensemble_id=${ENS_ID};"); do
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
