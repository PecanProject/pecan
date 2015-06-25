# convenience functions used in add.data.sh and add.models.sh
# these functions can be used by others as well to add more
# data, models, etc.

# fully qualified hostname
if [ -z "$FQDN" ]; then
	FQDN=$( hostname -f )
fi

# command to execute to add items to BETY database
if [ -z "$PSQL" ]; then
	PSQL="psql -U bety bety -q -t -c"
fi

# function to add a input, takes 2 parameters. Will set FORMAT_ID to
# format that is created, or found.
#
# 1 : mimetype
# 2 : name
addFormat() {
    FORMAT_ID=$( ${PSQL} "SELECT id FROM formats WHERE mime_type='$1' AND name='$2' LIMIT 1;" )
    if [ "$FORMAT_ID" == "" ]; then
        ${PSQL} "INSERT INTO formats (mime_type, name, created_at, updated_at) VALUES ('$1', '$2', NOW(), NOW());"
        FORMAT_ID=$( ${PSQL} "SELECT id FROM formats WHERE mime_type='$1' AND name='$2' LIMIT 1;" )
        echo "Added new format with ID=${FORMAT_ID} for mime_type=$1, name=$2"
    fi
}

# function to add a input, takes 4 parameters. Will set INPUT_ID to
# the input that is created, or found.
# 1 : site id
# 2 : format id
# 3 : start date of input data
# 4 : end date of input data
addInput() {
    INPUT_ID=$( ${PSQL} "SELECT id FROM inputs WHERE site_id=$1 AND format_id=$2 AND start_date='$3' AND end_date='$4' LIMIT 1;" )
    if [ "$INPUT_ID" == "" ]; then
        ${PSQL} "INSERT INTO inputs (site_id, format_id, name, start_date, end_date, created_at, updated_at) VALUES ($1, $2, '', '$3', '$4', NOW(), NOW());"
        INPUT_ID=$( ${PSQL} "SELECT id FROM inputs WHERE site_id=$1 AND format_id=$2 AND start_date='$3' AND end_date='$4' LIMIT 1;" )
        echo "Added new input with ID=${INPUT_ID} for site=$1, format_id=$2, start=$3, end=$4"
    fi
}

# function to add a model, takes 4 parameters
# 1 : fully qualified hostname
# 2 : id of input
# 3 : name of file, without the path
# 4 : path to the file
addInputFile() {
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

# function to add a model, takes 6 parameters
# 1 : fully qualified hostname
# 2 : name of the model, shown in web interface
# 3 : type of model (ED2, SIPNET, BIOCRO, DALEC, ...)
# 4 : model revision number
# 5 : name of executable, without the path
# 6 : path to the executable
addModelFile() {
	HOSTID="(SELECT id FROM machines WHERE hostname='${1}')"
  MODELTYPEID="(SELECT id FROM modeltypes WHERE modeltypes.name='${3}')"
  MODELID="(SELECT models.id FROM models, modeltypes WHERE model_name='${2}' AND modeltypes.name='${3}' AND modeltypes.id=models.modeltype_id AND revision='${4}')"

	# Make sure host exists
	${PSQL} "INSERT INTO machines (hostname, created_at, updated_at)
		SELECT *, now(), now() FROM (SELECT '${1}') AS tmp WHERE NOT EXISTS ${HOSTID};"

	# Make sure modeltype exists
  ${PSQL}  "INSERT INTO modeltypes (name, created_at, updated_at)
    SELECT *, now(), now() FROM (SELECT '${3}') AS tmp WHERE NOT EXISTS ${MODELTYPEID};"

  # Make sure model exists
	${PSQL}  "INSERT INTO models (model_name, modeltype_id, revision, created_at, updated_at)
		SELECT *, now(), now() FROM (SELECT '${2}', ${MODELTYPEID}, '${4}') AS tmp WHERE NOT EXISTS ${MODELID};"

  # check if binary already added
  COUNT=$( ${PSQL} "SELECT COUNT(id) FROM dbfiles WHERE container_type='Model' AND container_id=${MODELID} AND file_name='${5}' AND file_path='${6}' and machine_id=${HOSTID};" )
  if [ "$COUNT" -gt 0 ]; then
      echo "File ${6}/${5} already added to model ${2} on host ${1}."
      return 0
  fi

	# Add binary
	${PSQL} "INSERT INTO dbfiles (container_type, container_id, file_name, file_path, machine_id) VALUES
		    ('Model', ${MODELID}, '${5}','${6}', ${HOSTID});"

	echo "File ${6}/${5} added to model ${2} on host ${1}."
}

# function to add a model if found on local machine, takes 4 paramters
# 1 : name of the model, shown in web interface
# 2 : type of model (ED2, SIPNET, BIOCRO, DALEC, ...)
# 3 : model revision number
# 4 : name of executable, without the path (assumed it can be found in path)
addLocalModel() {
    BINARY=$( which $4 )
    if [ "${BINARY}" != "" ]; then
        addModelFile "${FQDN}" "$1" "$2" "$3" "$4" $( dirname $BINARY )
    fi
}
