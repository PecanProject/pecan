#!/bin/bash
#
#
#=============================================================================================
#
# Generate CLM5-FATES reference run for PEcAn site-scale simulations
# 
# Users should update all of the lines with -USER UPDATE to match their local environment
#
# --- Last updated by Shawn P. Serbin <sserbin@bnl.gov>
#=============================================================================================

# Define CTSM-FATES options
export MODEL_SOURCE=/data/software/CTSM/ctsm                   	# CTSM/CLM/ELM source directory - USER UPDATE
export PROJECT=PEcAn_simulations                               	# project or run tag
export COMPSET=I2000Clm50FatesGs				# define compset for run
export RES=1x1_brazil						# set grid resolution
export MAC=eddi                                             	# Name your machine - USER UPDATE
export COMPILER=gnu                                          	# Name your compiler - USER UPDATE
export CASEROOT=/data/software/FATES/FATES_refrun               # Where the build is generated. The ref case location - USER UPDATE

# Set CLM & FATES parameter path and param files here:
# USER needs to define the name and location of the FATES parameter file to use for PEcAn simulations here
# This file is then copied to the refcase location and used for all PEcAn runs based on that refcase
export FATES_PARAM_FILE_PATH=/data/software/CTSM/fates_parameter_files 			# - USER UPDATE
export FATES_PARAM_FILE=fates_params_2troppftclones.c171018_sps.nc			# - USER UPDATE
export CLM_PARAM_FILE_PATH=/data/Model_Data/cesm_input_datasets/lnd/clm2/paramdata	# - USER UPDATE
export CLM_PARAM_FILE=clm5_params.c171117.nc						# - USER UPDATE

# Define output options
export date_var=$(date +%s)
echo "*** Build start: ${date_var} "
export TAG='PEcAn_ref_run03'
export MODEL_VERSION=CLM5-FATES
export CLM_HASH=`(cd ${MODEL_SOURCE};git log -n 1 --pretty=%h)`
export FATES_HASH=`(cd ${MODEL_SOURCE}/src/fates;git log -n 1 --pretty=%h)`
export GIT_HASH=C${CLM_HASH}-F${FATES_HASH}
export CASE_NAME=${CASEROOT}/${MODEL_VERSION}_${TAG}
echo "*** Building CASE: ${CASE_NAME} "
# =======================================================================================

# =======================================================================================
# REMOVE EXISTING CASE IF PRESENT
rm -r ${CASE_NAME}

# CREATE THE CASE
cd ${MODEL_SOURCE}/cime/scripts/
echo $PWD
echo "Running with HLM-FATES location: "${MODEL_SOURCE}
./create_newcase --case ${CASE_NAME} --res ${RES} --compset ${COMPSET} --mach ${MAC} --compiler ${COMPILER} --project ${PROJECT} --run-unsupported

echo "*** Switching directory to CASE: ${CASE_NAME} "
cd ${CASE_NAME}
echo ${PWD}

# Copy parameter file to case
echo "*** Copy FATES parameter file ***"
echo " "
cp ${FATES_PARAM_FILE_PATH}/${FATES_PARAM_FILE} .
echo "*** Copy CLM parameter file ***"
echo " "
cp ${CLM_PARAM_FILE_PATH}/${CLM_PARAM_FILE} .
# =======================================================================================

# =======================================================================================
# Modifying : env_mach_pes.xml
echo "*** Modifying xmls  ***"
./xmlchange --id DEBUG --val FALSE
./xmlchange --file env_run.xml --id PIO_DEBUG_LEVEL --val 0
./xmlchange --id STOP_N --val 1
./xmlchange --id RUN_STARTDATE --val '1980-01-01'
./xmlchange --id STOP_OPTION --val nyears
./xmlchange --id REST_N --val 1
./xmlchange --id REST_OPTION --val nyears
./xmlchange --id CLM_FORCE_COLDSTART --val on
./xmlchange --id RESUBMIT --val 1
./xmlchange --id DATM_CLMNCEP_YR_START --val 1999
./xmlchange --id DATM_CLMNCEP_YR_END --val 1999

./xmlchange --file env_run.xml --id DOUT_S_SAVE_INTERIM_RESTART_FILES --val TRUE
./xmlchange --file env_run.xml --id DOUT_S --val TRUE
./xmlchange --file env_run.xml --id DOUT_S_ROOT --val '$CASEROOT/run'
./xmlchange --file env_run.xml --id RUNDIR --val ${CASE_NAME}/run
./xmlchange --file env_build.xml --id EXEROOT --val ${CASE_NAME}/bld

# Set run location to case dir
./xmlchange --file env_build.xml --id CIME_OUTPUT_ROOT --val ${CASE_NAME}
# =======================================================================================

# =======================================================================================
#./cesm_setup
echo "*** Running case.setup ***"
./case.setup

cat >> user_nl_clm <<EOF
hist_empty_htapes = .true.
hist_fincl1='NEP','NPP','GPP','TLAI','TSOI_10CM','QVEGT','EFLX_LH_TOT','AR','HR','ED_biomass','ED_bleaf','ED_balive','DDBH_SCPF','BA_SCPF','NPLANT_SCPF','M1_SCPF','M2_SCPF','M3_SCPF','M4_SCPF','M5_SCPF','M6_SCPF','GPP_BY_AGE','PATCH_AREA_BY_AGE','CANOPY_AREA_BY_AGE','BA_SCLS','NPLANT_CANOPY_SCLS','NPLANT_UNDERSTORY_SCLS','DDBH_CANOPY_SCLS','DDBH_UNDERSTORY_SCLS','MORTALITY_CANOPY_SCLS','MORTALITY_UNDERSTORY_SCLS','WIND','ZBOT','FSDS','RH','TBOT','PBOT','QBOT','RAIN','FLDS'
hist_mfilt             = 8760
hist_nhtfrq            = -1
EOF

echo "*** Update CLM && FATES parameter files ***"
echo " "
cat >> user_nl_clm <<EOF
fates_paramfile = "${CASE_NAME}/${FATES_PARAM_FILE}"
paramfile = "${CASE_NAME}/${CLM_PARAM_FILE}"
EOF

echo *** Build case ***
./case.build


echo "*** Finished building new case in CASE: ${CASE_NAME} "
# =======================================================================================
