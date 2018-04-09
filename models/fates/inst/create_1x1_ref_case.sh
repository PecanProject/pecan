#!/bin/bash

# Modified script originally provided by Ryan Knox
# To be run from ed-clm/cime/scripts to generate point reference case
# After run, need to insert model record into BETY
# In this case the whole CASE directory is the 'executable'
#
#=============================================================================================
#
# The purpose of this script is to generate a high resolution global grid for the land model
#   and then simply mask out all of the sites with the exception of a list of key locations.
#   Then we go in and make special modifications.
#
# Current list of sites: BCI-PANAMA, KORUP-CAMEROON, ZF2-BRAZIL, 
#                        ITURI-DRC, LAMBIR-MALAYSIA, HUAI KHA KHAENG-THAILAND
#
#
# site_lat_c =   9.1543, 5.07389,  -2.60909722,  1.4368,   4.1865,  15.6324
# site_lon_c = 280.1539, 8.85472, 299.7907,     28.5826, 114.017,   99.217
#=============================================================================================

#Optional netcdf explicit settings
export NETCDF_HOME=/usr/local/  
export NETCDF_PATH=${NETCDF_HOME}

CIME_MODEL=cesm

MACH=eddi
COMP=ICLM45ED
GITHASH=`git log -n 1 --format=%h`
CASE=ref1x1_${GITHASH}

CROOT=/home/carya/FATES_refrun/ # Define path where run will be written to

DIN_LOC_ROOT=/home/carya/FATESinput/ # Defiune path to input data

DOMAIN_PATH=${DIN_LOC_ROOT}/share/domains/

WORKDIR=`pwd`

export CASEROOT=${CROOT}${CASE}
echo "CREATING NEW CASE IN "${CASEROOT}

rm -rf ${CASEROOT}
./create_newcase -case ${CASEROOT} -res 1x1_brazil -compset ${COMP} -mach eddi -compiler gnu

cd ${CASEROOT}

# Modifying : env_mach_pes.xml
echo "*** Modifying xmls  ***"
./xmlchange -file env_mach_pes.xml -id NTASKS_ATM -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_LND -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_ICE -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_OCN -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_CPL -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_GLC -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_ROF -val 1
./xmlchange -file env_mach_pes.xml -id NTASKS_WAV -val 1
./xmlchange -file env_mach_pes.xml -id MAX_TASKS_PER_NODE -val 1
./xmlchange -file env_mach_pes.xml -id TOTALPES -val 1
# Modifying : env_build.xml
./xmlchange -file env_build.xml -id CIME_OUTPUT_ROOT -val ${CASEROOT}
./xmlchange -file env_build.xml -id GMAKE -val make
#./xmlchange -file env_build.xml -id MPILIB -val openmpi
#./xmlchange -file env_build.xml -id OS -val Linux
#./xmlchange -file env_build.xml -id COMPILER -val gnu
./xmlchange -file env_build.xml -id DEBUG -val FALSE
#./xmlchange -file env_build.xml -id SUPPORTED_BY -val 'clm-ed test case'
./xmlchange -file env_build.xml -id EXEROOT -val ${CASEROOT}/bld

#./xmlchange -file env_run.xml -id CLM_BLDNML_OPTS -val "-no-megan" --append

# Modifying : env_run.xml
./xmlchange -file env_run.xml -id STOP_N -val 2
./xmlchange -file env_run.xml -id RUN_STARTDATE -val '1500-01-01'
./xmlchange -file env_run.xml -id STOP_OPTION -val nyears
./xmlchange -file env_run.xml -id REST_N -val 1
./xmlchange -file env_run.xml -id REST_OPTION -val nyears
./xmlchange -file env_run.xml -id DATM_CLMNCEP_YR_START -val 1999
./xmlchange -file env_run.xml -id DATM_CLMNCEP_YR_END -val 1999

##./xmlchange -file env_run.xml -id DATM_CO2_TSERIES -val rcp4.5
##./xmlchange -file env_run.xml -id CLM_CO2_TYPE -val 'diagnostic'
./xmlchange -file env_run.xml -id DIN_LOC_ROOT -val ${DIN_LOC_ROOT}
./xmlchange -file env_run.xml -id DIN_LOC_ROOT_CLMFORC -val '$DIN_LOC_ROOT'
#./xmlchange -file env_run.xml -id DOUT_S_SAVE_INTERIM_RESTART_FILES -val TRUE
./xmlchange -file env_run.xml -id DOUT_S -val TRUE
./xmlchange -file env_run.xml -id DOUT_S_ROOT -val '$CASEROOT/run'
./xmlchange -file env_run.xml -id RUNDIR -val ${CASEROOT}/run
./xmlchange -file env_run.xml -id PIO_DEBUG_LEVEL -val 0

#./cesm_setup
echo "*** Running case.setup ***"
./case.setup

# Modify run script
#

#hist_fincl2='ED_GPP_GD_SCPF','ED_NPP_LEAF_GD_SCPF',,'ED_NPP_FNRT_GD_SCPF',
#'ED_NPP_BGSW_GD_SCPF','ED_NPP_BGDW_GD_SCPF','ED_NPP_AGSW_GD_SCPF','ED_NPP_AGDW_GD_SCPF',
#'ED_NPP_STOR_GD_SCPF','ED_NPP_SEED_GD_SCPF','ED_DDBH_GD_SCPF','ED_BA_GD_SCPF',
#'ED_NPLANT_GD_SCPF','ED_M1_GD_SCPF','ED_M2_GD_SCPF','ED_M3_GD_SCPF','ED_M4_GD_SCPF','ED_M5_GD_SCPF'
#hist_mfilt             = 480,12
#hist_nhtfrq            = -1,0
#hist_fincl1='EFLX_LH_TOT','FSH','TSOI_10CM','QVEGT','FLDS','FSDS','RAIN','NPP','CBALANCE_ERROR_BGC','CBALANCE_ERROR_ED','CBALANCE_ERROR_TOTAL'
#finidat = ''
#hist_empty_htapes = .true.
#hist_fincl1='EFLX_LH_TOT','QVEGT','TRIMMING','TRIMMING2','AREA_PLANT','AREA_PLANT2','AREA_TREES','AREA_TREES2','CANOPY_SPREAD','CANOPY_SPREAD2','PFTbiomass','PFTBIOMASS2','PFTleafbiomass','PFTLEAFBIOMASS2','PFTstorebiomass','PFTSTOREBIOMASS2','PFTnindivs','PFTNINDIVS2','FIRE_NESTEROV_INDEX2','FIRE_NESTEROV_INDEX','FIRE_ROS2','FIRE_ROS','EFFECT_WSPEED2','EFFECT_WSPEED','FIRE_TFC_ROS2','FIRE_TFC_ROS','FIRE_INTENSITY2','FIRE_INTENSITY','FIRE_AREA','FIRE_AREA2','SCORCH_HEIGHT2','SCORCH_HEIGHT','FIRE_FUEL_MEF2','fire_fuel_mef','FIRE_FUEL_BULKD2','fire_fuel_bulkd','FIRE_FUEL_EFF_MOIST2','fire_fuel_eff_moist','FIRE_FUEL_SAV2','fire_fuel_sav','SUM_FUEL2','SUM_FUEL','LITTER_IN2','LITTER_IN','LITTER_OUT2','LITTER_OUT','SEED_BANK2','SEED_BANK','SEEDS_IN2','SEEDS_IN','SEED_GERMINATION2','SEED_GERMINATION','SEED_DECAY2','SEED_DECAY','BSTORE2','ED_bstore','BDEAD2','ED_bdead','ED_balive','BALIVE2','ED_bleaf','BLEAF2','ED_biomass','BTOTAL2','GPP2','NPP2','ARESP2','MAINT_RESP2','GROWTH_RESP2','GPP','NPP','AR','GROWTH_RESP','MAINT_RESP'

cat >> user_nl_clm << \EOF
hist_empty_htapes = .true.
hist_fincl1='EFLX_LH_TOT','TSOI_10CM','QVEGT','GPP','AR','ED_bleaf','ED_biomass','NPP','MAINT_RESP','GROWTH_RESP'
hist_mfilt             = 8760
hist_nhtfrq            = -1
EOF

#cat >> user_nl_clm << \EOF
#finidat = ''
#EOF

# Modify user_nl_datm
cat >> user_nl_datm << EOF
EOF
# building case :
#./${CASE}.build

echo "*** Running case.build ***"
./case.build

#cd $WORKDIR
## STEPS TO SET UP FOR GENERAL USE
## ADDED BY MCD
