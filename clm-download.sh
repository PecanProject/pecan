#!/bin/bash
#CLM Compilation script

exec 3>&1
exec &> "clm-down.log"

cd models
cd clm45

wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm4_5_1_r085.tar.gz
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm/ccsm_inputdata.tar.gz

tar -xvzf clm4_5*
tar -xvzf ccsm_inputdata.tar.gz

mkdir /home/tonygard/pecan/models/clm45/ccsm_inputdata/lnd/clm2/paramdata
cd /models/ccsm_inputdata/lnd/clm2/paramdata
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm_params.c140423.nc
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clm_params.c130821.nc

cd /home/tonygard/pecan/models/clm45/ccsm_inputdata/share/domains/domain.clm
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/dpmain.lnd.1x1pt_US-UMB_navy.nc

cd /home/tonygard/pecan/models/clm45/ccsm_inputdata/atm/datm7/CLM1PT_data/1x1pt_US-UMB
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/all_hourly.nc

cd /home/tonygard/pecan/models/clm45/ccsm_inputdata/atm/datm7/NASA_LIS/
wget ftp://nacp.ornl.gov/synthesis/2008/firenze/site/clmforc.Li_2012_climo1995-2011.T62,lnfm_Total_c140423.nc 
