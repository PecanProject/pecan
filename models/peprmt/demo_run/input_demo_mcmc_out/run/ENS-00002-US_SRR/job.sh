#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00002-US_SRR

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00002-US_SRR)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00002-US_SRR", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.723527804396939, a1 = 1.09862907574448, Ha = 163.611527947122, Hd = 95.6091413790716, Ea_SOM = 19.2551728524657, kM_SOM = 9613.85612469167, Ea_labile = 14.538019993507, kM_labile = 27.3137743165717, Ea_SOM_CH4 = 82.3627879745686, kM_SOM_CH4 = 17.4807210253244, Ea_labile_CH4 = 88.3998511520175, kM_labile_CH4 = 23.2467628258062, Ea_oxi_CH4 = 90.4348840676722, kM_oxi_CH4 = 23.2077088596448, kI_SO4 = 100.163231444666, kI_NO3 = 1.5888262185248, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00002-US_SRR", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00002-US_SRR",
  sitelat = 38.2,
  sitelon = -122.026,
  start_date = "2014-03-12",
  end_date = "2018-09-20",
  delete_raw = FALSE
)'
