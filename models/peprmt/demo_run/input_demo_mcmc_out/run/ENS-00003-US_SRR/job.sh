#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00003-US_SRR

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00003-US_SRR)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00003-US_SRR", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.787846779611897, a1 = 1.04591939425609, Ha = 119.886306145119, Hd = 93.9705905262127, Ea_SOM = 17.6298939676151, kM_SOM = 7044.78489025496, Ea_labile = 8.93742856112326, kM_labile = 25.7129413890652, Ea_SOM_CH4 = 83.7802619406367, kM_SOM_CH4 = 16.4578386053158, Ea_labile_CH4 = 86.5199786992674, kM_labile_CH4 = 23.4389727208756, Ea_oxi_CH4 = 91.1589099444955, kM_oxi_CH4 = 22.3433710250756, kI_SO4 = 100.055292150658, kI_NO3 = -0.930553793955635, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00003-US_SRR", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00003-US_SRR",
  sitelat = 38.2,
  sitelon = -122.026,
  start_date = "2014-03-12",
  end_date = "2018-09-20",
  delete_raw = FALSE
)'
