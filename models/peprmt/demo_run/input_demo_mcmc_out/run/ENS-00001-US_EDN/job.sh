#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00001-US_EDN

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00001-US_EDN)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00001-US_EDN", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.744082512141497, a1 = 1.15544289485085, Ha = 119.589297901609, Hd = 93.6537118831357, Ea_SOM = 17.6939861598126, kM_SOM = 9907.39505784586, Ea_labile = 10.4679888669531, kM_labile = 94.4321891176514, Ea_SOM_CH4 = 83.3468674710418, kM_SOM_CH4 = 20.6126951904004, Ea_labile_CH4 = 87.3040938196751, kM_labile_CH4 = 21.0943910116999, Ea_oxi_CH4 = 91.3871460132901, kM_oxi_CH4 = 23.2262669730873, kI_SO4 = 100.248951010205, kI_NO3 = 2.46019191158601, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00001-US_EDN", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00001-US_EDN",
  sitelat = 37.615,
  sitelon = -122.114,
  start_date = "2018-04-03",
  end_date = "2021-06-16",
  delete_raw = FALSE
)'
