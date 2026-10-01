#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00010-US_SRR

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00010-US_SRR)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00010-US_SRR", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.580283848897826, a1 = 1.24776432920409, Ha = 185.792033987053, Hd = 94.1109798089999, Ea_SOM = 19.0027692387458, kM_SOM = 1491.78908625618, Ea_labile = 10.800680161689, kM_labile = 14.6228672633879, Ea_SOM_CH4 = 81.9074667936285, kM_SOM_CH4 = 16.7695320046497, Ea_labile_CH4 = 86.3038354900633, kM_labile_CH4 = 23.5078783271687, Ea_oxi_CH4 = 89.8802245419512, kM_oxi_CH4 = 23.5666958845237, kI_SO4 = 100.747605702895, kI_NO3 = 1.08929815917092, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00010-US_SRR", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00010-US_SRR",
  sitelat = 38.2,
  sitelon = -122.026,
  start_date = "2014-03-12",
  end_date = "2018-09-20",
  delete_raw = FALSE
)'
