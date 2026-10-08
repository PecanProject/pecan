#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00009-US_EDN

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00009-US_EDN)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00009-US_EDN", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.746000111653031, a1 = 1.04205283759265, Ha = 130.415709012612, Hd = 95.8243200715916, Ea_SOM = 18.243982523276, kM_SOM = 8275.20603849553, Ea_labile = 10.1180325071206, kM_labile = 48.1022316403687, Ea_SOM_CH4 = 83.39875799929, kM_SOM_CH4 = 17.1965519487127, Ea_labile_CH4 = 87.5581544952132, kM_labile_CH4 = 24.3317616708233, Ea_oxi_CH4 = 88.8014404154283, kM_oxi_CH4 = 24.5084240687405, kI_SO4 = 99.1316850029363, kI_NO3 = -1.03610123863222, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00009-US_EDN", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00009-US_EDN",
  sitelat = 37.615,
  sitelon = -122.114,
  start_date = "2018-04-03",
  end_date = "2021-06-16",
  delete_raw = FALSE
)'
