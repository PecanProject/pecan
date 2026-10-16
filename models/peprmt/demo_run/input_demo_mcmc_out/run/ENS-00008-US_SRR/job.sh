#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00008-US_SRR

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00008-US_SRR)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00008-US_SRR", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.750025565823059, a1 = 1.02833406681492, Ha = 170.319010211072, Hd = 92.742350914081, Ea_SOM = 16.2590846682488, kM_SOM = 10532.7402998228, Ea_labile = 8.25630382023339, kM_labile = 79.8311578319408, Ea_SOM_CH4 = 81.5442865460709, kM_SOM_CH4 = 18.2147819535662, Ea_labile_CH4 = 87.386127935059, kM_labile_CH4 = 21.8944314520137, Ea_oxi_CH4 = 91.8076738368089, kM_oxi_CH4 = 25.3983789005783, kI_SO4 = 99.5123807058774, kI_NO3 = 0.357012392234241, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00008-US_SRR", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00008-US_SRR",
  sitelat = 38.2,
  sitelon = -122.026,
  start_date = "2014-03-12",
  end_date = "2018-09-20",
  delete_raw = FALSE
)'
