#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00007-US_DMG

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00007-US_DMG)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run PEPRMT
Rscript \
  -e 'dat <- read.csv(file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/run/ENS-00007-US_DMG", "run_data.csv"))' \
  -e 'res <- do.call(PEPRMT::run_PEPRMT, c(list(a0 = 0.890749794432893, a1 = 0.988174847707282, Ha = 115.567781642873, Hd = 92.789998815683, Ea_SOM = 18.4020209089187, kM_SOM = 11136.8087520823, Ea_labile = 9.8434272348644, kM_labile = 73.9116891263984, Ea_SOM_CH4 = 81.1380311243979, kM_SOM_CH4 = 15.8220342411473, Ea_labile_CH4 = 86.5086173890222, kM_labile_CH4 = 24.3152636739418, Ea_oxi_CH4 = 91.3704821565193, kM_oxi_CH4 = 24.2666909755986, kI_SO4 = 99.2956957344569, kI_NO3 = -0.0619561461539668, wetland_type = 2),
    list(data = dat)
  ))' \
  -e 'write.csv(res, file.path("/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00007-US_DMG", "out.csv"), row.names = FALSE)'

# convert output to PEcAn format
Rscript -e 'PEcAn.PEPRMT::model2netcdf.PEPRMT(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/peprmt/demo_run/input_demo_mcmc_out/out/ENS-00007-US_DMG",
  sitelat = 38.0015,
  sitelon = -121.6691,
  start_date = "2021-12-15",
  end_date = "2024-12-19",
  delete_raw = FALSE
)'
