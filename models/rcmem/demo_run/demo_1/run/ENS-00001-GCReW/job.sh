#!/usr/bin/env bash


mkdir -p /Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW

# Redirect output
exec 3>&1
exec &> "$(realpath /Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run RCMEM
Rscript \
  -e 'drivers <- readRDS(file.path("/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//run/ENS-00001-GCReW", "drivers_RCMEM.rds"))' \
  -e 'parameters <- readRDS(file.path("/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//run/ENS-00001-GCReW", "parameters_RCMEM.rds"))' \
  -e 'res <- rCMEM::runCohortMem2(run_spinup = 1, 
                           run_scenario = 1,
                           
                           spinup_iterations = 50,
                           years_per_spinup_iter = 10,
                           
                           competition_function = 1,
                           
                           msl = drivers$msl,
                           mhwMat = drivers$mhwMat,
                           mlwMat = drivers$mlwMat,
                           flood_frequency = drivers$flood_frequency,
                           
                           scenario_calendar_years = drivers$scenario_calendar_years,
                           suspendedSediment = drivers$suspendedSediment,
 
                           rootShape = parameters$rootShape,
                           omDecayRateFast = parameters$omDecayRateFast,
                           omDecayRateSlow = parameters$omDecayRateSlow,
                           bMax = parameters$bMax,
                           rootTurnover = parameters$rootTurnover,
                           recalcitrantFrac = parameters$recalcitrantFrac,
                           rootDepthMax = parameters$rootDepthMax,
                           captureRate = parameters$captureRate,
                           species_codes = parameters$species_codes,
                           meanOmPackingDensity = parameters$meanOmPackingDensity, 
                           meanMineralPackingDensity = parameters$meanMineralPackingDensity,
                           zVegMax = parameters$zVegMax,
                           zVegPeak = parameters$zVegPeak, 
                           zVegMin = parameters$zVegMin,
                           abovegroundTurnover = parameters$abovegroundTurnover,
                           rootPackingDensity = parameters$rootPackingDensity,
                           rootToShoot = parameters$rootToShoot)
  ' \
  -e 'res2 <- rCMEM::calculateCarbonFlux(cohorts = res[[1]], scenario = res[[2]]) ' \
  -e 'write.csv(res2[[1]], file.path("/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW", "scenario_out.csv"), row.names = FALSE)'\
  -e 'write.csv(res[[1]], file.path("/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW", "cohorts_out.csv"), row.names = FALSE)'\
  -e 'write.csv(res[[3]], file.path("/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW", "species_out.csv"), row.names = FALSE)'\

# convert output to PEcAn format
Rscript -e 'PEcAn.RCMEM::model2netcdf.RCMEM(
  outdir = "/Users/jamesholmquist/GitHub/pecan/models/rcmem/demo_run/demo_1//out/ENS-00001-GCReW",
  sitelat = 38.874825,
  sitelon = -76.549052,
  start_date = "1928-01-01",
  end_date = "2100-12-31",
  delete_raw = FALSE
)'
