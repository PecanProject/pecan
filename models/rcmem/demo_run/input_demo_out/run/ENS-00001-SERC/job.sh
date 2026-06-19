#!/usr/bin/env bash


mkdir -p models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC

# Redirect output
exec 3>&1
exec &> "$(realpath models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC)/logfile.txt"

# host specific setup


# cdo setup
# @CDO_SETUP@

# Run RCMEM
Rscript \
  -e 'drivers <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run//ENS-00001-SERC", "drivers_RCMEM.rds"))' \
  -e 'parameters <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run//ENS-00001-SERC", "parameters_RCMEM.rds"))' \
  -e 'inits <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run//ENS-00001-SERC", "inits_RCMEM.rds"))' \
  -e 'res <- rCMEM::runCohortMem2(run_spinup = 0, 
                           run_scenario = 1,
                           competition_function = 1,
                           
                           msl = drivers$msl,
                           mhwMat = drivers$mhwMat,
                           mlwMat = drivers$mlwMat,
                           flood_frequency = drivers$flood_frequency,
                           
                           scenario_calendar_years = drivers$scenario_calendar_years,
                           suspendedSediment = drivers$suspendedSediment,
                           
                           initial_scenario = inits$initial_scenario,
                           initial_cohorts = inits$initial_cohorts,
                           initial_species = inits$initial_species,
                           
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
  -e 'write.csv(res[[1]], file.path("models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC", "scenario_out.csv"), row.names = FALSE)'\
  -e 'write.csv(res[[2]], file.path("models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC", "cohorts_out.csv"), row.names = FALSE)'\
  -e 'write.csv(res[[3]], file.path("models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC", "species_out.csv"), row.names = FALSE)'\

# convert output to PEcAn format
Rscript -e 'PEcAn.RCMEM::model2netcdf.RCMEM(
  outdir = "models/rcmem/demo_run/input_demo_out/out//ENS-00001-SERC",
  sitelat = 38.874544,
  sitelon = -76.548628,
  start_date = "1928",
  end_date = "2018",
  delete_raw = FALSE
)'
