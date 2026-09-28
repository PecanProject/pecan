{
  
  drivers <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run/ENS-00001-SERC/", "drivers_RCMEM.rds"))
  inits <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run/ENS-00001-SERC/", "inits_RCMEM.rds"))
  parameters <- readRDS(file.path("models/rcmem/demo_run/input_demo_out/run/ENS-00001-SERC/", "parameters_RCMEM.rds"))

  
  run_spinup = 0 
run_scenario = 1
competition_function = 1

msl = drivers$msl
mhwMat = drivers$mhwMat
mlwMat = drivers$mlwMat
flood_frequency = drivers$flood_frequency

scenario_calendar_years = drivers$scenario_calendar_years
suspendedSediment = drivers$suspendedSediment

initial_scenario = inits$initial_scenario
initial_cohorts = inits$initial_cohorts
initial_species = inits$initial_species

rootShape = parameters$rootShape
omDecayRateFast = parameters$omDecayRateFast
omDecayRateSlow = parameters$omDecayRateSlow
bMax = parameters$bMax
rootTurnover = parameters$rootTurnover
recalcitrantFrac = parameters$recalcitrantFrac
rootDepthMax = parameters$rootDepthMax
captureRate = parameters$captureRate
species_codes = parameters$species_codes
meanOmPackingDensity = parameters$meanOmPackingDensity 
meanMineralPackingDensity = parameters$meanMineralPackingDensity
zVegMax = parameters$zVegMax
zVegPeak = parameters$zVegPeak 
zVegMin = parameters$zVegMin
abovegroundTurnover = parameters$abovegroundTurnover
rootPackingDensity = parameters$rootPackingDensity
rootToShoot = parameters$rootToShoot
}

write.csv(res[[2]], file.path("models/rcmem/demo_run/input_demo_out/out/ENS-00001-SERC/", "scenario_out.csv"), row.names = FALSE)
write.csv(res[[1]], file.path("models/rcmem/demo_run/input_demo_out/out/ENS-00001-SERC/", "cohorts_out.csv"), row.names = FALSE)
write.csv(res[[3]], file.path("models/rcmem/demo_run/input_demo_out/out/ENS-00001-SERC/", "species_out.csv"), row.names = FALSE)

PEcAn.RCMEM::model2netcdf.RCMEM(
  outdir = "models/rcmem/demo_run/input_demo_out/out/ENS-00001-SERC/",
  sitelat = 38.874487,
  sitelon = -76.549925,
  start_date = 1928,
  end_date = 2018,
  delete_raw = F
)
