# Demo 1 Marginal vs MC Parameters

# Test
library(tidyverse)
# library(PEcAn.RCMEM)
# library(PEcAn.data.water)

# Create Site Table
sites <- data.frame(
  id = c("GCReW"), 
  latitude = c( 38.874825),	
  longitude = c(-76.549052)
)

# Find the nearest tide gauge data
sites_w_gauges <- PEcAn.data.water::findNearestNoaaGauge(sites)
(sites_w_gauges)

# Create scenario
scenario_output <- PEcAn.data.water::generateFullTidalScenario(
  station_id = sites_w_gauges$gauge_id,
  run_hindcast = T,
  run_forecast = T,
  hindcast_start = sites_w_gauges$startDate,
  forecast_start = 2018,
  forecast_end = 2100,
  scenario = c("ssp245"),
  confidence_level = "medium",
  target_quantile = 0.5,
  include_lt_tidal_const = T,
  datum_start_year = 1980,
  datum_end_year = 2025)

# Run the met2model function to format scenario as a PECAN input
PEcAn.RCMEM::met2model.RCMEM(scenario_output, 
                             outfolder = "demo_run/demo_1/data/met",
                             overwrite = T)

# Read in the scenario manifest produced by met2model
scenario_manifest <- read_csv("demo_run/demo_1/data/met/scenario_manifest.csv")

# Add some marginal posterior vegetation parameters from a different analysis
veg.distns <- data.frame(
  distn = c("gamma", "norm", "norm",
            "norm", "gamma", "gamma",
            "unif", "gamma", "unif"
  ),
  parama = c(74.08, -1.14, 1.84,
             1.48, 192.1, 34.2,
             1, 94.1, 0.07
  ),
  paramb = c(1406, 0.16, 0.12,
             0.08, 115.3, 63.2,
             1, 1.9, 0.07
  ),
  n = NA_integer_,
  row.names = c(
    "bMax","zVegMin","zVegMax",
    "zVegPeak","rootToShoot","rootTurnover",
    "abovegroundTurnover","rootDepthMax","rootPackingDensity"
  )
)

# Soil parameters
soil.distns <- data.frame(
  distn = c("gamma", "gamma", "beta", 
            "unif", "beta", "lnorm", 
            "lnorm", "norm",
            "norm", "norm"
            ),
  parama = c(39.7, 6.6, 80.0,
             0, 9.6, 1.7,
             -12.32, -13.9,
             0.40, 0.25
  ),
  paramb = c(399.6, 4.9, 4.3,
             0, 13.8, 0.4,
             0.34, 2.6,
             0.025, 0.025
  ),
  n = NA_integer_,
  row.names = c(
    "meanOmPackingDensity", "meanMineralPackingDensity", "omDecayRateFast",
    "omDecayRateSlow", "recalcitrantFrac", "captureRate",
    "suspendedSediment", "initElv",
    "omToOc_B1", "omToOc_B2"
  )
)

# Vegetation PFT
dir.create(
  "demo_run/demo_1/data/pfts/vegetation/scam/",
  recursive = TRUE,
  showWarnings = FALSE
)

post.distns <- veg.distns

save(
  post.distns,
  file = "demo_run/demo_1/data/pfts/vegetation/scam/post.distns.RData"
)

dir.create(
  "demo_run/demo_1/data/pfts/soil",
  recursive = TRUE,
  showWarnings = FALSE
)

post.distns <- soil.distns

save(
  post.distns,
  file = "demo_run/demo_1/data/pfts/soil/post.distns.RData"
)

# Need to create a table to associate sites with different PFTs. This could be useful later
# ... when we have sites in different regions with different vegetation and soil types
site_pfts <- data.frame( id = c("GCReW", "GCReW"),
                         pft_type = c("soil", "vegetation"),
                         pft_name = c("soil", "scam")
)

# PFT needs a manifest to connect pft names, types, and output file locations
pft_manifest <- data.frame(pft_name = c("soil", "scam"), 
                           pft_type = c("soil", "vegetation"),
                           posterior_file = c("demo_run/demo_1/data/pfts/soil/post.distns.RData",
                                              "demo_run/demo_1/data/pfts/vegetation/scam/post.distns.RData"
                           )
)

dir.create(
  "demo_run/demo_1/data/defaults",
  recursive = TRUE,
  showWarnings = FALSE
)

study_defaults <- list(
  run_spinup = 1,
  run_scenario = 1,
  spinup_iterations = 50,
  years_per_spinup_iter = 10,
  competition_function = 1,
  rootShape = 0)

saveRDS(
  study_defaults,
  "demo_run/demo_1/data/defaults/study_defaults.rds"
)

# Build setting. This will build your settings.xml file based on what'weve already generated
PEcAn.RCMEM::buildSettings.RCMEM(site_info = sites_w_gauges, # Table we created by hand 
                                 scenario_manifest = scenario_manifest, # Generated from met2model
                                 site_pfts = site_pfts, # We created this
                                 pft_manifest = pft_manifest, # We created this
                                 scenario = c("ssp245"), # Same input as generateFullTidalScenario
                                 confidence = "medium", # Same input as generateFullTidalScenario
                                 quantiles = c(0.5), # Same input as generateFullTidalScenario
                                 ensemble_variable = "surface_elevation", # Specify a variable
                                 outdir = "demo_run/demo_1/", # Specify an output location
                                 ensemble_size = 20,
                                 study_defaults = "demo_run/demo_1/data/defaults/study_defaults.rds" # we created this
)

# Read in the settings file created by buildSettings
settings_unprepared <-  PEcAn.settings::read.settings("demo_run/demo_1/settings_RCMEM_unprepared.xml")

settings_prepared <- PEcAn.settings::prepare.settings(settings_unprepared)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings_prepared
)

PEcAn.workflow::runModule_start_model_runs(settings)


runid <- read.csv(file.path(settings$outdir, "runs_manifest.csv"))$run_id
outdirs <- file.path(settings$outdir, "out", runid)
model_output <- purrr::map2_dfr(
  .x = runid |> setNames(nm = _),
  .y = outdirs,
  .f = function(id, dir) {
    PEcAn.utils::read.output(
      runid = id, outdir = dir,
      start.year = NA, end.year = NA,
      variables = c("surface_elevation","cFlux"),
      dataframe = TRUE,
      verbose = FALSE,
      print_summary = FALSE
    )
  },
  .id = "run_id"
)
available_vars <- names(model_output)[!names(model_output) %in% c("posix", "time_bounds")]

ggplot(model_output, aes(x = year, y = surface_elevation)) +
  geom_line(aes(group=run_id))

ggplot(model_output, aes(x = year, y = cFlux)) +
  geom_line(aes(group=run_id))

sample_files <- list.files(
  "demo_run/demo_1",
  pattern = "^samples\\.Rdata$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

sample_files
