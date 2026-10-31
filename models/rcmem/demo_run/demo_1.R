# Demo 1 Marginal vs MC Parameters

# Test
library(tidyverse)
library(PEcAn.RCMEM)
library(PEcAn.data.water)

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
            "gamma"
  ),
  parama = c(74.08, -1.14, 1.84,
             1.48, 192.1, 34.2,
             94.1
  ),
  paramb = c(1406, 0.16, 0.12,
             0.08, 115.3, 63.2,
             1.9
  ),
  n = NA_integer_,
  row.names = c(
    "bMax","zVegMin","zVegMax",
    "zVegPeak","rootToShoot","rootTurnover",
    "rootDepthMax"
  )
)

# Soil parameters
soil.distns <- data.frame(
  distn = c("gamma", "gamma", "beta",
            "beta", "lnorm", 
            "lnorm", "norm"
            ),
  parama = c(39.7, 6.6, 80.0,
            9.6, 1.7,
             -12.32, -13.9
  ),
  paramb = c(399.6, 4.9, 4.3,
             13.8, 0.4,
             0.34, 2.6
  ),
  n = NA_integer_,
  row.names = c(
    "meanOmPackingDensity", "meanMineralPackingDensity", "omDecayRateFast",
    "recalcitrantFrac", "captureRate",
    "suspendedSediment", "initElv"
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
  rootShape = 0,
  abovegroundTurnover = 1,
  omDecayRateSlow = 0,
  rootPackingDensity = 0.07
  )

saveRDS(
  study_defaults,
  "demo_run/demo_1/data/defaults/study_defaults.rds"
)

# Build setting. This will build your settings.xml file based on what'weve already generated
setup <- PEcAn.RCMEM::buildSettings.RCMEM(
  site_info = sites_w_gauges,
  scenario_manifest = scenario_manifest,
  site_pfts = site_pfts,
  pft_manifest = pft_manifest,
  scenario = "ssp245",
  confidence = "medium",
  quantiles = 0.5,
  ensemble_variable = "surface_elevation",
  ensemble_size = 20,
  study_defaults =
    "demo_run/demo_1/data/defaults/study_defaults.rds",
  samples = NULL,
  outdir = "demo_run/demo_1/"
)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings = setup$settings,
  input_design = setup$input_design
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

## MCMC samples
library(arrow)

target_params <- c("initElv",  "recalcitrantFrac", "omDecayRate", "captureRate",
                   "suspendedSediment", "bMax", "zVegMin", "zVegMax", 
                   "zVegPeak", "rootToShoot", "rootDepthMax", "mineralPackingDensity", 
                   "omPackingDensity", "rootTurnover")

soil_params <- c("initElv",  "recalcitrantFrac", "omDecayRateFast", "captureRate",
                 "suspendedSediment", "meanMineralPackingDensity", 
                 "meanOmPackingDensity")

# Need to update omDecayRate to omDecayRateFast
# mineralPackingDensity to meanMineralPackingDensity
# omPackingDensity to meanOmPackingDensity

# Add abovegroundTurnover, rootPackingDensity, omDecayRateSlow

mcmc_long <- read_parquet("inst/demo_inputs/StanHMCoutputs.parquet") %>%
  filter(Parameter %in% target_params) %>% 
  mutate(Parameter = recode(Parameter, "omDecayRate"="omDecayRateFast",
                            "mineralPackingDensity" = "meanMineralPackingDensity",
                            "omPackingDensity" = "meanOmPackingDensity"
                            ),
         pft = ifelse(Parameter %in% soil_params, "soil", "scam"))

hmc_samples <- mcmc_to_pecan_samples.RCMEM(mcmc = mcmc_long,
                                       ensemble_size = 20
                                       )

# Run the met2model function to format scenario as a PECAN input
PEcAn.RCMEM::met2model.RCMEM(scenario_output, 
                             outfolder = "demo_run/demo_1b/data/met",
                             overwrite = T)

# Read in the scenario manifest produced by met2model
scenario_manifest <- read_csv("demo_run/demo_1b/data/met/scenario_manifest.csv")

dir.create(
  "demo_run/demo_1b/data/defaults",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  study_defaults,
  "demo_run/demo_1b/data/defaults/study_defaults.rds"
)

pft_manifest_b <- pft_manifest %>% 
  select(-posterior_file)

setup_b <- PEcAn.RCMEM::buildSettings.RCMEM(
  site_info = sites_w_gauges,
  scenario_manifest = scenario_manifest,
  site_pfts = site_pfts,
  pft_manifest = pft_manifest_b,
  scenario = "ssp245",
  confidence = "medium",
  quantiles = 0.5,
  ensemble_variable = "surface_elevation",
  ensemble_size = 20,
  study_defaults =
    "demo_run/demo_1b/data/defaults/study_defaults.rds",
  samples = hmc_samples$samples,
  outdir = "demo_run/demo_1b/"
)

settings <- PEcAn.workflow::runModule.run.write.configs(
  settings = setup_b$settings,
  input_design = setup_b$input_design
)

PEcAn.workflow::runModule_start_model_runs(settings)

runid <- read.csv(file.path(settings$outdir, "runs_manifest.csv"))$run_id
outdirs <- file.path(settings$outdir, "out", runid)
model_output_b <- purrr::map2_dfr(
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

vis_df <- bind_rows(model_output %>% mutate(run_type = "marginal"),
                    model_output_b %>% mutate(run_type = "HMC chains")
                    ) %>% 
  pivot_longer(-c(run_id, posix, year, run_type)) %>% 
  filter(year >= 1928) 

cis <- vis_df %>% 
  group_by(year, run_type, name) %>% 
  summarise(lower_ci = quantile(value, 0.1),
            upper_ci = quantile(value, 0.9)
            ) %>% 
  ungroup()


ggplot(vis_df, aes(x = year, color = run_type)) +
  geom_line(aes(group=paste(name, run_id, run_type),  y = value)) +
  # geom_point(aes(y = value), col = "grey", alpha=0.1) +
  geom_ribbon(data = cis, aes(x = year, ymin = lower_ci, ymax = upper_ci, fill = run_type), alpha = 0.5) +
  facet_wrap(.~name, scale = "free_y")
  
