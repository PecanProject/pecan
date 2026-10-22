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


PEcAn.RCMEM::met2model.RCMEM(scenario_output, 
                             outfolder = "demo_run/demo_1/data/met",
                             overwrite = T)

scenario_manifest <- read_csv("demo_run/demo_1/data/met/scenario_manifest.csv")

# Vegetation parameters
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
            "lnorm", "norm"),
  parama = c(39.7, 6.6, 80.0,
             0.001, 9.6, 1.7,
             -12.32, -13.9
  ),
  paramb = c(399.6, 4.9, 4.3,
             0.001, 13.8, 0.4,
             0.34, 2.6
  ),
  n = NA_integer_,
  row.names = c(
    "meanOmPackingDensity", "meanMineralPackingDensity", "omDecayRateFast",
    "omDecayRateSlow", "recalcitrantFrac", "captureRate",
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
  veg.distns,
  file = "demo_run/demo_1/data/pfts/vegetation/scam/post.distns.RData"
)

post.distns <- veg.distns
# Soil PFT
dir.create(
  "demo_run/demo_1/data/pfts/soil",
  recursive = TRUE,
  showWarnings = FALSE
)

save(
  post.distns,
  file = "demo_run/demo_1/data/pfts/soil/soil.RData"
)

site_pfts <- data.frame( id = c("GCReW", "GCReW"),
                         pft_type = c("soil", "vegetation"),
                         pft_name = c("soil", "scam")
)

pft_manifest <- data.frame(pft_name = c("soil", "scam"), 
                           pft_type = c("soil", "vegetation"),
                           posterior_file = c("demo_run/demo_1/data/pfts/soil/post.distns.RData",
                                              "demo_run/demo_1/data/pfts/vegetation/scam/post.distns.RData"
                           )
)

PEcAn.RCMEM::buildSettings.RCMEM(site_info = sites_w_gauges,
                                 scenario_manifest = scenario_manifest,
                                 site_pfts = site_pfts,
                                 pft_manifest = pft_manifest,
                                 scenario = c("ssp245"),
                                 confidence = "medium",
                                 quantiles = c(0.5),
                                 ensemble_variable = "surface_elevation",
                                 outdir = "demo_run/demo_1/"
)

settings_unprepared <-  PEcAn.settings::read.settings("demo_run/demo_1/settings_RCMEM_unprepared.xml")

settings_prepared <- PEcAn.settings::prepare.settings(settings_unprepared)

settings2 <- PEcAn.workflow::runModule.run.write.configs(settings_prepared)
