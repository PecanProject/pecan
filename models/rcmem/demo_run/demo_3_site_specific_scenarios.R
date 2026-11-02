library(tidyverse)
library(PEcAn.RCMEM)
library(PEcAn.data.water)

# Create Site Table
sites <- data.frame(
  id = c("Tijuana"), 
  latitude = c( 32.57427143),	
  longitude = c(-117.1287429)
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
                             outfolder = "demo_run/demo_3/data/met",
                             overwrite = T)

scenario_manifest <- read_csv("demo_run/demo_3/data/met/scenario_manifest.csv")

# Vegetation parameters
salicornia.distns <- data.frame(
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

# Vegetation parameters
spartina.distns <- data.frame(
  distn = c("gamma", "norm", "norm",
            "norm", "gamma", "gamma",
            "unif", "gamma", "unif"
  ),
  parama = c(74.08, -2.14, 0.84,
             0.48, 192.1, 34.2,
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
            "norm"),
  parama = c(39.7, 6.6, 80.0,
             0.001, 9.6, 1.7,
             -13.9
  ),
  paramb = c(399.6, 4.9, 4.3,
             0.001, 13.8, 0.4,
             2.6
  ),
  n = NA_integer_,
  row.names = c(
    "meanOmPackingDensity", "meanMineralPackingDensity", "omDecayRateFast",
    "omDecayRateSlow", "recalcitrantFrac", "captureRate",
    "initElv"
  )
)

# Vegetation PFT
dir.create(
  "demo_run/demo_3/data/pfts/vegetation/salicornia/",
  recursive = TRUE,
  showWarnings = FALSE
)

post.distns <- salicornia.distns
save(
  veg.distns,
  file = "demo_run/demo_3/data/pfts/vegetation/salicornia/post.distns.RData"
)

dir.create(
  "demo_run/demo_3/data/pfts/vegetation/spartina/",
  recursive = TRUE,
  showWarnings = FALSE
)

post.distns <- spartina.distns
save(
  veg.distns,
  file = "demo_run/demo_3/data/pfts/vegetation/spartina/post.distns.RData"
)

post.distns <- soil.distns
# Soil PFT
dir.create(
  "demo_run/demo_3/data/pfts/soil",
  recursive = TRUE,
  showWarnings = FALSE
)

save(
  post.distns,
  file = "demo_run/demo_3/data/pfts/soil/soil.RData"
)

site_pfts <- data.frame( id = c("Tijuana", "Tijuana", "Tijuana"),
                         pft_type = c("soil", "vegetation", "vegetation"),
                         pft_name = c("soil", "salicornia", "spartina")
)

pft_manifest <- data.frame( pft_name = c("soil", "salicornia", "spartina"),
                            pft_type = c("soil", "vegetation", "vegetation"),
                           posterior_file = c("demo_run/demo_3/data/pfts/soil/soil.RData",
                                              "demo_run/demo_3/data/pfts/vegetation/salicornia/post.distns.RData",
                                              "demo_run/demo_3/data/pfts/vegetation/spartina/post.distns.RData")
)

site_met <- list(
  year = 1906:2100,
  
  suspendedSediment = rep(7e-6, 195),
  
  species_presence = matrix(
    1,
    nrow = 195,
    ncol = 2,
    dimnames = list(
      as.character(1906:2100),
      c("spartina", "salicornia")
    )
  )
)

dir.create(
  "demo_run/demo_3/data/site_met/",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(site_met, "demo_run/demo_3/data/site_met/tijuana.rds")

# Add site_met_path to site_inf
sites_w_gauges2 <- sites_w_gauges %>% 
  mutate(site_met_path = "demo_run/demo_3/data/site_met/tijuana.rds")

PEcAn.RCMEM::buildSettings.RCMEM(site_info = sites_w_gauges2,
                                 scenario_manifest = scenario_manifest,
                                 site_pfts = site_pfts,
                                 pft_manifest = pft_manifest,
                                 scenario = c("ssp245"),
                                 confidence = "medium",
                                 quantiles = c(0.5),
                                 ensemble_variable = "surface_elevation",
                                 outdir = "demo_run/demo_3/",
                                 ensemble_size = 20)

settings_unprepared <-  PEcAn.settings::read.settings("demo_run/demo_3/settings_RCMEM_unprepared.xml")

settings_prepared <- PEcAn.settings::prepare.settings(settings_unprepared)

