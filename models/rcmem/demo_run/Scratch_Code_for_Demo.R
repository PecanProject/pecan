# Test
library(tidyverse)

# Create Site Table
sites <- data.frame(
  id = c("Tijuana", "SealBeach", "Triangle"), 
  latitude = c(32.57427143, 33.73520729, 37.45842503),	
  longitude = c(-117.1287429, -118.0792275, -121.9773209)
)

sites_w_gauges <- PEcAn.data.water::findNearestNoaaGauge(sites)
(sites_w_gauges)

# Create scenario
scenario_output <- PEcAn.data.water::generateFullTidalScenario(
  station_id = sites_w_gauges$gauge_id,
  run_hindcast = T,
  run_forecast = T,
  hindcast_start = sites_w_gauges$startDate,
  forecast_start = 2024,
  forecast_end = 2100,
  scenario = c("ssp126", "ssp245", "ssp370"),
  confidence_level = "medium",
  target_quantile = c(0.25, 0.5, 0.75),
  include_lt_tidal_const = T,
  datum_start_year = 1980,
  datum_end_year = 2025)


PEcAn.RCMEM::met2model.RCMEM(scenario_output, 
                             outfolder = "demo_run/data/met",
                             overwrite = T)

scenario_manifest <- read_csv("demo_run/data/met/scenario_manifest.csv")

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


PEcAn.RCMEM::buildSettings.RCMEM(site_info = sites_w_gauges,
                    scenario_manifest = scenario_manifest,
                    scenario = c("ssp126", "ssp245", "ssp370"),
                    confidence = "medium",
                    quantiles = c(0.25, 0.5, 0.75),
                    outdir = "demo_run/demo_2"
                    )

settings_unprepared <-  PEcAn.settings::read.settings("demo_run/settings_RCMEM_unprepared.xml")

settings_prepared <- PEcAn.settings::prepare.settings(settings_unprepared)


