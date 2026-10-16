
# Test

# Create Site Table
sites <- data.frame(
  id = c("Tijuana", "Seal Beach", "Triangle"), 
  latitude = c(32.57427143, 33.73520729, 37.45842503),	
  longitude = c(-117.1287429, -118.0792275, -121.9773209)
)

sites_w_gauges <- findNearestNoaaGauge(sites)
(sites_w_gauges)

# Create scenario
scenario_output <- data.water::generateFullTidalScenario(
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

met2model.RCMEM(scenario_output, outfolder = "demo_run/data/met")

scenario_manifest <- read_csv("demo_run/data/met/scenario_manifest.csv")

buildSettings.RCMEM(site_info = sites_w_gauges,
                    scenario_manifest = scenario_manifest,
                    scenario = c("ssp126", "ssp245", "ssp370"),
                    confidence = "medium",
                    quantiles = c(0.25, 0.5, 0.75),
                    outdir = "demo_run/"
                    )
