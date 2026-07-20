##' Generate a full sea-level rise and tidal inundation scenario
##'
##' @name generateFullTidalScenario
##' @title Function to query NOAA tide gauge information and generate annual flood level scenearios
##' @param station_id
##' @param start_year
##' @param RSLR_RCP
##' @param RSRL_probability
##' @param ssc
##' @param ssc_storm
##' @param floods_to_include
##' @param floods_to_include
##' @param include_lt_tidal_const
##' @param include_flood_anomalies
##'
##' @export
##' @author J. Holmquist
generateFullTidalScenario <- function(station_id=8575512, start_year=2018, end_year=2100, RCP = c("RCP4.5"), percentile=c(0.25,0.5,0.75),
                                      ssc,  ssc_storm,  floods_to_include, include_lt_tidal_const,
                                      include_lt_tidal_const, include_flood_anomalies
                                      ) {

  require(arrow)
  require(tidyverse)
  require(VulnToolkit)
  require(jsonlite)

  # Workflow in creating a sea-level rise scenario

  # Steps
  # noaa_psml_tab <- read_csv("inst/extdata/npsset_data.csv")
  # psmsl_id <- dplyr::filter(noaa_psml_tab, noaa_id == station_id) %>% dplyr::distinct_all()

  # 1. Query long term MSL
  noaa_data <- VulnToolkit::noaa.parameters(stn = station_id)

  # generate link psmsl link
  # psmsl_link <- paste0("https://psmsl.org/data/obtaining/rlr.annual.data/", psmsl_id$psmsl_id[1], ".rlrdata")
  # psmsl_data <-
  msl <- VulnToolkit::noaa(begindate = min(noaa_data$startDate),
                    enddate = max(noaa_data$endDate),
                    station = station_id,
                    interval = "monthly",
                    datum = "NAVD",
                    units = "meters",
                    met=F
                    )

  # annualize
  msl_annual <- msl %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(MSL = mean(MSL),
                     MHW = mean(MHW)) %>%
    dplyr::mutate(MSL = MSL * 100,
                  MHW = MHW * 100
                  )

  # 2. Interpolate missing data
  msl_spline <- loess(MSL ~ Year, data = msl_annual %>% dplyr::filter(complete.cases(.)))

  msl_annual$msl_smoothed <- predict(msl_spline, newdata = msl_annual$Year)

  # 3. Get SLR rate at year of scenario start
  init_msl <- msl_annual$msl_smoothed[msl_annual$Year == start_year] -
    msl_annual$msl_smoothed[msl_annual$Year == start_year-1]

  # 4. Query future SLR
  kopp_2014 <- arrow::read_parquet("inst/extdata/Kopp_2014_projections_long.parquet")

  kopp_filtered <- kopp_2014 %>%
    dplyr::filter(noaa_id == station_id,
                  year == end_year,
                  rcp %in% RCP
                  )

  rcp_list <- list()
  for (i in 1:length(RCP)) {

    kopp_rcp <- kopp_filtered %>%
      dplyr::filter(rcp == RCP[i])

    msl_outputs <- approx(x = kopp_filtered$percentile,
                          y = kopp_filtered$slr_cm,
                          xout = percentile)
    names(msl_outputs) <- c("percentile", "slr_cm")

    rcp_list[[i]] <- kopp_rcp %>%
      dplyr::select(-c(percentile, slr_cm)) %>%
      dplyr::distinct_all() %>%
      merge(msl_outputs)

  }

  rcp_table <- bind_rows(rcp_list)

  # 5. Query tidal constituents

  url <- paste0(
      "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations/",
      station_id,
      "/harcon.json?units=metric"
    )

  harcon <- jsonlite::fromJSON(url)

  constituents <- harcon$HarmonicConstituents

  M2 <- constituents$amplitude[constituents$name == "M2"]
  K1 <- constituents$amplitude[constituents$name == "K1"]
  O1 <- constituents$amplitude[constituents$name == "O1"]
  S2 <- constituents$amplitude[constituents$name == "S2"]

  F_factor <- (K1 + O1) / (M2 + S2)
  # F = (K1 + 01) / (M2 + S2)

  # 6. Approximate datums

  # MHW = M2
  mhwDatum = M2

  # MHHW = M2 + K1 + O1
  mhhwDatum = M2 + K1 + O1

  # MHHWS =
  mhhwsDatum = M2 + K1 + O1 + S2

  # HAT
  # M2, K1, O1, S2,
  # N2, K2, P1, Q1


  if (F_factor > 3) {

    mhwVect <- t(c(mhhwDatum, mhhwsDatum))
    mlwVect <- -mhwVect

    flood_freq <- c(353-24.8, 24.8)
    flood_time <- c(12.42,12.42)
  } else {

    mhwVect <- t(c(mhwDatum, mhhwDatum, mhhwsDatum))
    mlwVect <- -mhwVect

    flood_freq <- c(353, 353-24.8, 24.8)
    flood_time <- c(6.21,6.21)
  }

  # 7. Query anomalous flood events?

  # !!! Leave this blank for now. There is room to grow

  # Output a vector of mean sea-level

  # A vector of calendar years

  # A matrix with flood heights

  # A matrix with ebb heights

  # A vector of flood frequency (n per year)

  # A vector of event times (hours)

  # A matrix of SSC's (future)

}



