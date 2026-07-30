##' Generate a full sea-level rise and tidal inundation scenario
##'
##' @name generateFullTidalScenario
##' @title Function to query NOAA tide gauge information and generate annual flood level scenearios
##' @param station_id
##' @param  run_hindcast
##' @param run_forecast
##' @param hindcast_start
##' @param forecast_start
##' @param forecast_end
##' @param RCP
##' @param RCP_probability
##' @param ssc
##' @param ssc_storm
##' @param floods_to_include
##' @param floods_to_include
##' @param include_lt_tidal_const
##' @param include_flood_anomalies
##'
##' @export
##' @author J. Holmquist
generateFullTidalScenario <- function(station_id=8575512,
                                      run_hindcast = T,
                                      run_forecast = T,
                                      hindcast_start = 1928,
                                      forecast_start = 2018,
                                      forecast_end = 2100,
                                      RCP = c("RCP4.5"),
                                      RCP_probability=c(0.25,0.5,0.75),
                                      ssc,
                                      ssc_storm,
                                      floods_to_include,
                                      include_lt_tidal_const,
                                      include_lt_tidal_const,
                                      include_flood_anomalies
                                      ) {

  require(arrow)
  require(tidyverse)
  require(VulnToolkit)
  # require(jsonlite)

  # forecast_end needs to be one of the following
  # 2030
  # 2050
  # 2100
  # 2150
  # 2200
  # First, do we create a hindcast?
  # if (run_hindcast) {
  #
  # }

  # Workflow in creating a sea-level rise scenario

  # Steps
  # noaa_psml_tab <- read_csv("inst/extdata/npsset_data.csv")
  # psmsl_id <- dplyr::filter(noaa_psml_tab, noaa_id == station_id) %>% dplyr::distinct_all()

  # 1. Query long term MSL

  # We have to do this anyway
  noaa_data <- VulnToolkit::noaa.parameters(stn = station_id) %>%
    mutate(startDate = format(lubridate::ymd_hm(startDate), format = "%Y%m%d"),
           endDate = format(lubridate::ymd_hm(endDate), format = "%Y%m%d")
    )

  mtl <- VulnToolkit::noaa(begindate = max(min(noaa_data$startDate),
                                           paste0(hindcast_start, "0101"),
                                           na.rm = T
                                           ),
                    enddate = min(max(noaa_data$endDate),
                                  paste0(forecast_start, "1231"),
                                  na.rm = T
                                  ),
                    station = station_id,
                    interval = "monthly",
                    datum = "NAVD",
                    units = "meters",
                    met=F
                    )

  # annualize
  mtl_hindcast <- mtl %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(MTL = mean(MTL)) %>%
    dplyr::mutate(MTL = MTL * 100)

  # 2. Interpolate missing data
  mtl_spline <- loess(MTL ~ Year, data = mtl_hindcast %>% dplyr::filter(complete.cases(.)))

  mtl_hindcast$meanTidalLevel <- predict(mtl_spline, newdata = mtl_hindcast$Year)

  mtl_hindcast <- mtl_hindcast %>%
    dplyr::select(-MTL) %>%
    mutate(index = 1:n()-1) %>%
    rename(year=Year)

  # 3. Get SLR rate at year of scenario start

  # If forecast == T
  if (run_forecast) {

    # Else if add the hindcast to a list

    init_slr <- mtl_annual$mtl_smoothed[mtl_annual$Year == forecast_start] -
      mtl_annual$mtl_smoothed[mtl_annual$Year == forecast_start-1]

    # 4. Query future SLR
    kopp_2014 <- arrow::read_parquet("inst/extdata/Kopp_2014_projections_long.parquet")

    kopp_filtered <- kopp_2014 %>%
      dplyr::filter(noaa_id == station_id,
                    year == forecast_end,
                    rcp %in% RCP
      )

    rcp_list <- list()
    for (i in 1:length(RCP)) {

      kopp_rcp <- kopp_filtered %>%
        dplyr::filter(rcp == RCP[i])

      msl_outputs <- approx(x = kopp_filtered$percentile/100,
                            y = kopp_filtered$slr_cm,
                            xout = RCP_probability)
      names(msl_outputs) <- c("probabiliy", "slr_cm")

      scenario_list[[i]] <- kopp_rcp %>%
        dplyr::select(-c(percentile, slr_cm)) %>%
        dplyr::distinct_all() %>%
        merge(msl_outputs)

    }

    rcp_table <- bind_rows(rcp_list)

    # !!! Add a stop or warning for forecast starts greater than 2000

    # Forecast start - 2000
    slr_2000toStart <- mtl_hindcast$meanTidalLevel[mtl_hindcast$year == forecast_start] -
      mtl_hindcast$meanTidalLevel[mtl_hindcast$year == 2000]

    init_mtl <- rev(mtl_hindcast$meanTidalLevel)[1]

    # Create an initial sea-level rise curve
    scenario_curve_list <- list()

    for (i in 1:nrow(rcp_table)) {

      temp_curve <- buildScenarioCurve(startYear = forecast_start,
                                       endYear = forecast_end,
                                       meanTidalLevel = init_mtl,
                                       relSeaLevelRiseInit = init_slr,
                                       relSeaLevelRiseTotal = rcp_table$slr_cm[i]-slr_2000toStart
      )

      if (run_hindcast) {

        temp_curve <- temp_curve[-1,] %>%
          mutate(index = index+max(mtl_hindcast$index))

        temp_curve <- bind_rows(mtl_hindcast,
                                temp_curve
                                )

      }

      scenario_curve_list[[i]] <- temp_curve

    }


  } else if (run_hindcast) {

    scenario_curve_list <- list(mtl_hindcast)

  } else {
    # Else stop
    stop("Must specify either a hindcast, a forecast, or both.")

  } # end of run_forecast, run hindcast checks

  # 5. Query tidal constituents
  constituents <- VulnToolkit::harcon(station_id)

  M2 <- constituents$hc.amp[constituents$hc.name == "M2"]
  K1 <- constituents$hc.amp[constituents$hc.name == "K1"]
  O1 <- constituents$hc.amp[constituents$hc.name == "O1"]
  S2 <- constituents$hc.amp[constituents$hc.name == "S2"]

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

    datumNames <- c("MHHW", "MHHWS")
    ampVect <- matrix(c(c(mhhwDatum, mhhwsDatum),
                 rep = ),
                 nrow = 2)

    flood_freq <- c(353-24.8, 24.8)
    flood_time <- c(12.42,12.42)

  } else {
    datumNames <- c("MLHW", "MHHW", "MHHWS")
    mhwVect <- t(c(mhwDatum, mhhwDatum, mhhwsDatum))
    mlwVect <- -mhwVect

    flood_freq <- c(353, 353-24.8, 24.8)
    flood_time <- c(6.21,6.21,6.21)
  }

  # 7. Query anomalous flood events?

  # !!! Leave this blank for now. There is room to grow

  # 8. Long term nodal cycles for

  # Output a vector of mean sea-level

  # A vector of calendar years

  # A matrix with flood heights

  # A matrix with ebb heights

  # A vector of flood frequency (n per year)

  # A vector of event times (hours)

  # A matrix of SSC's (future)

}



