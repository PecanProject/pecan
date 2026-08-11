##' Generate a full sea-level rise and tidal inundation scenario
##'
##' @name generateFullTidalScenario
##' @title Function to query NOAA tide gauge information and generate annual flood level scenearios
##'
##' @param station_id Unique NOAA station identifier
##' @param run_hindcast True or False, run a hindcast
##' @param run_forecast True or False, run a forecast
##' @param hindcast_start Calendar year (YYYY) that hindcast starts
##' @param forecast_start Calendar year (YYYY) that forecast starts
##' @param forecast_end Calendar year (YYYY) that forecast ends, needs to be 2030, 2050, 2100, 2150 or 2200
##' @param RCP Realized concentration pathway, either string or vector, must be RCP2.6, RCP4.5 or RCP8.5
##' @param RCP_probability RCP probability, either numeric or vector, sea-level rise senario probabilities to test.
##' @param include_lt_tidal_const Include long term tidal constituents, True or False.
##' @param datum_start_year Datum start year over which to calculate tidal datums (calendar year, YYYY).
##' @param datum_end_year Datum end year over which to calculate tidal datums (calendar year, YYYY).
##'
##' @export
##' @author J. Holmquist
generateFullTidalScenario <- function(station_id=9410660,
                                      run_hindcast = T,
                                      run_forecast = T,
                                      hindcast_start = 1928,
                                      forecast_start = 2018,
                                      forecast_end = 2100,
                                      RCP = c("RCP4.5"),
                                      RCP_probability=c(0.25,0.5,0.75),
                                      include_lt_tidal_const = T,
                                      datum_start_year = 1980,
                                      datum_end_year = 2025
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
  # noaa_psml_tab <- read.csv("inst/extdata/npsset_data.csv")
  # psmsl_id <- dplyr::filter(noaa_psml_tab, noaa_id == station_id) %>% dplyr::distinct_all()

  # 1. Query long term MSL

  # We have to do this anyway
  noaa_data <- VulnToolkit::noaa.parameters(stn = station_id) %>%
    mutate(startDate = format(lubridate::ymd_hm(startDate), format = "%Y%m%d"),
           endDate = format(lubridate::ymd_hm(endDate), format = "%Y%m%d")
    )

  msl <- VulnToolkit::noaa(begindate = max(min(noaa_data$startDate),
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
  msl_hindcast <- msl %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(MSL = mean(MSL)) %>%
    dplyr::mutate(MSL = MSL * 100)

  # 2. Interpolate missing data
  msl_spline <- loess(MSL ~ Year, data = msl_hindcast %>% dplyr::filter(complete.cases(.)))

  msl_hindcast$meanSeaLevel <- predict(msl_spline, newdata = msl_hindcast$Year)

  msl_hindcast <- msl_hindcast %>%
    dplyr::select(-MSL) %>%
    mutate(index = 1:n()-1) %>%
    rename(year=Year)

  # 3. Get SLR rate at year of scenario start

  # If forecast == T
  if (run_forecast) {

    init_slr <- msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start] -
      msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start-1]

    path <- system.file("extdata",
                        "Kopp_2014_projections_long.parquet",
                        package = "data.water")

    # 4. Query future SLR
    kopp_2014 <- arrow::read_parquet(path)


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

      rcp_list[[i]] <- kopp_rcp %>%
        dplyr::select(-c(percentile, slr_cm)) %>%
        dplyr::distinct_all() %>%
        merge(msl_outputs)

    }

    rcp_table <- bind_rows(rcp_list)

    # !!! Add a stop or warning for forecast starts greater than 2000

    # Forecast start - 2000
    slr_2000toStart <- msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start] -
      msl_hindcast$meanSeaLevel[msl_hindcast$year == 2000]

    init_msl <- rev(msl_hindcast$meanSeaLevel)[1]

    # Create an initial sea-level rise curve
    scenario_curve_list <- list()

    for (i in 1:nrow(rcp_table)) {

      temp_curve <- buildScenarioCurve(startYear = forecast_start,
                                       endYear = forecast_end,
                                       meanSeaLevel = init_msl,
                                       relSeaLevelRiseInit = init_slr,
                                       relSeaLevelRiseTotal = rcp_table$slr_cm[i]-slr_2000toStart
      )

      if (run_hindcast) {

        temp_curve <- temp_curve[-1,] %>%
          mutate(index = index+max(msl_hindcast$index))

        temp_curve <- bind_rows(msl_hindcast,
                                temp_curve
                                )

      }

      scenario_curve_list[[i]] <- temp_curve

    }


  } else if (run_hindcast) {

    # Else if add the hindcast to a list
    scenario_curve_list <- list(msl_hindcast)

  } else {
    # Else stop
    stop("Must specify either a hindcast, a forecast, or both.")

  } # end of run_forecast, run hindcast checks

  # plot(scenario_curve_list[[1]]$year, scenario_curve_list[[1]]$meanSeaLevel, type = "l")

  # for (i in 2:length(scenario_curve_list)) {
  #
  #   lines(scenario_curve_list[[i]]$year, scenario_curve_list[[i]]$meanSeaLevel)
  #
  # }

  # 5. Query tidal constituents

  tidal_datum_path <- system.file("extdata",
                                  "annual_compiled_datums.csv",
                                  package = "data.water")

  tidal_datums <- read.csv(tidal_datum_path) %>%
    dplyr::rename(noaa_id=station_id) %>%
    dplyr::filter(noaa_id == station_id) %>%
    filter(! Datum %in% c("HOT", "LOT"))

  tidal_datums_MSL <- tidal_datums %>%
    filter(Datum %in% c("MSL"),
           n_obs >= 364*24) %>%
    select(Datum, observed, year) %>%
    pivot_wider(names_from = "Datum", values_from = "observed")

  tidal_datums_summarized <- tidal_datums %>%
    filter(Datum != "MSL") %>%
    left_join(tidal_datums_MSL) %>%
    mutate(observed = observed - MSL) %>%
    filter(year >= datum_start_year & year <= datum_end_year) %>%
    group_by(Datum) %>%
    summarise(observed = mean(observed,na.rm=T),
              flood_n = mean(n_pred),
              risingTime = mean(risingTime),
              fallingTime = mean(fallingTime)) %>%
    ungroup() %>%
    mutate(flood_time = (risingTime + abs(fallingTime))/2,
           observed = observed * 100) %>%
    select(Datum, observed, flood_n, flood_time) %>%
    arrange(-observed)

  ampMat <- matrix(rep(tidal_datums_summarized$observed,
                         nrow(scenario_curve_list[[1]])),
                     ncol = length(tidal_datums_summarized$observed),
                     byrow = T)

  datumNames <- tidal_datums_summarized$Datum

  # 7. Query anomalous flood events?

  # !!! Leave this blank for now. There is room to grow

  # 8. Long term nodal cycles for
  if (include_lt_tidal_const) {

    lt_tide_const_path <- system.file("extdata",
                "long_term_tidal_constituents.csv",
                package = "data.water")

    lt_tide_const <- read.csv(lt_tide_const_path)

    lt_tide_const <- lt_tide_const %>%
      dplyr::rename(noaa_id=station_id) %>%
      dplyr::filter(noaa_id == station_id)

    all_years <- scenario_curve_list[[1]]$year

    for (j in 1:length(datumNames)) {

      temp_lt_tide <- lt_tide_const %>%
        dplyr::filter(tide == datumNames[j])

      # Is 4.4 sig?
      is4p4_sig_1 <- temp_lt_tide$amp44>1 | temp_lt_tide$amp44/temp_lt_tide$amp18 > 0.4
      is4p4_sig_2 <- temp_lt_tide$amp18 + temp_lt_tide$amp44 > temp_lt_tide$rse & temp_lt_tide$r2 >= 0.5

      # Is 18.61 sig?
      is18_sig <- temp_lt_tide$amp18b > temp_lt_tide$rseb & temp_lt_tide$r2b >= 0.25

      if (is4p4_sig_1 & is4p4_sig_2) {

        offset <- (temp_lt_tide$amp44 * sin(2*pi*(all_years-temp_lt_tide$phase44)/4.4)) +
          temp_lt_tide$amp18 * sin(2*pi*(all_years-temp_lt_tide$phase18)/18.61)

      } else if (is18_sig) {

        offset <- temp_lt_tide$amp18b * sin(2*pi*(all_years-temp_lt_tide$phase18b)/18.61)

      } else {
        offset <- 0
      }

      ampMat[,j] <- ampMat[,j] + offset*100

    }

  }

  MHW_mat_list <- list()
  MLW_mat_list <- list()
  for (i in 1:length(scenario_curve_list)) {

    ampMatTemp <- ampMat + scenario_curve_list[[i]]$meanSeaLevel

    MHW_mat_list[[i]] <- ampMatTemp[,1:(ncol(ampMatTemp)/2)]
    MLW_mat_list[[i]] <- ampMatTemp[, ((ncol(ampMatTemp)/2)+1):ncol(ampMatTemp)]

  }

  # Output a list of mean sea-levels
  # A vector of calendar years
  # A list of matrices with flood heights
  # A list of matrices with ebb heights
  # A vector of flood frequency (n per year)
  # A vector of event times (hours)
  # A table with sea level rise scenario info

  output_list <- list(rcp_table,
                      scenario_curve_list,
                      MHW_mat_list,
                      MLW_mat_list,
                      n_flood = tidal_datums_summarized$flood_n[1:(length(tidal_datums_summarized$flood_n)/2)],
                      flood_times = tidal_datums_summarized$flood_time[1:(length(tidal_datums_summarized$flood_n)/2)]
                      )
  return(output_list)
}



