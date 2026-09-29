#' Generate sea-level rise and tidal inundation scenarios
#'
#' Queries one or more NOAA tide gauges and generates annual sea-level and
#' tidal inundation scenarios for each gauge.
#'
#' @param station_id Vector of NOAA station identifiers.
#' @param run_hindcast Logical. Generate historical sea-level data.
#' @param run_forecast Logical. Generate forecast sea-level scenarios.
#' @param hindcast_start First year of hindcast.
#' @param forecast_start First year of forecast.
#' @param forecast_end Final forecast year.
#' @param scenario Character vector of AR6 scenarios.
#' @param confidence_level Character vector of confidence levels.
#' @param target_quantile Numeric vector of target quantiles.
#' @param include_lt_tidal_const Include long-term tidal constituents.
#' @param datum_start_year First year used to calculate tidal datums.
#' @param datum_end_year Last year used to calculate tidal datums.
#'
#' @return A list containing:
#' \itemize{
#'   \item scenario_curves: Combined scenario table for all gauges.
#'   \item tidal_datums_summarized: Combined tidal datum table for all gauges.
#' }
#'
#' @export
generateFullTidalScenario <- function(
    station_id = 9410660,
    run_hindcast = TRUE,
    run_forecast = TRUE,
    hindcast_start = 1928,
    forecast_start = 2026,
    forecast_end = 2100,
    scenario = c("ssp126", "ssp245"),
    confidence_level = "medium",
    target_quantile = c(0.25, 0.5, 0.75),
    include_lt_tidal_const = TRUE,
    datum_start_year = 1980,
    datum_end_year = 2025
) {
  
  require(arrow)
  require(tidyverse)
  require(VulnToolkit)
  # require(jsonlite)
  
  gauge_path <- system.file("extdata",
                            "noaa_us_gauge_info.csv",
                            package = "data.water")
  
  # 4. Query future SLR
  gauge_tab <- readr::read_csv(gauge_path, show_col_types = FALSE)
  
  # [1] "ssp119"         "ssp126"         "ssp245"         "ssp370"         "ssp585"        
  # [6] "tlim1.5win0.25" "tlim2.0win0.25" "tlim3.0win0.25" "tlim4.0win0.25" "tlim5.0win0.25"
  if (! any(scenario %in% c("ssp119","ssp126", "ssp245", "ssp370", "ssp585",
                            "tlim1.5win0.25", "tlim2.0win0.25", "tlim3.0win0.25", "tlim4.0win0.25", "tlim5.0win0.25"))) { stop(paste0("One or more scenarios is not a recognized AR6 scenario. Choose from the following: ", 
                                                                                                                                      paste(c("ssp119","ssp126", "ssp245", "ssp370", "ssp585", "tlim1.5win0.25", "tlim2.0win0.25", "tlim3.0win0.25", "tlim4.0win0.25", "tlim5.0win0.25"), collapse =", "),
                                                                                                                                      "."))
  }
  
  if (any(! confidence_level %in% c("low", "medium"))) {stop("Confidence can only be low or medium.")}
  
  if (any(target_quantile<0) | any(target_quantile>1)) {stop("Quantiles are misspeccified. Must be between 0 and 1.")}
  
  # or one start date for each gauge
  if (length(hindcast_start) == 1) {
    hindcast_start <- rep(hindcast_start, length(station_id))
  }
  
  if (length(hindcast_start) != length(station_id)) {
    stop(
      "`hindcast_start` must have length 1 or the same length as `station_id`."
    )
  }
  
  .generateFullTidalScenario_one_gauge <- function(station_id=9410660,
                                                  run_hindcast = T,
                                                  run_forecast = T,
                                                  hindcast_start = 1928,
                                                  forecast_start = 2026,
                                                  forecast_end = 2100,
                                                  scenario = c("ssp126", "ssp245"),
                                                  confidence_level = "medium",
                                                  target_quantile =c(0.25,0.5,0.75),
                                                  include_lt_tidal_const = T,
                                                  datum_start_year = 1980,
                                                  datum_end_year = 2025
  ) {
    
    # First, do we create a hindcast?
    # if (run_hindcast) {
    #
    # }
    
    # Workflow in creating a sea-level rise scenario
    
    # Steps
    # noaa_psml_tab <- read.csv("inst/extdata/npsset_data.csv")
    # psmsl_id <- dplyr::filter(noaa_psml_tab, noaa_id == station_id) %>% dplyr::distinct_all()
    if (! station_id %in% gauge_tab$noaa_id) {stop(paste0(station_id, " is not a recognized long term NOAA gauge."))}
    
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
      dplyr::mutate(meanSeaLevel = MSL * 100) %>% 
      dplyr::select(-MSL) %>%
      dplyr::rename(year=Year)
    
    # 2. Interpolate missing data
    # ??
    # msl_spline <- loess(MSL ~ Year, data = msl_hindcast %>% dplyr::filter(complete.cases(.)))
    # msl_hindcast$meanSeaLevel <- predict(msl_spline, newdata = msl_hindcast$Year)
    
    # msl_hindcast <- msl_hindcast %>%
    #   dplyr::select(-MSL) %>%
    #   mutate(index = 1:n()-1) %>%
    #   rename(year=Year)
    
    # 3. Get SLR rate at year of scenario start
    
    # If forecast == T
    if (run_forecast) {
      
      # init_slr <- msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start] -
      #   msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start-1]
      
      path <- system.file("extdata",
                          "ar6_us_compiled_senarios.parquet",
                          package = "data.water")
      
      # 4. Query future SLR
      ar6 <- arrow::read_parquet(path)
      
      ar6_filtered <- ar6 %>%
        dplyr::filter(noaa_id == station_id,
                      year <= forecast_end,
                      scenario_name %in% scenario,
                      confidence %in% confidence_level
        )
      
      init_msl <- mean(msl_hindcast$meanSeaLevel[msl_hindcast$year %in% 1995:2014], na.rm = T)
      
      scp_table <- ar6_filtered %>% 
        group_by(noaa_id, noaa_name, year, scenario_name, confidence) %>% 
        reframe(
          approx(
            x = quantile,
            y = sea_level_change_mm,
            xout = target_quantile
          ) %>%
            as_tibble()
        ) %>% 
        mutate(y=y/10+init_msl) %>% 
        rename(quantile=x,
               meanSeaLevel=y) %>% 
        filter(year>max(msl_hindcast$year))
      
      # !!! Add a stop or warning for forecast starts greater than 2000
      
      # Forecast start - 2000
      # slr_2010toStart <- msl_hindcast$meanSeaLevel[msl_hindcast$year == forecast_start] -
      #   msl_hindcast$meanSeaLevel[msl_hindcast$year == 2010]
      
      all_scenario_names <- unique(scp_table$scenario_name)
      
      historical_by_scenario <- crossing(
        msl_hindcast,
        scenario_name = scenario,
        confidence = confidence_level,
        quantile = target_quantile
      )
      
      dat <- bind_rows(
        historical_by_scenario,
        scp_table
      )
      
      
    } else if (run_hindcast) {
      
      # Else if add the hindcast to a list
      dat <- msl_hindcast %>% 
        mutate(scenario_name = "hindcast", confidence= "not applicable", quantile= 0.5)
      
    } else {
      # Else stop
      stop("Must specify either a hindcast, a forecast, or both.")
      
    } # end of run_forecast, run hindcast checks
    
    dat_spline <- dat %>%
      group_by(scenario_name, confidence, quantile) %>%
      arrange(year) %>%
      reframe({
        
        years_out <- seq(
          min(year),
          max(year),
          by = 1
        )
        
        sp <- spline(
          x = year,
          y = meanSeaLevel,
          xout = years_out
        )
        
        tibble(
          year = sp$x,
          meanSeaLevel = sp$y
        )
      })
    
    # ggplot(dat_spline, aes(x = year, y = meanSeaLevel)) +
    #   geom_line(aes(group=quantile)) +
    #   facet_wrap(.~scenario_name) +
    #   geom_point(data=dat)
    
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
                         length(unique(dat_spline$year))),
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
      
      all_years <- unique(dat_spline$year)
      
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
    
    
    amp_df <- as.data.frame(ampMat)
    names(amp_df) <- datumNames
    
    amp_df <- amp_df %>% 
      mutate(year = all_years)
    
    scenario_curves <- dat_spline %>% 
      left_join(amp_df, by = "year") %>% 
      mutate(
        across(all_of(datumNames), ~ .x + meanSeaLevel)
      )
    
    # Output a list of mean sea-levels
    # A vector of calendar years
    # A list of matrices with flood heights
    # A list of matrices with ebb heights
    # A vector of flood frequency (n per year)
    # A vector of event times (hours)
    # A table with sea level rise scenario info
    
    scenario_curves <- scenario_curves %>%
      dplyr::mutate(
        gauge_id = station_id,
        .before = 1
      )
    
    tidal_datums_summarized <- tidal_datums_summarized %>%
      dplyr::mutate(
        gauge_id = station_id,
        .before = 1
      )
    
    output_list <- list(
      scenario_curves = scenario_curves,
      tidal_datums_summarized = tidal_datums_summarized
    )
    
    return(output_list)
  }
  
  
  # Ensure station IDs are unique
  station_id <- unique(station_id)
  
  gauge_runs <- tibble::tibble(
    station_id = station_id,
    hindcast_start = hindcast_start
  )
  
  results <- purrr::map2(
    gauge_runs$station_id,
    gauge_runs$hindcast_start,
    function(gauge, gauge_hindcast_start) {
      
      message(
        "Generating tidal scenarios for gauge: ",
        gauge,
        " (hindcast start: ",
        gauge_hindcast_start,
        ")"
      )
      
      .generateFullTidalScenario_one_gauge(
        station_id = gauge,
        run_hindcast = run_hindcast,
        run_forecast = run_forecast,
        hindcast_start = gauge_hindcast_start,
        forecast_start = forecast_start,
        forecast_end = forecast_end,
        scenario = scenario,
        confidence_level = confidence_level,
        target_quantile = target_quantile,
        include_lt_tidal_const = include_lt_tidal_const,
        datum_start_year = datum_start_year,
        datum_end_year = datum_end_year
      )
    }
  )
  
  names(results) <- as.character(station_id)
  
  # Combine corresponding outputs across gauges
  scenario_curves <- results %>%
    purrr::map("scenario_curves") %>%
    dplyr::bind_rows()
  
  tidal_datums_summarized <- results %>%
    purrr::map("tidal_datums_summarized") %>%
    dplyr::bind_rows()
  
  list(
    scenario_curves = scenario_curves,
    tidal_datums_summarized = tidal_datums_summarized
  )
}
