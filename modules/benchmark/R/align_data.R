##' Align timeseries data
##'
##' @param model.calc data.frame
##' @param obvs.calc data.frame
##' @param var data.frame
##' @param align_method name of function to use for alignment
##'
##' @importFrom rlang .data
##' @return dat
##' @author Betsy Cowdery
##' @export

## Align timeseries data using different functions

align_data <- function(model.calc, obvs.calc, var, align_method = "match_timestep") {
  
  fcn <- match.fun(align_method)
  
  # Put both timestamps in UTC
  model.calc$posix <- as.POSIXct(lubridate::with_tz(model.calc$posix, "UTC"))
  obvs.calc$posix  <- as.POSIXct(lubridate::with_tz(obvs.calc$posix, "UTC"))
  
  diff.m <- diff(model.calc$posix)
  mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
  
  diff.o <- diff(obvs.calc$posix)
  mode.o <- diff.o[which.max(tabulate(match(unique(diff.o), diff.o)))]
  
  compare <- data.frame( 
    type = c("m","o"),
    diff_mode = c(as.numeric(mode.m), as.numeric(mode.o)), 
    diff_units = c(units(mode.m),units(mode.o)), 
    diff_secs = c(as.numeric(mode.m, units = "secs"),as.numeric(mode.o, units = "secs")),
    diff_days = c(as.numeric(mode.m, units = "days"),as.numeric(mode.o, units = "days")),
    diff_time = c(mode.m, mode.o), 
    stringsAsFactors = FALSE
  )
  
  # Determine if time step units are different, if so which is the coarser
  # This will just be redundant if they are the same
  
  coarse <- which.max(compare$diff_secs)
  fine   <- 2 %/% coarse
  coarse.unit <- compare$diff_units[coarse]
  
  # Round to the larger time step (experimental)
  # Note: Oddly, the second argument to `round()` has to be unnamed here
  #   because of an inconsistency in base R's rounding methods.
  #   The generic `round()` expects the second arg to be called `digits`,
  #   but then dispatches to `round.POSIXt`, which takes `units`.
  obvs.calc$round.posix  <- as.POSIXct(round(obvs.calc$posix,  coarse.unit))
  model.calc$round.posix <- as.POSIXct(round(model.calc$posix, coarse.unit))
  
  
  # Determine the overlaping range of dates
  # Compare the rounded dates because you can't compare dates of different units with range
  rng_obvs  <- range(unique(obvs.calc$round.posix))
  rng_model <- range(unique(model.calc$round.posix))
  rng_dat   <- sort(c(rng_obvs, rng_model))[c(2, 3)] %>% lubridate::with_tz(tzone = "UTC")
  
  # Special case for annual timestep
  if(setequal(c(365,366), compare$diff_days[coarse]) | setequal(c(365), compare$diff_days[coarse]) | 
     setequal(c(366), compare$diff_days[coarse])){
    rng_dat <- lubridate::year(rng_dat)
    model.calc$round.posix <- lubridate::year(model.calc$round.posix)
    obvs.calc$round.posix  <- lubridate::year(obvs.calc$round.posix)
  }
  
  
  # Subset by date range
  date_subsets <- list()
  date_subsets[["m"]] <- model.calc %>% 
    filter(rng_dat[1] <= .data$round.posix)  %>% 
    filter(rng_dat[2] >= .data$round.posix) 
  date_subsets[["o"]] <-  obvs.calc %>% 
    filter(rng_dat[1] <= .data$round.posix)  %>% 
    filter(rng_dat[2] >= .data$round.posix) 
  
  # Additional date range check: the date range of the fine data must be inside
  # that of the coarse data or the aggregation functions will add an extra day
  coarse_range_check <- range(date_subsets[[compare$type[coarse]]]$round.posix)

  date_subsets[[compare$type[fine]]] <- date_subsets[[compare$type[fine]]] %>% 
    filter(coarse_range_check[1] <= .data$round.posix)  %>% 
    filter(coarse_range_check[2] >= .data$round.posix)
  
  out1 <- date_subsets[[compare$type[coarse]]] %>% dplyr::select(dplyr::one_of(var))
  colnames(out1) <- paste0(colnames(out1), ".", compare$type[coarse])
  
  
  args <- list()
  if (mode.o != mode.m) {
    
    date.coarse <- date_subsets[[compare$type[coarse]]]$round.posix
    date.fine   <- date_subsets[[compare$type[fine]]]$round.posix
    
    data.fine   <- date_subsets[[compare$type[fine]]] %>% dplyr::select(dplyr::one_of(var))
    colnames(data.fine) <- paste0(colnames(data.fine), ".", compare$type[fine])
    
    out2 <- apply(data.fine, 2, 
                  function(x){
                    args$date.coarse = date.coarse
                    args$date.fine = date.fine
                    args$data.fine = x
                    do.call(fcn, args)
                  })
    dat <- cbind(out1, out2)
    dat$posix <- date.coarse
    
  } else if (mode.o == mode.m) { # here coarse and fine are just index values but but the time steps are the same size
    
    out2 <- date_subsets[[compare$type[fine]]] %>% dplyr::select(dplyr::one_of(var))
    colnames(out2) <- paste0(colnames(out2), ".", compare$type[fine])
    dat <- cbind(out1, out2)
    dat$posix <- date_subsets[[compare$type[fine]]] %>% dplyr::select(dplyr::one_of("round.posix")) %>% .[,1]
    
  }
  
  return(dat)
} # align_data