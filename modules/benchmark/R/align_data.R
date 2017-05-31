##' @name align_data
##' @title Align timeseries data
##' @export
##' @param model.calc data.frame
##' @param obvs.calc data.frame
##' @param var data.frame
##' @param start_year numeric
##' @param end_year numeric
##' @return dat

##' @author Betsy Cowdery


## Align timeseries data using different functions

align_data <- function(model.calc, obvs.calc, var, start_year, end_year, align_method = "match_timestep") {
  
  fcn <- match.fun(align_method)
  
  # Put both timestamps in UTC
  model.calc$posix <- as.POSIXct(with_tz(model.calc$posix, "UTC"))
  obvs.calc$posix  <- as.POSIXct(with_tz(obvs.calc$posix, "UTC"))
  
  diff.m <- diff(model.calc$posix)
  mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
  
  units(diff.m) <- units(diff.o) <- max(units(diff.m),units(diff.o))
  
  compare <- data.frame( 
    type = c("m","o"),
    diff_mode = c(as.numeric(mode.m), as.numeric(mode.o)), 
    diff_units = c(units(mode.m),units(mode.o)), 
    diff_secs = c(as.numeric(mode.m, units = "secs"),as.numeric(mode.o, units = "secs")),
    diff_days = c(as.numeric(mode.m, units = "days"),as.numeric(mode.o, units = "days")),
    diff_time = c(mode.m, mode.o), 
    stringsAsFactors = FALSE
  )
  
  rng_model <- range(model.calc$posix)
  rng_obvs <- range(obvs.calc$posix)
  rng_dat <- sort(c(rng_obvs, rng_model))[c(2, 3)]
  if(setequal(c(365,366), max.diff)){ # Special case for annual timestep
    rng_dat_yr <- year(rng_dat)
    model <- model.calc[year(model.calc$posix) >= rng_dat_yr[1] & 
                          year(model.calc$posix) <= rng_dat_yr[2], ]
    obvs <- obvs.calc[year(obvs.calc$posix) >= rng_dat_yr[1] & 
                        year(obvs.calc$posix) <= rng_dat_yr[2], ]
    model$posix <- year(model$posix)
    obvs$posix <- year(obvs$posix)
  }else{
    model <- model.calc[model.calc$posix >= rng_dat[1] & model.calc$posix <= rng_dat[2], ]
    obvs <- obvs.calc[obvs.calc$posix >= rng_dat[1] & obvs.calc$posix <= rng_dat[2], ]
  }
  
  # Determine the overlaping range of dates
  # Compare the rounded dates because you can't compare dates of different units with range
  rng_obvs  <- range(unique(obvs.calc$round.posix))
  rng_model <- range(unique(model.calc$round.posix))
  rng_dat   <- sort(c(rng_obvs, rng_model))[c(2, 3)] %>% with_tz(., tzone = "UTC")
  
  # Special case for annual timestep
  if(setequal(c(365,366), compare$diff_days[coarse])){
    rng_dat <- year(rng_dat)
    model_sub$round.posix <- year(model$round.posix)
    obvs_sub$round.posix  <- year(obvs$round.posix)
  }
  
  
  # Subset by date range
  date_subsets <- list()
  date_subsets[["m"]] <- model.calc %>% 
    filter(rng_dat[1] <= round.posix)  %>% 
    filter(rng_dat[2] >= round.posix) 
  date_subsets[["o"]] <-  obvs.calc %>% 
    filter(rng_dat[1] <= round.posix)  %>% 
    filter(rng_dat[2] >= round.posix) 
  
  out1 <- date_subsets[[compare$type[coarse]]] %>% dplyr::select(.,one_of(var))
  colnames(out1) <- paste0(colnames(out1), ".", compare$type[coarse])
  
  
  args <- list()
  if (mode.o != mode.m) {
    
    date.coarse <- date_subsets[[compare$type[coarse]]]$round.posix
    date.fine   <- date_subsets[[compare$type[fine]]]$round.posix
    
    data.fine   <- date_subsets[[compare$type[fine]]] %>% dplyr::select(.,one_of(var))
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
    
    out2 <- date_subsets[[compare$type[fine]]] %>% dplyr::select(.,one_of(var))
    colnames(out2) <- paste0(colnames(out2), ".", compare$type[fine])
    dat <- cbind(out1, out2)
    dat$posix <- date_subsets[[compare$type[fine]]] %>% dplyr::select(.,one_of("round.posix")) %>% .[,1]
    
  }
  
  return(dat)
} # align_data