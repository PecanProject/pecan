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
  var<-as.data.frame(var)
  names(var)<-c("model", "obvs")
  
  fcn <- match.fun(align_method)
  
  diff.m <- diff(model.calc$posix)
  diff.o <- diff(obvs.calc$posix)
  
  if(units(diff.m) != units(diff.o)){
    units(diff.m) <- units(diff.o) <- "secs" # For now just convert to the smallest units possible which is seconds
  }

  mode.m <- as.numeric(diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))])
  mode.o <- as.numeric(diff.o[which.max(tabulate(match(unique(diff.o), diff.o)))])
  max.diff <- if(mode.m > mode.o) diff.m else diff.o 
  
  rng_model <- range(model.calc$posix)
  rng_obvs <- range(obvs.calc$posix)
  rng_dat <- sort(c(rng_obvs, rng_model))[c(2, 3)]
  max.diff.day <- max.diff; units(max.diff.day) <- "days"
  if(setequal(c(365,366), max.diff.day)){ # Special case for annual timestep
    rng_dat_yr <- year(rng_dat)
    model_sub <- model.calc[year(model.calc$posix) >= rng_dat_yr[1] & 
                          year(model.calc$posix) <= rng_dat_yr[2], ]
    obvs_sub <- obvs.calc[year(obvs.calc$posix) >= rng_dat_yr[1] & 
                        year(obvs.calc$posix) <= rng_dat_yr[2], ]
    model_sub$posix <- year(model$posix)
    obvs_sub$posix <- year(obvs$posix)
  }else{
    model_sub <- model.calc[model.calc$posix >= rng_dat[1] & model.calc$posix <= rng_dat[2], ]
    obvs_sub <- obvs.calc[obvs.calc$posix >= rng_dat[1] & obvs.calc$posix <= rng_dat[2], ] #This has NEE data, not all NA's
  }

  
  if (mode.m > mode.o) {
    date.coarse <- model_sub$posix
    date.fine <- obvs_sub$posix
    data.fine <- obvs_sub[, as.character(var$obvs), drop = FALSE]
    colnames(data.fine) <- paste0(colnames(data.fine), ".o")
    out1 <- model_sub[, as.character(var$model), drop = FALSE]
    colnames(out1) <- paste0(colnames(out1), ".m")
  } else if (mode.o > mode.m) {
    date.coarse <- obvs_sub$posix
    date.fine <- model_sub$posix
    data.fine <- model_sub[, as.character(var$model), drop = FALSE]
    colnames(data.fine) <- paste0(colnames(data.fine), ".m")
    out1 <- obvs_sub[, as.character(var$obvs), drop = FALSE]
    colnames(out1) <- paste0(colnames(out1), ".o")
  }
  
  args <- list()
  if (mode.o != mode.m) {
    # There will be other functions eventually
    out2 <- apply(data.fine, 2, 
                  function(x){
                    args$date.coarse = date.coarse
                    args$date.fine = date.fine
                    args$data.fine = x
                    do.call(fcn, args)
                  })
    dat <- cbind(out1, out2)
    dat$posix <- date.coarse
  } else if (mode.o == mode.m) {
    out1 <- model_sub[, as.character(var$model), drop = FALSE]
    colnames(out1) <- paste0(as.character(var$model), ".m")
    out2 <- obvs_sub[, as.character(var$obvs), drop = FALSE] #This also has NEE data
    colnames(out2) <- paste0(as.character(var$obvs), ".o")
    if (dim(out1)[1]!=dim(out2)[1]){
      out1<-out1[-1,]
      out1<-as.data.frame(out1)
      colnames(out1) <- paste0(as.character(var$model), ".m")
    }
    
    dat <- cbind(out1, out2) #This is where my "imply differing number of rows error comes form" 
    if(dim(out1)[1]!=length(model_sub$posix)){
      dat$posix<-model_sub$posix[-1]
    }else{
      dat$posix <- model_sub$posix
    }
    
  }
  
  return(dat)
} # align_data