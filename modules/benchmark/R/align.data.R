##' @name align.data
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

align.data <- function(model.calc, obvs.calc, var, start_year, end_year, align_method = "match.timestep") {
  
  fcn <- match.fun(align_method)
  
  diff.m <- diff(model.calc$posix)
  diff.o <- diff(obvs.calc$posix)
  
  units(diff.m) <- units(diff.o) <- max(units(diff.m),units(diff.o))
  
  mode.m <- as.numeric(diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))])
  mode.o <- as.numeric(diff.o[which.max(tabulate(match(unique(diff.o), diff.o)))])
  max.diff <- if(mode.m > mode.o) diff.m else diff.o
  
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

  
  if (mode.m > mode.o) {
    date.coarse <- model$posix
    date.fine <- obvs$posix
    data.fine <- obvs[, var, drop = FALSE]
    colnames(data.fine) <- paste0(colnames(data.fine), ".o")
    out1 <- model[, var, drop = FALSE]
    colnames(out1) <- paste0(colnames(out1), ".m")
  } else if (mode.o > mode.m) {
    date.coarse <- obvs$posix
    date.fine <- model$posix
    data.fine <- model[, var, drop = FALSE]
    colnames(data.fine) <- paste0(colnames(data.fine), ".m")
    out1 <- obvs[, var, drop = FALSE]
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
    out1 <- model[, var, drop = FALSE]
    colnames(out1) <- paste0(var, ".m")
    out2 <- obvs[, var, drop = FALSE]
    colnames(out2) <- paste0(var, ".o")
    dat <- cbind(out1, out2)
    dat$posix <- model$posix
  }
  
  return(dat)
} # align.data

# # Compare timestep sizes,
# # choose the smaller of the two
# # then choose the appropriate conversion function
# 
# which.min(c(mode_model,mode_obvs))
# 
# date.coarse <- obvs$time
# date.fine <- strptime(paste(model$time, model$year), format = "%j %Y")
# data.fine <- model$NPP
# mean.over.larger.timestep(date.coarse, date.fine, data.fine)
# 
# ###################################################
# 
# # Then big theoretical leap to get me here O.o
# 
# out_model <- as.data.frame(matrix(NA, ncol = length(vars_used$pecan_name), nrow = length(unique(model$year))))
# colnames(out_model) <- paste(vars_used$pecan_name, "model", sep = "_")
# out_model$years <- sort(unique(model$year))
# for(i in 1:nrow(vars_used)){
#   v <- vars_used$pecan_name[i]
#   out_model[,paste(v,"model",sep="_")] <- aggregate(model[,v], by=list(model$year), FUN=mean, na.rm=TRUE)[,2]
# }
# 
# colnames(obvs)[which(names(obvs)=="YEAR")] <- "time"
# out_obvs <- as.data.frame(matrix(NA, ncol = length(vars_used$pecan_name), nrow = length(unique(obvs$time))))
# colnames(out_obvs) <- paste(vars_used$pecan_name, "obvs", sep = "_")
# out_obvs$years <- sort(unique(obvs$time))
# for(i in 1:nrow(vars_used)){
#   v <- vars_used$pecan_name[i]
#   print(v)
#   print(paste(v,"obvs",sep="_"))
#   out_obvs[,paste(v,"obvs",sep="_")] <- aggregate(obvs[,v], by=list(obvs$time), FUN=mean, na.rm=TRUE)[,2]
# }
# 
# 
# out <- merge(out_model, out_obvs, by = "years", all = TRUE)
# colnames(out)[which(names(out)=="years")] <- "time"
# out$time_model  <- NULL
# out$time_obvs  <- NULL
