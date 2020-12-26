##' Load Dataset for Paramater Data Assimilation
##'
##' @title Load Dataset for Paramater Data Assimilation
##' ##' This function is used to load and preprocess data for PDA. It is expected to be moved / merged 
##' with a more general PEcAn 'load_data' function eventually.
##' 
##' @param settings = PEcAn settings list
##' @param bety bety list object
##' @param external.formats formats list
##'
##' @return A list containg the loaded input data, plus metadata
##'
##' @author Ryan Kelly, Istem Fer
##' @export
load.pda.data <- function(settings, bety, external.formats = NULL) {

  # Outlining setup for multiple datasets

  inputs         <- list()
  input.settings <- settings$assim.batch$inputs
  n.input        <- length(input.settings)
 
  for(i in seq_len(n.input)) {
    inputs[[i]]               <- list()
    
    inputs[[i]]$variable.name <- lapply(input.settings[[i]]$variable.name, PEcAn.utils::convert.expr)
    data.var                  <-  sapply(inputs[[i]]$variable.name, `[[`, "variable.drv")

    data.path                 <- input.settings[[i]]$path

    inputs[[i]]$variable.id   <- input.settings[[i]]$variable.id
    inputs[[i]]$input.id      <- input.settings[[i]]$input.id
    inputs[[i]]$align.method  <- ifelse(!is.null(input.settings[[i]]$align.method), input.settings[[i]]$align.method, "match_timestep")
    
    # I require that the user defines data.path in the settings as well, instead of using query.file.path
    # because 'data.path <- query.file.path(obvs.id, con)' might return an incomplete path 
    # which results in reading all the files in that particular directory in the load_x_netcdf step
    if (is.null(inputs[[i]]$input.id) | is.null(data.path)) {
      PEcAn.logger::logger.error("Must provide both ID and PATH for all data assimilation inputs.")
    }
    
    if(is.null(bety$con)){
      format <- external.formats[[i]]
    }else{
      format <- PEcAn.DB::query.format.vars(bety = bety, input.id = inputs[[i]]$input.id)
    }

    
    vars.used.index <- which(format$vars$bety_name %in% data.var)
    
    inputs[[i]]$data <- PEcAn.benchmark::load_data(data.path = data.path,
                                  format = format, 
                                  start_year = lubridate::year(settings$run$start.date), 
                                  end_year = lubridate::year(settings$run$end.date), 
                                  site = settings$run$site, 
                                  vars.used.index = vars.used.index,
                                  time.row = format$time.row)
    
    ## Preprocess data
    # TODO: Generalize
    # TODO: Soil Respiration uncertainty calculation
    if(all(data.var %in% c("NEE", "FC", "LE", "UST"))) {    
      
      ustar.thresh <- 0.4  # TODO: soft code this
      
      var.obs <- colnames(inputs[[i]]$data)[!colnames(inputs[[i]]$data) %in% c("UST", "posix", "year", format$vars[format$time.row,]$bety_name)]
      
      AMFo                        <- inputs[[i]]$data[[var.obs]]
      UST                         <- inputs[[i]]$data$UST
      AMFo[AMFo == -9999]         <- NA
      AMFo[UST < ustar.thresh]    <- NA
      inputs[[i]]$data[[var.obs]] <- AMFo # write filtered data
      
      # Have to just pretend like these quality control variables exist...
      AMFq  <- rep(0, length(AMFo))
      flags <- TRUE
      
      AMF.params <- PEcAn.uncertainty::flux.uncertainty(AMFo, AMFq, flags, bin.num = 20)
      
      inputs[[i]]$obs <- AMFo
      inputs[[i]]$par <- c(AMF.params$intercept, AMF.params$slopeP, AMF.params$slopeN)
    }else{
      inputs[[i]]$obs <- inputs[[i]]$data[colnames(inputs[[i]]$data) %in% data.var]
      inputs[[i]]$par <- stats::sd(unlist(inputs[[i]]$obs), na.rm = TRUE) # testing
    }
    inputs[[i]]$n <- sum(!is.na(inputs[[i]]$obs))
  }  # end loop over files
  
  return(inputs)
} # load.pda.data
