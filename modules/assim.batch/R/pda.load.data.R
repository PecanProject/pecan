##' Load Dataset for Paramater Data Assimilation
##'
##' @title Load Dataset for Paramater Data Assimilation
##' ##' This function is used to load and preprocess data for PDA. It is expected to be moved / merged 
##' with a more general PEcAn 'load.data' function eventually.
##' 
##' @param settings = PEcAn settings list
##'
##' @return A list containg the loaded input data, plus metadata
##'
##' @author Ryan Kelly, Istem Fer
##' @export
load.pda.data <- function(settings, con) {
  
  require(PEcAn.benchmark)

  # Outlining setup for multiple datasets

  inputs <- list()
  input.settings <- settings$assim.batch$inputs
  n.input <- length(input.settings)
  
  for(i in 1:n.input) {
    inputs[[i]] <- list()
    inputs[[i]]$variable.name <- input.settings[[i]]$variable.name
    data.path <- input.settings[[i]]$path

    obvs.id <- input.settings[[i]]$input.id 
    
    # I require that the user defines data.path in the settings as well, instead of using query.file.path
    # because 'data.path <- query.file.path(obvs.id, con)' might return an incomplete path 
    # which results in reading all the files in that particular directory in the load.x_netcdf step
    # this may not be a problem but when there are hundreds of files in the folder and we want to read a few that would be unnecessary
    if(is.null(obvs.id) | is.null(data.path)) {            
      logger.error("Must provide both ID and PATH for all data assimilation inputs.")
    }
    
    format <- query.format.vars(obvs.id, con) 
    
    #model_vars <- format$vars$pecan_name
    vars.used.index <- which(format$vars$bety_name %in% c(inputs[[i]]$variable.name))
    
    
    inputs[[i]]$data <- load.data(data.path, format, start_year = year(settings$run$start.date), 
                                  end_year = year(settings$run$end.date), site = settings$run$site, 
                                  vars.used.index, time.row = NULL)
    
    ## Preprocess data
    # TODO: Generalize
    if(all(inputs[[i]]$variable.name %in% c("NEE", "FC", "LE", "UST"))) {    
    

      # # TODO: Put Ameriflux L4 compatibility back
      # if(format$file_name == 'AmeriFlux.level4.h') {
      #   # Load L4 from a csv
      #   inputs[[i]]$data <- read.csv(input.settings[[i]]$path)
      #   
      #   ## calculate flux uncertainty parameters
      #   NEEo <- inputs[[i]]$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
      #   NEEq <- inputs[[i]]$data$NEE_or_fMDSqc #data$qf_Fc
      #   NEEo[NEEq > 0] <- NA
      #   dTa <- get.change(inputs[[i]]$data$Ta_f)
      #   flags <- dTa < 3   ## filter data to temperature differences that are less than 3 degrees
      # } else if(input.settings[[i]]$format == 'Ameriflux.L2') {

      if(format$file_name == "AmeriFlux.level2.h.nc") {
        
        ustar.thresh <- 0.2 # TODO: soft code this
        
        var.obs <- colnames(inputs[[i]]$data)[!colnames(inputs[[i]]$data) %in% c("UST", "posix")]
        
        AMFo <- inputs[[i]]$data[[var.obs]]
        UST <- inputs[[i]]$data$UST
        AMFo[AMFo == -9999] <- NA
        AMFo[UST < ustar.thresh] <- NA
        
        # Have to just pretend like these quality control variables exist...
        AMFq <- rep(0, length(AMFo))
        flags <- TRUE
      } 
      
      AMF.params <- flux.uncertainty(AMFo, AMFq, flags, bin.num = 20)
      
      inputs[[i]]$obs <- AMFo
      inputs[[i]]$par <- c(AMF.params$intercept, AMF.params$slopeP, AMF.params$slopeN)
    }
  } # end loop over files
  
  return(inputs)
}
