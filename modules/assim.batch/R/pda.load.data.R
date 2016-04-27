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
##' @author Ryan Kelly
##' @export
load.pda.data <- function(settings, con) {
  ## load data
  # Outlining setup for multiple datasets, although for now the only option is to assimilate 
  # against a single NEE input
  inputs <- list()
  input.settings <- settings$assim.batch$inputs
  n.input <- length(input.settings)
  
  for(i in 1:n.input) {
    inputs[[i]] <- list()
    inputs[[i]]$variable.id <- input.settings[[i]]$variable.id
    
    ## Load input based on ID, PATH, or SOURCE
    if(!is.null(input.settings[[i]]$input.id)) {             # Input specified by ID
      ## Get file path from input id
      inputs[[i]]$input.id <- input.settings[[i]]$input.id
      file <- db.query(paste0('SELECT * FROM dbfiles WHERE container_id = ', input.settings[[i]]$input.id), con)
      input.settings[[i]]$path <- file.path(file$file_path, file$file_name)
    } else if(!is.null(input.settings[[i]]$path)) {    # Input specified by PATH
      inputs[[i]]$input.id <- -1
    } else if(!is.null(input.settings[[i]]$source)) {  # Input specified by SOURCE
      # TODO: insert code to extract data from standard sources (e.g. AMF)
    } else {
      logger.error("Must provide ID, PATH, or SOURCE for all data assimilation inputs")
    }
    
    
    ## Preprocess data
    # TODO: Generalize
    if(as.numeric(inputs[[i]]$variable.id) == 297) {
      # Need to have two cases now--proper L4 data, or Ustar-screened L2. Defaulting to L4 for backwards compatibility. For future, obviously the $format tag should be populated from inputs table in bety.
      if(is.null(input.settings[[i]]$format)) input.settings[[i]]$format <- 'Ameriflux.L4'
      if(input.settings[[i]]$format == 'Ameriflux.L4') {
        # Load L4 from a csv
        inputs[[i]]$data <- read.csv(input.settings[[i]]$path)
        
        ## calculate flux uncertainty parameters
        NEEo <- inputs[[i]]$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
        NEEq <- inputs[[i]]$data$NEE_or_fMDSqc #data$qf_Fc
        NEEo[NEEq > 0] <- NA
        dTa <- get.change(inputs[[i]]$data$Ta_f)
        flags <- dTa < 3   ## filter data to temperature differences that are less than 3 degrees
      } else if(input.settings[[i]]$format == 'Ameriflux.L2') {
        # Load L2 from netcdf
        ustar.thresh <- 0.2 # TODO: soft code this
        
        inputs[[i]]$data <- load.L2Ameriflux.cf(input.settings[[i]]$path)
        
        NEEo <- inputs[[i]]$data$NEE
        UST <- inputs[[i]]$data$UST
        NEEo[NEEo == -9999] <- NA
        NEEo[UST < ustar.thresh] <- NA
        
        # Have to just pretend like these quality control variables exist...
        NEEq <- rep(0, length(NEEo))
        flags <- TRUE
      } else {
        logger.severe(paste0("Unknown data type ", input.settings[[i]]$format, " for variable ID ", inputs[[i]]$variable.id))
      }
      
      NEE.params <- flux.uncertainty(NEEo,NEEq,flags,bin.num=20)
      
      inputs[[i]]$NEEo <- NEEo
      inputs[[i]]$b0 <- NEE.params$intercept
      inputs[[i]]$bp <- NEE.params$slopeP
      inputs[[i]]$bn <- NEE.params$slopeN
    }
  } # end loop over files
  
  return(inputs)
}
