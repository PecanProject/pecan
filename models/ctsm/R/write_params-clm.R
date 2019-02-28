#' Write CLM Parameter File
#'
#' @param defaults 
#' @param trait.values named list or data frame of traits, e.g.
#' \code{data.frame(vmax = 1, b0 = 2)} or \code{list(vmax = 1, b0 = 2)}
#' @param settings 
#' @param run.id 
#'
#' @return
#' @export
#'
#' @examples
write_params_ctsm <- function(defaults = system.file('', package = 'PEcAn.CTSM'),
                              trait.values, settings, run.id){
  
  ## COPY AND OPEN DEFAULT PARAMETER FILES
  # TODO: update this to read param files (CLM ONLY) out of the refcase directory, not the PEcAn package
  # TODO: clear out variables that aren't need to run ED model
  ## See issue https://github.com/PecanProject/pecan/issues/1008
  clm.param.default <- system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM')
  #clm.param.default <- file.path(refcase,"clm5_params.c171117.nc") # probably need to allow custom param file names here (pecan.xml?)
  clm.param.file <- file.path(local.rundir,paste0("clm_params.",run.id,".nc"))
  file.copy(clm.param.default,clm.param.file)
  clm.param.nc <- ncdf4::nc_open(clm.param.file,write=TRUE)
  
  clm_vars = c()
  
  ## Loop over PFTS
  npft <- length(trait.values)
  PEcAn.logger::logger.debug(npft)
  PEcAn.logger::logger.debug(dim(trait.values))
  PEcAn.logger::logger.debug(names(trait.values))
  #pftnames <- stringr::str_trim(tolower(ncvar_get(param.nc,"pftname"))) 
  pftnames <- stringr::str_trim(tolower(ncdf4::ncvar_get(clm.param.nc,"pftname")))
  PEcAn.logger::logger.debug(paste0("clm PFT names: "),pftnames)
  for (i in seq_len(npft)) {
    pft <- trait.values[[i]]
    print(c("PFT",i))
    PEcAn.logger::logger.info(pft)
    pft.name <- names(trait.values)[i]
    if(is.null(pft.name) | is.na(pft.name)){
      PEcAn.logger::logger.error("pft.name missing")
    } else {
      PEcAn.logger::logger.info(paste("PFT =",pft.name))
      PEcAn.logger::logger.debug(paste0("clm PFT number: ",which(pftnames==pft.name)))
    }
    if(pft.name == 'env') next   ## HACK, need to remove env from default
    
    ## Match PFT name to COLUMN
    ipft <- match(tolower(pft.name),pftnames)
    PEcAn.logger::logger.debug(paste0("ipft: ",ipft))
    
    if(is.na(ipft)){
      PEcAn.logger::logger.severe(paste("Unmatched PFT",pft.name,
                                        "in CLM. PEcAn does not yet support non-default PFTs for this model"))
    }
    
    # hard code hack until we can use more than 2 pfts in CLM 
    ipft <- 2
    PEcAn.logger::logger.debug(paste0("*** PFT number hard-coded to ", ipft," in clm. This will be updated when clm allows more PFTs"))
    
    ## Special variables used in conversions
    #     leafC <- pft['leafC']/100  ## percent to proportion
    leafC <- NA
    if(is.na(leafC)) leafC <- 0.48
    
    # determine photo pathway
    PEcAn.logger::logger.debug(paste0("Photosynthesis pathway flag value: ", photo_flag))
    
    ## Loop over VARIABLES
    for (v in seq_along(pft)) {
      var <- names(pft)[v]
      
      ## Required variables from google doc for CLM/FATES workshop Feb 2019: https://docs.google.com/document/d/12XWb7DmmOuvRAOEDviBt-x2uCUWIRDASZA2T9u_fbY0/edit#
      ### ----- Leaf physiological parameters
      ## missing from params.nc 
      ## Vcmax
      # if(var == "Vcmax"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      # 
      ## missing from params.nc 
      ##Jmax
      # if(var == "Jmax"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      ## missing from params.nc 
      ## Leaf Respiration
      # if(var == "Rd"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      ## missing from params.nc 
      ## Area based leaf nitrogen concentration
      # if(var == "Na"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      
      # Leaf C:N ratio
      if(var == "leafcn"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='leafcn', start = ipft, count = 1,
                         vals=pft[v])  ## (gC/gN)
      }
      
      # Target C:N ratio fine roots
      if(var == "frootcn"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='frootcn', start = ipft, count = 1,
                         vals=pft[v])  # gC/gN
      }
      
      # Fraction of leaf nitrogen in Rubisco
      if(var == "flnr"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='flnr', start = ipft, count = 1,
                         vals=pft[v])  ## (gC/gN)
      }
      ## missing from params.nc 
      ## Mass ratio of total Rubisco molecular mass to nitrogen in Rubisco
      # if(var == "mrnr"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  
      # }
      
      # SLA is top of canopy in clm (units = m^2/gC)
      if(var == "sla"){                                  ## default 0.012
        ncdf4::ncvar_put(nc=clm.param.nc, varid='slatop', start = ipft, count = 1,  
                         vals=udunits2::ud.convert(pft[v],"m2 kg-1","m2 g-1")/leafC)
        
      }
      ## missing from params.nc 
      ## gs
      # if(var == "gs"){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      
      # g1 BB: Ball-Berry slope of conductance-photosynthesis relationship, unstressed
      if(var == "stomatal_slope.BB"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='mbbopt', start = ipft, count = 1,
                         vals=pft[v]) ##(clm: umol H2O/umol CO2)
      }
      ## missing from params.nc 
      # # g0 BB: Ball-Berry intercept of conductance-photosynthesis relationship, unstressed
      #   if(var == "stomatal_int.BB"){
      #     ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) ##(clm: )
      #   }
      
     
      # g1 M: Medlyn slope of conductance-photosynthesis relationship
      if(var == "stomatal_slope.M"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='medlynslope', start = ipft, count = 1,
                         vals=pft[v]) ##(clm: umol H2O/umol CO2)
      }
      
      # g0 M: Medlyn intercept of conductance-photosynthesis relationship
      if(var == "stomatal_int.M"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='medlynintercept', start = ipft, count = 1,
                         vals=pft[v]) ##(clm: umol H2O)
      }
      
      
      # Ratio of new fine root : new leaf carbon allocation
      if(var == "froot_leaf"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='froot_leaf', start = ipft, count = 1,
                         vals=pft[v]) ##(clm: gC/gC)
      }
     
      # Ratio of growth respiration carbon : new growth carbon
      if(var == "grperc"){
        ncdf4::ncvar_put(nc=clm.param.nc, varid='grperc', start = ipft, count = 1,
                         vals=pft[v]) ##(clm:unitless)
      }

      # # Target C:N ratio leaf
      # ## missing from params.nc
      # if(var == ""){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) # gC/gN
      # }
      # 
      # # Target C:N ratio fine roots
      # ## missing from params.nc
      # if(var == ""){
      #   ncdf4::ncvar_put(nc=clm.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  # gC/gN
      # }

      # # Root mass per soil layer profile
      # ## missing from params.nc 
      #   if(var == "rootmassSoil"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) 
      #   }

      
    } ## end loop over VARIABLES
  } ## end loop over PFTs
  ncdf4::nc_close(.param.nc)
  
  
}
}