#' Write FATES Parameter File
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
write_params_ctsm <- function(defaults = system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM'),
                              trait.values, settings, run.id){
  
  ## COPY AND OPEN DEFAULT PARAMETER FILES
  # TODO: update this to read param files (CLM and FATES) out of the refcase directory, not the PEcAn package
  # TODO: update to allow it to pick between CLM4.5 and CLM5 parameter set based on refcase, user selection
  # TODO: separate CLM and FATES into 2 scripts. 
  # TODO: decide what CML variables are needed to run.
  ## See issue https://github.com/PecanProject/pecan/issues/1008
  # CLM5
  fates.param.default <- system.file('clm5_params.c171117_0001.nc', package = 'PEcAn.CTSM')
  #fates.param.file <- file.path(local.rundir,paste0("fates_params.",run.id,".nc"))
  #fates.param.default <- file.path(refcase,"clm5_params.c171117.nc") # probably need to allow custom param file names here (pecan.xml?)
  fates.param.file <- file.path(local.rundir,paste0("fates_params.",run.id,".nc"))
  file.copy(fates.param.default,fates.param.file)
  fates.param.nc <- ncdf4::nc_open(fates.param.file,write=TRUE)
  
  
  ## Loop over PFTS
  npft <- length(trait.values)
  PEcAn.logger::logger.debug(npft)
  PEcAn.logger::logger.debug(dim(trait.values))
  PEcAn.logger::logger.debug(names(trait.values))
  pftnames <- stringr::str_trim(tolower(ncdf4::ncvar_get(fates.param.nc,"pftname")))
  PEcAn.logger::logger.debug(paste0("FATES PFT names: "),pftnames)
  for (i in seq_len(npft)) {
    pft <- trait.values[[i]]
    print(c("PFT",i))
    PEcAn.logger::logger.info(pft)
    pft.name <- names(trait.values)[i]
    if(is.null(pft.name) | is.na(pft.name)){
      PEcAn.logger::logger.error("pft.name missing")
    } else {
      PEcAn.logger::logger.info(paste("PFT =",pft.name))
      PEcAn.logger::logger.debug(paste0("fates PFT number: ",which(pftnames==pft.name)))
    }
    if(pft.name == 'env') next   ## HACK, need to remove env from default
    
    ## Match PFT name to COLUMN
    ipft <- match(tolower(pft.name),pftnames)
    PEcAn.logger::logger.debug(paste0("ipft: ",ipft))
    
    if(is.na(ipft)){
      PEcAn.logger::logger.severe(paste("Unmatched PFT",pft.name,
                                        "in FATES. PEcAn does not yet support non-default PFTs for this model"))
    }
    
    # hard code hack until we can use more than 2 pfts in FATES 
    ipft <- 2
    PEcAn.logger::logger.debug(paste0("*** PFT number hard-coded to ", ipft," in fates. This will be updated when FATES allows more PFTs"))
    
    ## Special variables used in conversions
    #     leafC <- pft['leafC']/100  ## percent to proportion
    leafC <- NA
    if(is.na(leafC)) leafC <- 0.48
    
    # determine photo pathway
    PEcAn.logger::logger.debug(paste0("Photosynthesis pathway flag value: ", photo_flag))
    
    ## Loop over VARIABLES
    for (v in seq_along(pft)) {
      var <- names(pft)[v]
      
      # THESE NEED SOME FOLLOW UP
      ## ----- Leaf physiological parameters
      # Vcmax
      if(var == "Vcmax"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_leaf_vcmax25top', start = ipft, count = 1,
                         vals=pft[v])  ## umol CO2/m^2/s
      }

      # # missing from params.nc
      # #Jmax --> there is no term like Vcmax that is just jmax..... which parameter should be used?
      # if(var == "Jmax"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      # # missing from params.nc
      # # Leaf Respiration --> only respiration parametesr are growth resp factor (fates_grperc) and base maintenance resp rate for plant tissue (fates_base_mr_20)
      # if(var == "Rd"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      # # missing from params.nc
      # # Area based leaf nitrogen concentration
      # if(var == "Na"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (umol CO2 m-2 s-1)
      # }
      # # missing from params.nc
      # # Leaf C:N ratio
      # if(var == "CNl"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='leafcn', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      # 
      # # missing from params.nc
      # # Fraction of leaf nitrogen in Rubisco
      # if(var == "Flnr"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='flnr', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      # # missing from params.nc
      # # Mass ratio of total Rubisco molecular mass to nitrogen in Rubisco
      # if(var == "Fnr"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      
      # SLA is top of canopy in CLM (units = m^2/gC)
      if(var == "sla"){                                  ## default 0.012
        ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_leaf_slatop', start = ipft, count = 1,  
                         vals=udunits2::ud.convert(pft[v],"m2 kg-1","m2 g-1")/leafC)
        
      }
      ## missing from params.nc 
      ## gs
      # if(var == "gs"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v])  ## (gC/gN)
      # }
      
      # g1 BB: Ball-Berry slope of conductance-photosynthesis relationship, unstressed
      #if(var == "stomatal_slope.BB"){
      if(var == "stomatal_slope.BB"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_leaf_BB_slope', start = ipft, count = 1,
                         vals=pft[v]) ##unitless
      }
      ## missing from params.nc 
      # # g0 BB: Ball-Berry intercept of conductance-photosynthesis relationship, unstressed
      #   if(var == "g0BB"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) ##(fates: )
      #   }
      
      
      # # g1 M: Medlyn slope of conductance-photosynthesis relationship
      # ## missing from params.nc
      # if(var == "stomatal_slope.M"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) ##(fates: umol H2O/umol CO2)
      # }
      # 
      # # g0 M: Medlyn intercept of conductance-photosynthesis relationship
      # ## missing from params.nc
      # if(var == "stomatal_int.M"){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) ##(fates: umol H2O)
      # }
      
      
      # Ratio of new fine root : new leaf carbon allocation
      if(var == "froot_leaf"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_allom_l2fr', start = ipft, count = 1,
                         vals=pft[v]) ##(fates: gC/gC)
      }
      
      # Ratio of growth respiration carbon : new growth carbon
      if(var == "grperc"){
        ncdf4::ncvar_put(nc=fates.param.nc, varid='fates_grperc', start = ipft, count = 1,
                         vals=pft[v]) ##unitless
      }
      
      # # Target: Leaf C:N
      # if(var == ""){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) ##unitless
      # }
      # 
      # # Target: Fine Roots C:N
      # if(var == ""){
      #   ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                    vals=pft[v]) ##unitless
      # }
      
      ## missing from params.nc 
      # # Root mass per soil layer profile
      #   if(var == "b"){
      #     ncdf4::ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
      #                      vals=pft[v]) ##(fates:gC/gN)
      #   }
      
    } ## end loop over VARIABLES
  } ## end loop over PFTs
  #ncdf4::nc_close(param.nc)
  ncdf4::nc_close(fates.param.nc)
  
  
}
}