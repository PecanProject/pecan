##' Writes config files for use with FATES.
##'
##' @name write.config.FATES
##' @title Write FATES configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Mike Dietze, Shawn Serbin
##-------------------------------------------------------------------------------------------------# # nolint
# example trait.values list

write.config.FATES <- function(defaults, trait.values, settings, run.id){

   ## site information
   site <- settings$run$site
   site.id <- site$id # change1: 772 -> SOD1
  
   # find out where things are
   #local.rundir <- file.path(settings$rundir, run.id) ## this is on local machine for staging
   #rundir       <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
   #casedir      <- rundir # file.path(rundir,run.id) ## /ctsm-api/resources/cases/case_SOD1, change2: 'case' -> site.id
   #outdir       <- file.path(settings$host$outdir, run.id)
   #refcase      <- settings['model']['binary'] # question1: refcase==casedir?
   #bld          <- file.path(refcase,"bld")
   #binary       <- file.path(bld,"cesm.exe")
   #indir        <- file.path(rundir,"data/shared") ## input directory
   #default      <- settings$run$inputs$default$path ## reference inputs file structure
   #site_name    <- paste0(site.id, "_c", run.id) ## change3 
   
   ## DATES
   ## CLM is a bit odd and takes a start date and length, so we need to precompute
   ## this needs to be generalized to fractional years, but accounting for 365 day year
   start_date <- as.Date(settings$run$start.date)
   end_date   <- as.Date(settings$run$end.date)
   stop_n     <- as.numeric(end_date - start_date, units="days") #- PEcAn.utils::n_leap_day(start_date,end_date) + 1  
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             INPUTS                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##

   ## SITE INFO --> DOMAIN FILE (lat/lon)
   #  - should also store this in the refcase directory for PEcAn so we can grab from there, and not the PEcAn package
   #gridres <- 0.125  ## ultimately this should be a variable
   #lat <- as.numeric(site$lat)
   #lon <- (as.numeric(site$lon) + 360) %% 360 ## make sure coords in 0-360 range, not negative
   #domain.default <- system.file("domain.lnd.360x720_gswp3.0v1.c170606.nc",package="PEcAn.FATES")
   #domain.file <- file.path(local.rundir,paste0("domain.lnd.fv0.9x1.25_gx1v7_",site_name,".nc"))
   #file.copy(domain.default,domain.file)
   #domain.nc <- ncdf4::nc_open(domain.file,write=TRUE)
   #ncdf4::ncvar_put(nc=domain.nc, varid='xc', vals=lon)
   #ncdf4::ncvar_put(nc=domain.nc, varid='yc', vals=lat)
   #ncdf4::ncvar_put(nc=domain.nc, varid='xv', vals=lon+c(-1,1,1,-1)*gridres)
   #ncdf4::ncvar_put(nc=domain.nc, varid='yv', vals=lat+c(-1,-1,1,1)*gridres)
   #ncdf4::ncvar_put(nc=domain.nc, varid='area', vals=(2*gridres*pi/180)^2)   
   #ncdf4::nc_close(domain.nc)
   
   ## SURF - should also store this in the refcase directory for PEcAn so we can grab from there, and not the PEcAn package
   #surf.default <- system.file("surfdata_0.9x1.25_hist_16pfts_Irrig_CMIP6_simyr2000_c190214.nc",package = "PEcAn.FATES")
   #surf.file    <- file.path(local.rundir,paste0("surfdata_0.9x1.25_hist_16pfts_Irrig_CMIP6_simyr2000_",site_name, ".nc"))
   #file.copy(surf.default,surf.file)
   #Sys.chmod(surf.file)
   #surf.nc <- ncdf4::nc_open(surf.file,write=TRUE)
   #ncdf4::ncvar_put(nc=surf.nc, varid='LONGXY', vals=lon)
   #ncdf4::ncvar_put(nc=surf.nc, varid='LATIXY', vals=lat)
   #ncdf4::nc_close(surf.nc)   
   
   ## DATM & DATM Stream??
   ## MET HEADERS
   #if(!is.null(settings$run$inputs$met)){

     ## DATM HEADER: datm_atm_in
     #datm <- readLines(con=system.file("datm_atm_in.template",package = "PEcAn.FATES"),n=-1)
     #datm <- gsub('@DOMAIN@', file.path(indir,"share/domains/domain.clm",basename(domain.file)), datm)
     #datm <- gsub('@START_YEAR@',lubridate::year(start_date), datm)
     #datm <- gsub('@END_YEAR@',lubridate::year(end_date), datm)
     #writeLines(datm, con=file.path(local.rundir, "datm_atm_in"))
     
     ### DATM STREAM MET
     #met <- readLines(con=system.file("datm.streams.txt.PEcAn_met.template",package = "PEcAn.FATES"),n=-1)
     #met <- gsub('@INDIR@',indir, met)
     #domain.file.name <- paste0("domain.lnd.",site_name,".nc")
     #met <- gsub('@DOMAIN@',domain.file.name, met)  # attempting to provide correct domain file name
     #met <- gsub('@MET_PATH@',settings$run$inputs$met$path, met)
     #met.files <- dir(settings$run$inputs$met$path,"*.nc")
     #met <- gsub('@MET_FILES@',paste(met.files,collapse = "\n"), met)
     #writeLines(met, con=file.path(local.rundir, "datm.streams.txt.PEcAn_met"))
     
   #}
#   ... need to set this up so that if MET is blank it can run with default CLM met
#   ... fill in this template, the met template, and then have jobs.sh put them in the right place. 
#   ... Test, then adjust DB to have met required
   
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             JOB.SH                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##
   
# create launch script (which will create symlink)
   if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
     jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
   } else {
     jobsh <- readLines("/Users/mac/Documents/pecan/models/fates/R/template.job")
     #jobsh <- readLines(con=system.file("template.job", package = "PEcAn.FATES"), n=-1)
   }
   
 # create host specific settings
  # hostsetup <- ""
  # if (!is.null(settings$model$prerun)) {
  #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
  # }
  # if (!is.null(settings$host$prerun)) {
  #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
  # }

  # hostteardown <- ""
  # if (!is.null(settings$model$postrun)) {
  #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
  # }
  # if (!is.null(settings$host$postrun)) {
  #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
  # }

# create job.sh
   #jobsh <- gsub('@HOST_SETUP@', hostsetup, jobsh)
   #jobsh <- gsub('@HOST_TEARDOWN@', hostteardown, jobsh)
   
   ## Machine configs
   #   ./create_newcase –case A –res @RES@ –compset @COMPSET@ –driver @DRIVER@ –machine @MACHINE@ –run-unsupported
   if (!is.null(settings$model$machine)) {
     machine <- paste(settings$model$machine, collapse="\n") 
   } else {
     machine <- "container-nlp"
   }
   jobsh <- gsub('@MACHINE@', machine, jobsh)
   if (!is.null(settings$model$driver)) {
     driver <- paste(settings$model$driver, collapse="\n")
   } else {
     driver <- "nuopc"
   }
   jobsh <- gsub('@DRIVER@', driver, jobsh)
   if (!is.null(settings$model$compset)) {
     compset <- paste(settings$model$compset, collapse="\n")
   } else {
     compset <- "2000_DATM%GSWP3v1_CLM51%FATES_SICE_SOCN_MOSART_SGLC_SWAV"
   }
   jobsh <- gsub('@COMPSET@', compset, jobsh)
   if (!is.null(settings$model$res)) {
     res <- paste(settings$model$res, collapse="\n")
   } else {
     res <- "CLM_USRDAT"
   }
   jobsh <- gsub('@RES@', res, jobsh)
   
   ## PATHS
   #jobsh <- gsub('@RUNDIR@', rundir, jobsh)
   #jobsh <- gsub('@CASEDIR@', casedir, jobsh)
   #jobsh <- gsub('@OUTDIR@', outdir, jobsh)
   #jobsh <- gsub('@REFCASE@', refcase, jobsh)
   #jobsh <- gsub('@BLD@', bld, jobsh)
   #jobsh <- gsub('@BINARY@', binary, jobsh)
   #jobsh <- gsub('@INDIR@', indir, jobsh)
   #jobsh <- gsub('@DEFAULT@', default, jobsh)
   #jobsh <- gsub('@SITE_NAME@', site_name, jobsh) 
  
   ## DATES -> ENV_RUN
   jobsh <- gsub('@START_DATE@', start_date, jobsh)
   jobsh <- gsub('@STOP_N@', stop_n, jobsh)
   jobsh <- gsub('@RUN_ID@', run.id, jobsh)
   
   ## MET --> DATM
#   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
   ## FOR FIRST STEP, CAN USE DEFAULT
   writeLines(jobsh, file.path(settings$rundir, run.id, "job.sh"))
   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

#   ## Write PARAMETER file
   
   ## COPY AND OPEN DEFAULT PARAMETER FILES
   # TODO: update this to read param files (CLM and FATES) out of the refcase directory, not the PEcAn package
   # TODO: update to allow it to pick between CLM4.5 and CLM5 parameter set based on refcase, user selection
   ## See issue https://github.com/PecanProject/pecan/issues/1008
   # CLM
   #clm.param.default <- system.file("clm5_params.c171117.nc",package="PEcAn.FATES")
   #clm.param.file <- file.path(local.rundir,paste0("clm_params.",run.id,".nc"))
   ## Position of parameters file?
   #clm.param.default <- file.path(refcase,"clm5_params.c171117.nc") # probably need to allow custom param file names here (pecan.xml?)
   #clm.param.file <- file.path(local.rundir,paste0("clm_params.",run.id,".nc"))
   #file.copy(clm.param.default,clm.param.file)
   #clm.param.nc <- ncdf4::nc_open(clm.param.file,write=TRUE)
   
   # FATES
   #fates.param.default <- system.file("fates_params_2troppftclones.c171018_sps.nc",package="PEcAn.FATES")
   # above is a temporary param file corrected for the tropics by lowering freezing tolerace parameters
   #fates.param.default <- file.path(refcase,"fates_params_2troppftclones.c171018_sps.nc") # probably need to allow custom param file names here (pecan.xml?)
   #fates.param.file <- file.path(local.rundir,paste0("fates_params.",run.id,".nc"))
   #file.copy(fates.param.default,fates.param.file)
   #fates.param.nc <- ncdf4::nc_open(fates.param.file,write=TRUE)
   
   ## Loop over PFTS
   #npft <- length(trait.values)
   #PEcAn.logger::logger.debug(npft)
   #PEcAn.logger::logger.debug(dim(trait.values))
   #PEcAn.logger::logger.debug(names(trait.values))
   #pftnames <- stringr::str_trim(tolower(ncvar_get(param.nc,"pftname"))) 
   #pftnames <- stringr::str_trim(tolower(ncdf4::ncvar_get(clm.param.nc,"pftname")))
   #PEcAn.logger::logger.debug(paste0("CLM PFT names: "),pftnames)
   #for (i in seq_len(npft)) {
     #pft <- trait.values[[i]]
     #print(c("PFT",i))
     #PEcAn.logger::logger.info(pft)
     #pft.name <- names(trait.values)[i]
     #if(is.null(pft.name) | is.na(pft.name)){
       #PEcAn.logger::logger.error("pft.name missing")
     #} else {
       #PEcAn.logger::logger.info(paste("PFT =",pft.name))
       #PEcAn.logger::logger.debug(paste0("fates-clm PFT number: ",which(pftnames==pft.name)))
     #}
     #if(pft.name == 'env') next   ## HACK, need to remove env from default
     
     ## Match PFT name to COLUMN
     #ipft <- match(tolower(pft.name),pftnames)
     #PEcAn.logger::logger.debug(paste0("ipft: ",ipft))
                                      
     #if(is.na(ipft)){
       #PEcAn.logger::logger.severe(paste("Unmatched PFT",pft.name,
                          #"in FATES. PEcAn does not yet support non-default PFTs for this model"))
     #}
     
     # hard code hack until we can use more than 2 pfts in FATES 
     #ipft <- 2
     #PEcAn.logger::logger.debug(paste0("*** PFT number hard-coded to ", ipft," in fates. This will be updated when FATES allows more PFTs"))
     
     ## Special variables used in conversions
#     leafC <- pft['leafC']/100  ## percent to proportion
     #leafC <- NA
     #if(is.na(leafC)) leafC <- 0.48
     
     # determine photo pathway
     #photo_flag <- ncdf4::ncvar_get(fates.param.nc,varid="fates_c3psn", start = ipft, count = 1)
     #PEcAn.logger::logger.debug(paste0("Photosynthesis pathway flag value: ", photo_flag))
     
     ## Loop over VARIABLES
     #for (v in seq_along(pft)) {
       #var <- names(pft)[v]

       ## THESE NEED SOME FOLLOW UP
       
       ### ----- Leaf physiological parameters
       # Vcmax
       # Ball-Berry slope

       # Ball-Berry intercept - c3.  We need to figure out how to set either C3 or C4 values? Based on the PFT?
       # TODO: allow setting this for C3 and/or C4 PFTs
       # right now, each are just one dimension, will need to revist this if this changes.
       
       # T response params - modified Arrhenius params for Vcmax, Jmax, and TPU
       # -- NOT YET IMPLEMENTED IN BETYdb. FATES params:
       # fates_vcmaxha, fates_jmaxha, fates_tpuha, fates_vcmaxhd, fates_jmaxhd, fates_tpuhd,
       # fates_vcmaxse, fates_jmaxse, fates_tpuse
      
       # Ha activation energy for vcmax - FATES units: J/mol

       # Hd deactivation energy for vcmax - FATES units: J/mol

       # Ha activation energy for Jmax - FATES units: J/mol
       
       # Hd deactivation energy for Jmax - FATES units: J/mol

       # deltaS Vcmax - BETY units:J/mol/K;  FATES units: J/mol/K
       
       # deltaS Jmax - BETY units:J/mol/K;  FATES units: J/mol/K

       ### ----- Leaf physiological parameters
       
       
       ### These variable names (from ED2) should updated in BETY to be more generic
       ## missing from params.nc       
#       if(var == "mort3"){
#         ncvar_put(nc=param.nc, varid='background_mort_rate', start = ipft, count = 1,
#                   vals=pft[v])  
#       }
       ## PFT-level variables

## missing from params.nc 
#       if(var == "cuticular_cond"){
#         gH2O_per_mol <- 18.01528
#         ncvar_put(nc=param.nc, varid='gsmin', start = ipft, count = 1,
#                   vals=pft[v]*gH2O_per_mol*1e-12)   ### umol H2O m-2 s-1 ->  [m s-1]
#       }

       # if(var == "sapwood_ratio"){         # leaf to sapwood area ratio. IS THIS NOW fates_sapwood_ratio(fates_pft)??
       #   ncvar_put(nc=fates.param.nc, varid='latosa', start = ipft, count = 1,
       #             vals=PEcAn.utils::ud_convert(pft[v],"m2 m-2","m2 cm-2"))
       # }
       
       # leaf to sapwood area ratio. This is the INTERCEPT parameter in FATES
       # [sserbin@modex paramdata]$ ncdump fates_params_2troppftclones.c171018.nc | grep latosa
       # double fates_allom_latosa_int(fates_pft) ;
       # fates_allom_latosa_int:long_name = "Leaf area to sap area ratio, intercept [m2/cm2]" ;
       #fates_allom_latosa_int:units = "ratio" ;
       # double fates_allom_latosa_slp(fates_pft) ;
       # fates_allom_latosa_slp:long_name = "Leaf area to sap area ratio, slope (optional)" ;
       # fates_allom_latosa_slp:units = "unitless" ;
       # fates_allom_latosa_int = 0.001, 0.001 ;
       # fates_allom_latosa_slp = 0, 0 ;


       ## --- update these to match new FATES hydro code when that code-base is added to FATES master -- ##
       ## --- update these to match new FATES hydro code when that code-base is added to FATES master -- ##
       
       #if(var == "grass_spread"){         # Controls the area footprint of a grass pft, as a function of number density and dbh
       #  ncdf4::ncvar_put(nc=fates.param.nc, varid='grass_spread', start = ipft, count = 1,
       #            vals=pft[v])
       #}
       
       

       ## BINARY FLAGS: These should be set-able by PEcAn but not sampled
       #if(var == "photosynthetic_pathway"){         # 
       #  ncdf4::ncvar_put(nc=fates.param.nc, varid='c3psn', start = ipft, count = 1,
       #            vals=as.numeric(pft[v] == 3))
       #}
       
       
       ## ALLPFT indexed (size = 1) 
       #if(var == "veg_respiration_Q10"){            ## Q10 for maintenance respiration. CLM param. q10_mr(allpfts)
         #ncdf4::ncvar_put(nc=clm.param.nc, varid='q10_mr', start = 1, count = 1,
                   #vals=pft[v])
       #}
       
       ## NONE indexed
       ##   -- FIRE

       ##   -- CANOPY
       #if(var == "canopy_max_spread"){            ## Maximum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
       #  ncdf4::ncvar_put(nc=fates.param.nc, varid='maxspread',vals=pft[v])
       #}
       # 	
       #if(var == "canopy_min_spread"){            ## Minimum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
       #  ncdf4::ncvar_put(nc=fates.param.nc, varid='minspread',vals=pft[v])
       #}

       ## LITTERCLASS indexed (Size:6)
       ## MCD: skipping for now until there's demonstrated demand because it requires expanding every variable out into VARNAME_[1..n]
       # low_moisture_C	Intercept (constant) of fuel moisture to burned fraction term for drier fuel	litterclass	
       # low_moisture_S	Slope of fuel moisture to burned fraction term for drier fuel	litterclass	
       # max_decomp	Maximum decomposition rate of litter in the absence of moisture or temperature stress, per fuel class	litterclass	y-1
       # mid_moisture	Parameter of burned fraction term. Below this 'low' constants apply, above this, 'mid' constants apply, 	litterclass	
       # mid_moisture_C	Intercept (constant) of fuel moisture to burned fraction term for wetter fuel	litterclass	
       # min_moisture	Parameter of burned fraction term. Below this value all litter is burned by a fire. Above, 'low' constants apply	litterclass	
       # FBD	Fuel Bulk Density of fuel class 	litterclass	kg m-3
       # alpha_FMC	Parameter of function relating fuel moisture content to meteorological fire danger index 	litterclass	
       # SAV	Surface Area to Volume Ratio of fuel class 	litterclass	cm-1
       
       ## NCWD dimensioned       Size:4
       
       
     #} ## end loop over VARIABLES
   #} ## end loop over PFTs
   #ncdf4::nc_close(param.nc)
   #ncdf4::nc_close(clm.param.nc)
   #ncdf4::nc_close(fates.param.nc)
   
#   ## Write SETTINGS file
#     
}
#---------------------------------------------------------------------------------------------------------------------#
### EOF
