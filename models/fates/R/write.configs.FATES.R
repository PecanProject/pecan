#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
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
##' @importFrom ncdf4 ncvar_put ncvar_get
##-------------------------------------------------------------------------------------------------#
write.config.FATES <- function(defaults, trait.values, settings, run.id){

   ## site information
   site <- settings$run$site
   site.id <- as.numeric(site$id)
  
   # find out where things are
   local.rundir <- file.path(settings$rundir, run.id) ## this is on local machine for staging
   rundir     <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
   casedir    <- file.path(rundir,"case") 
   outdir     <- file.path(settings$host$outdir, run.id)
   refcase    <- settings$model$binary
   bld        <- file.path(refcase,"bld")
   binary     <- file.path(bld,"cesm.exe")
   indir      <- file.path(rundir,"input") ## input directory
   default    <- settings$run$inputs$default$path ## reference inputs file structure
   site_name  <- paste0(site.id %/% 1000000000, "-", site.id %% 1000000000)
   
   ## DATES
   ## CLM is a bit odd and takes a start date and length, so we need to precompute
   ## this needs to be generalized to fractional years, but accounting for 365 day year
   start_date <- as.Date(settings$run$start.date)
   end_date   <- as.Date(settings$run$end.date)
   stop_n     <- as.numeric(end_date - start_date, units="days") - PEcAn.utils::n_leap_day(start_date,end_date) + 1  
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             INPUTS                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##

   ## SITE INFO --> DOMAIN FILE (lat/lon)
   gridres <- 0.125  ## ultimately this should be a variable
   lat <- as.numeric(site$lat)
   lon <- (as.numeric(site$lon) + 360) %% 360 ## make sure coords in 0-360 range, not negative
   domain.default <- system.file("domain.lnd.1x1pt-brazil_navy.090715.nc",package="PEcAn.FATES")
   domain.file <- file.path(local.rundir,paste0("domain.lnd.",site_name,".nc"))
   file.copy(domain.default,domain.file)
   domain.nc <- ncdf4::nc_open(domain.file,write=TRUE)
   ncvar_put(nc=domain.nc, varid='xc', vals=lon)
   ncvar_put(nc=domain.nc, varid='yc', vals=lat)
   ncvar_put(nc=domain.nc, varid='xv', vals=lon+c(-1,1,1,-1)*gridres)
   ncvar_put(nc=domain.nc, varid='yv', vals=lat+c(-1,-1,1,1)*gridres)
   ncvar_put(nc=domain.nc, varid='area', vals=(2*gridres*pi/180)^2)   
   ncdf4::nc_close(domain.nc)
   
   ## SURF
   #surf.default <- system.file("surfdata_ref.nc",package = "PEcAn.FATES")
   surf.default <- system.file("surfdata_1x1_brazil_16pfts_Irrig_CMIP6_simyr2000_c171214.nc",package = "PEcAn.FATES")
   surf.file    <- file.path(local.rundir,paste0("surfdata_",site_name,"_simyr2000.nc"))
   file.copy(surf.default,surf.file)
   Sys.chmod(surf.file)
   surf.nc <- ncdf4::nc_open(surf.file,write=TRUE)
   ncdf4::ncvar_put(nc=surf.nc, varid='LONGXY', vals=lon)
   ncdf4::ncvar_put(nc=surf.nc, varid='LATIXY', vals=lat)
   ncdf4::nc_close(surf.nc)   
   
   ## MET HEADERS
   if(!is.null(settings$run$inputs$met)){

     ## DATM HEADER: datm_atm_in
     datm <- readLines(con=system.file("datm_atm_in.template",package = "PEcAn.FATES"),n=-1)
     datm <- gsub('@DOMAIN@', file.path(indir,"share/domains/domain.clm",basename(domain.file)), datm)
     datm <- gsub('@START_YEAR@',lubridate::year(start_date), datm)
     datm <- gsub('@END_YEAR@',lubridate::year(end_date), datm)
     writeLines(datm, con=file.path(local.rundir, "datm_atm_in"))
     
     ## DATM STREAM MET
     met <- readLines(con=system.file("datm.streams.txt.PEcAn_met.template",package = "PEcAn.FATES"),n=-1)
     met <- gsub('@INDIR@',indir, met)
     #domain.file.name <- paste0("domain.lnd.",site_name,".nc")
     #met <- gsub('@DOMAIN@',domain.file.name, met)  # attempting to provide correct domain file name
     met <- gsub('@MET_PATH@',settings$run$inputs$met$path, met)
     met.files <- dir(settings$run$inputs$met$path,"*.nc")
     met <- gsub('@MET_FILES@',paste(met.files,collapse = "\n            "), met)
     writeLines(met, con=file.path(local.rundir, "datm.streams.txt.PEcAn_met"))
     
   }
   
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
     jobsh <- readLines(con=system.file("template.job", package = "PEcAn.FATES"), n=-1)
   }
   
 # create host specific setttings
   hostsetup <- ""
   if (!is.null(settings$model$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
   }
   if (!is.null(settings$host$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
   }

   hostteardown <- ""
   if (!is.null(settings$model$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
   }
   if (!is.null(settings$host$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
   }

# create job.sh
   jobsh <- gsub('@HOST_SETUP@', hostsetup, jobsh)
   jobsh <- gsub('@HOST_TEARDOWN@', hostteardown, jobsh)
   
   ## Machine configs
   #   ./create_newcase -case @CASEDIR@ -res 1x1_brazil -compset ICLM45ED -mach @MACHINE@ -compiler @COMPILER@ -project @PROJECT@
   if (!is.null(settings$model$machine)) {
     machine <- paste(settings$model$machine, collapse="\n") 
   } else {
     machine <- "eddi"
   }
   jobsh <- gsub('@MACHINE@', machine, jobsh)
   if (!is.null(settings$model$compiler)) {
     compiler <- paste(settings$model$compiler, collapse="\n")
   } else {
     compiler <- "gnu"
   }
   jobsh <- gsub('@COMPILER@', compiler, jobsh)
   if (!is.null(settings$model$resolution)) {
     resolution <- paste(settings$model$resolution, collapse="\n")
   } else {
     resolution <- "1x1_brazil"
   }
   jobsh <- gsub('@RES@', resolution, jobsh)
   if (!is.null(settings$model$compset)) {
     compset <- paste(settings$model$compset, collapse="\n")
   } else {
     compset <- "I2000Clm50FatesGs"
   }
   jobsh <- gsub('@COMPSET@', compset, jobsh)
   if (!is.null(settings$model$project)) {
     project <- paste(settings$model$project, collapse="\n")
   } else {
     project <- "pecan"
   }
   jobsh <- gsub('@PROJECT@', project, jobsh)
   
   ## PATHS
   jobsh <- gsub('@RUNDIR@', rundir, jobsh)
   jobsh <- gsub('@CASEDIR@', casedir, jobsh)
   jobsh <- gsub('@OUTDIR@', outdir, jobsh)
   jobsh <- gsub('@REFCASE@', refcase, jobsh)
   jobsh <- gsub('@BLD@', bld, jobsh)
   jobsh <- gsub('@BINARY@', binary, jobsh)
   jobsh <- gsub('@INDIR@', indir, jobsh)
   jobsh <- gsub('@DEFAULT@', default, jobsh)
   jobsh <- gsub('@SITE_NAME@', site_name, jobsh) 
  
   ## DATES -> ENV_RUN
   jobsh <- gsub('@START_DATE@', start_date, jobsh)
   jobsh <- gsub('@STOP_N@', stop_n, jobsh)
   jobsh <- gsub('@RUN_ID@', run.id, jobsh)
   
   ## MET --> DATM
#   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
   ## FOR FIRST STEP, CAN USE DEFAULT
   
   writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
#   
#   ## Write PARAMETER file
   
   ## COPY AND OPEN DEFAULT PARAMETER FILES
   # TODO: update this to read param files (CLM and FATES) out of the refcase directory, not the PEcAn package
   # TODO: update to allow it to pick between CLM4.5 and CLM5 parameter set based on refcase, user selection
   # CLM
   clm.param.default <- system.file("clm5_params.c171117.nc",package="PEcAn.FATES")
   clm.param.file <- file.path(local.rundir,paste0("clm_params.",run.id,".nc"))
   file.copy(clm.param.default,clm.param.file)
   clm.param.nc <- ncdf4::nc_open(clm.param.file,write=TRUE)
   
   # FATES
   fates.param.default <- system.file("fates_params_2troppftclones.c171018_sps.nc",package="PEcAn.FATES")
   # above is a temporary param file corrected for the tropics by lowering freezing tolerace parameters
   fates.param.file <- file.path(local.rundir,paste0("fates_params.",run.id,".nc"))
   file.copy(fates.param.default,fates.param.file)
   fates.param.nc <- ncdf4::nc_open(fates.param.file,write=TRUE)
   
   ## Loop over PFTS
   npft <- length(trait.values)
   PEcAn.logger::logger.debug(npft)
   PEcAn.logger::logger.debug(dim(trait.values))
   PEcAn.logger::logger.debug(names(trait.values))
   #pftnames <- stringr::str_trim(tolower(ncvar_get(param.nc,"pftname"))) 
   pftnames <- stringr::str_trim(tolower(ncvar_get(clm.param.nc,"pftname")))
   PEcAn.logger::logger.debug(paste0("CLM PFT names: "),pftnames)
   for (i in seq_len(npft)) {
     pft <- trait.values[[i]]
     print(c("PFT",i))
     PEcAn.logger::logger.info(pft)
     pft.name <- names(trait.values)[i]
     if(is.null(pft.name) | is.na(pft.name)){
       PEcAn.logger::logger.error("pft.name missing")
     } else {
       PEcAn.logger::logger.info(paste("PFT =",pft.name))
       PEcAn.logger::logger.debug(paste0("fates-clm PFT number: ",which(pftnames==pft.name)))
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
     
     ## Loop over VARIABLES
     for (v in seq_along(pft)) {
       var <- names(pft)[v]

       ## THESE NEED SOME FOLLOW UP       
       if(var == "Vcmax"){
         ncvar_put(nc=fates.param.nc, varid='fates_vcmax25top', start = ipft, count = 1,
                   vals=pft[v])  ## (umol CO2 m-2 s-1)
       }
       
       
       ### These variable names (from ED2) should updated in BETY to be more generic
## missing from params.nc       
#       if(var == "mort3"){
#         ncvar_put(nc=param.nc, varid='background_mort_rate', start = ipft, count = 1,
#                   vals=pft[v])  
#       }
       if(var == "r_fract"){                    ## Fraction of carbon balance remaining after maintenance costs have been met that is dedicated to seed production.	[0-1]
         ncvar_put(nc=fates.param.nc, varid='fates_seed_alloc', start = ipft, count = 1,
                   vals=pft[v])  
       }
       ## This one is currently allpft level but should be pft level  - no longer in FATES params, what was this changed to?
        if(var == "agf_bs"){                    ## The fraction of sapwood and structural biomass that is above ground [0-1]
          ncvar_put(nc=fates.param.nc, varid='fates_allom_agb_frac', start = 1, count = 1,
                    vals=pft[v])  
       }
       
       ## PFT-level variables
       if(var == "seed_rain_kgC"){                    ## External seed rain from outside site (non-mass conserving) ;
         ncvar_put(nc=fates.param.nc, varid='fates_seed_rain', start = ipft, count = 1,
                   vals=pft[v])  
       }
## missing from params.nc 
#       if(var == "cuticular_cond"){
#         gH2O_per_mol <- 18.01528
#         ncvar_put(nc=param.nc, varid='gsmin', start = ipft, count = 1,
#                   vals=pft[v]*gH2O_per_mol*1e-12)   ### umol H2O m-2 s-1 ->  [m s-1]
#       }
       if(var == "DBH_at_HTMAX"){                    ## note in FATES parameter list about switching to HTMAX
         ncvar_put(nc=fates.param.nc, varid='fates_allom_dbh_maxheight', start = ipft, count = 1,
                   vals=pft[v])  ## [cm]
       }
       if(var == "growth_resp_factor"){                    ## r_growth = grperc * (gpp+r_maint)  fates_grperc:long_name = "Growth respiration factor" ;
         ncvar_put(nc=fates.param.nc, varid='fates_grperc', start = ipft, count = 1,
                   vals=pft[v])  
       }
       if(var == "SLA"){                                  ## default 0.012
         ncvar_put(nc=fates.param.nc, varid='fates_slatop', start = ipft, count = 1,  
                   vals=udunits2::ud.convert(pft[v],"m2 kg-1","m2 g-1")/leafC)
       }
       if(var == "leaf_turnover_rate"){                   ## fates_leaf_long:long_name = "Leaf longevity (ie turnover timescale)" ;
         ncvar_put(nc=fates.param.nc, varid='fates_leaf_long', start = ipft, count = 1,
                   vals=1/pft[v]) ## leaf_long = 1/leaf_turnover_rate, 1/years -> years
       }
       if(var == "root_turnover_rate"){                   ## fates_root_long:long_name = "root longevity (alternatively, turnover time)" ;
         ncvar_put(nc=fates.param.nc, varid='fates_root_long', start = ipft, count = 1,
                   vals=1/pft[v]) ## root_long = 1/root_turnover_rate, 1/years -> years
       }
       if(var == "c2n_leaf"){
         ncvar_put(nc=fates.param.nc, varid='fates_leafcn', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "fineroot2leaf"){ #"Allocation parameter: new fine root C per new leaf C" units = "gC/gC"
         ncvar_put(nc=fates.param.nc, varid='fates_froot_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       
       # if(var == "sapwood_ratio"){         # leaf to sapwood area ratio. IS THIS NOW fates_sapwood_ratio(fates_pft)??
       #   ncvar_put(nc=fates.param.nc, varid='latosa', start = ipft, count = 1,
       #             vals=udunits2::ud.convert(pft[v],"m2 m-2","m2 cm-2"))
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
       if(var == "sapwood_ratio"){         
         ncvar_put(nc=fates.param.nc, varid='fates_allom_latosa_int', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m2 m-2","m2 cm-2"))
       }
       if(var == "leaf_width"){            # Characteristic leaf dimension use for aerodynamic resistance
         ncvar_put(nc=fates.param.nc, varid='fates_dleaf', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"mm","m"))
         #PEcAn.logger::logger.debug(paste0("fates_dleaf: ",udunits2::ud.convert(pft[v],"mm","m"))) # temp debugging
       }
       ## Currently not in param.nc file despite being on NGEE-T parameter list       
       #       if(var == "nonlocal_dispersal"){    # Place-holder parameter for important seed dispersal parameters
       #         ncvar_put(nc=param.nc, varid='seed_dispersal_x', start = ipft, count = 1,
       #                   vals=pft[v])
       #       }
       if(var == "hgt_min"){               # the minimum height (ie starting height) of a newly recruited plant" ;
         ncvar_put(nc=fates.param.nc, varid='fates_hgt_min', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_reflect_nir"){      # Leaf reflectance: near-IR	[0-1]
         ncvar_put(nc=fates.param.nc, varid='fates_rholnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_reflect_vis"){      # Leaf reflectance: visible	[0-1]
         ncvar_put(nc=fates.param.nc, varid='fates_rholvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_reflect_nir"){      # Stem reflectance: near-IR	[0-1]
         ncvar_put(nc=fates.param.nc, varid='fates_rhosnir', start = ipft, count = 1,
                   vals=pft[v])
       }

       if(var == "wood_reflect_vis"){      # Stem reflectance: visible	[0-1]
         ncvar_put(nc=fates.param.nc, varid='fates_rhosvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_trans_nir"){        # Leaf transmittance: near-IR
         ncvar_put(nc=fates.param.nc, varid='fates_taulnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_trans_vis"){        # Leaf transmittance: visible	pft
         ncvar_put(nc=fates.param.nc, varid='fates_taulvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_trans_nir"){        # Stem transmittance: near-IR
         ncvar_put(nc=fates.param.nc, varid='fates_tausnir', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_trans_vis"){        # Stem transmittance: visible
         ncvar_put(nc=fates.param.nc, varid='fates_tausvis', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "orient_factor"){         # Leaf/stem orientation index	[-0/4 <xl< 0.6], fates_xl:valid_range = -1., 1. ;
         ncvar_put(nc=fates.param.nc, varid='fates_xl', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "wood_density"){         # Wood Specific Gravity (ie density of wood relative to density of water),
                                          #fates_wood_density:long_name = "mean density of woody tissue in plant" ;
         ncvar_put(nc=fates.param.nc, varid='fates_wood_density', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "roota_par"){            # CLM rooting distribution parameter [1/m]
         ncvar_put(nc=fates.param.nc, varid='roota_par', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "rootb_par"){            # CLM rooting distribution parameter [1/m] 
         ncvar_put(nc=fates.param.nc, varid='rootb_par', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "gsmax"){         # Maximum stomatal conductance [m s-1]
         ncvar_put(nc=fates.param.nc, varid='gsmax', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "psi_stomata_closure"){         # Soil water potential at full stomatal closure	[mm]
         ncvar_put(nc=fates.param.nc, varid='smpsc', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m","mm"))
       }
       if(var == "psi_stomata_open"){            # Soil water potential at full stomatal opening	pft	[mm]
         ncvar_put(nc=fates.param.nc, varid='smpso', start = ipft, count = 1,
                   vals=udunits2::ud.convert(pft[v],"m","mm"))
       }
       if(var == "root_bulk_modulus"){         # coarse root bulk elastic modulus (εroot)	[MPa]
         ncvar_put(nc=fates.param.nc, varid='epsil_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "sapwood_bulk_modulus"){         # sapwood bulk elastic modulus (εstem)	[MPa]
         ncvar_put(nc=fates.param.nc, varid='epsil_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_bulk_modulus"){         # leaf bulk elastic modulus (εleaf) [MPa]
         ncvar_put(nc=fates.param.nc, varid='epsil_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "root_osmotic_potential"){         # coarse root osmotic potential at full turgor (πoroot)	[MPa]
         ncvar_put(nc=fates.param.nc, varid='pinot_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "sapwood_osmotic_potential"){         # sapwood osmotic potential at full turgor (πostem) [MPa]
         ncvar_put(nc=fates.param.nc, varid='pinot_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_osmotic_potential"){         # leaf osmotic potential at full turgor (πoleaf) [MPa]
         ncvar_put(nc=fates.param.nc, varid='pinot_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_leaf"){         # Maximum leaf hydraulic conductivity per unit leaf area [mmol m-2 s-1 Mpa-1]
         ncvar_put(nc=fates.param.nc, varid='kmax_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_root"){         # Maximum root hydraulic conductivity per unit xs sapwood [kg m-1 s-1 Mpa-1]
         ncvar_put(nc=fates.param.nc, varid='kmax_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "kmax_stem"){         # Maximum stem hydraulic conductivity per unit xs sapwood area	[kg m-1 s-1 Mpa-1]
         ncvar_put(nc=fates.param.nc, varid='kmax_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_gs"){         # leaf water potential at 50% loss of stomatal conductance (Pgs50)	[MPa]
         ncvar_put(nc=fates.param.nc, varid='p50_gs', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_leaf"){         # leaf water potential at 50% loss of leaf hydraulic conductivity (P50leaf)	pft	[MPa]
         ncvar_put(nc=fates.param.nc, varid='p50_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_root"){         # root water potential at 50% loss of root hydraulic conductivity	[MPa]
         ncvar_put(nc=fates.param.nc, varid='p50_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "p50_stem"){         # stem water potential at 50% loss of stem hydraulic conductivity [MPa]
         ncvar_put(nc=fates.param.nc, varid='p50_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_root"){         # coarse root relative water content at turgor loss (RWCtlproot)
         ncvar_put(nc=fates.param.nc, varid='rwctlp_root', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_sapwood"){         # sapwood relative water content at turgor loss
         ncvar_put(nc=fates.param.nc, varid='rwctlp_stem', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "water_content_TLP_leaf"){         # leaf relative water content at turgor loss
         ncvar_put(nc=fates.param.nc, varid='rwctlp_leaf', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leafp_min"){         # Minimum leaf water potential [MPa]
         ncvar_put(nc=fates.param.nc, varid='leafp_min', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "clone_alloc"){         # A carbon allocation that is added on to seed_alloc for trees larger than dbh_max.	[0-1]
         ncvar_put(nc=fates.param.nc, varid='clone_alloc', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "storage_target_ratio"){         # The target fraction of storage carbon over leaf carbon	[0-1]
         ncvar_put(nc=fates.param.nc, varid='cushion', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "crown_depth_fraction"){         # Crown depth fraction of a cohort relative to its total height [0-1]
         ncvar_put(nc=fates.param.nc, varid='crown', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "hydraulic_stress_mortality"){         # The mortality rate imposed on plants meeting hydraulic stress failure condition [1/yr]
         ncvar_put(nc=fates.param.nc, varid='stress_mort', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "PPA_comp_exclusion"){         # Competetive exclusion parameter for weighting demotions from the upper canopy classification in PPA
         ncvar_put(nc=fates.param.nc, varid='comp_excln', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "grass_spread"){         # Controls the area footprint of a grass pft, as a function of number density and dbh
         ncvar_put(nc=fates.param.nc, varid='grass_spread', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "leaf_stor_priority"){         # Leaf turnover vs labile carbon use prioritisation. This is the fraction of maintenance demand that will be replenished at all costs and before storage is filled.	pft	[0-1]
         ncvar_put(nc=fates.param.nc, varid='leaf_stor_priority', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "understory_treefall_mortality"){         # The fraction of trees in understory that die from impacts of large treefalls. In the model this is not a rate 1/year, so we need to convert it
         ncvar_put(nc=fates.param.nc, varid='understorey_death', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "displar"){         # 
         ncvar_put(nc=fates.param.nc, varid='displar', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "z0mr"){         # Ratio of momentum roughness length to canopy top height
         ncvar_put(nc=fates.param.nc, varid='z0mr', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "inital_stem_density"){         # Stem density of different PFTs during a bare ground initialization.	[/m2]
         ncvar_put(nc=fates.param.nc, varid='initd', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "bark_scaler"){         # Fraction of tree diameter that is bark. Used in fire.	[0-1]
         ncvar_put(nc=fates.param.nc, varid='bark_scaler', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "crown_kill"){                    ## SPITFIRE: Mortality from fire scorching susceptibility parameter
         ncvar_put(nc=fates.param.nc, varid='crown_kill', start = ipft, count = 1,
                   vals=pft[v])  
       }
       

       ## BINARY FLAGS: These should be set-able by PEcAn but not sampled
       if(var == "photosynthetic_pathway"){         # 
         ncvar_put(nc=fates.param.nc, varid='c3psn', start = ipft, count = 1,
                   vals=as.numeric(pft[v] == 3))
       }
       if(var == "crop"){         # Binary crop flag: 0. = not crop, 1. = crop
         ncvar_put(nc=fates.param.nc, varid='', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "irrigated"){         # Binary Irrigated PFT flag
         ncvar_put(nc=fates.param.nc, varid='irrigated', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "cold_deciduous"){         # Binary flag for seasonal-deciduous leaf habit (0-not,1-it is)
         ncvar_put(nc=fates.param.nc, varid='season_decid', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=0)
       }
       if(var == "stress_deciduous"){         # Binary flag for stress-deciduous leaf habit (0-not,1-it is)
         ncvar_put(nc=fates.param.nc, varid='stress_decid', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=0)
       }
       if(var == "woody"){         # Binary woody lifeform flag (0-is not woody, 1-it is woody)
         ncvar_put(nc=fates.param.nc, varid='woody', start = ipft, count = 1,
                   vals=pft[v])
       }
       if(var == "evergreen"){         # Binary flag for evergreen leaf habit
         ncvar_put(nc=fates.param.nc, varid='evergreen', start = ipft, count = 1,
                   vals=pft[v])
         ncvar_put(nc=fates.param.nc, varid='stress_decid', start = ipft, count = 1,
                   vals=0)
         ncvar_put(nc=fates.param.nc, varid='season_decid', start = ipft, count = 1,
                   vals=0)
       }
       
       ## ALLPFT indexed (size = 1) 
       if(var == "veg_respiration_Q10"){            ## Q10 for maintenance respiration. CLM param. q10_mr(allpfts)
         ncvar_put(nc=clm.param.nc, varid='q10_mr', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CelluloseS"){            ## Cellulose fraction for CWD
         ncvar_put(nc=fates.param.nc, varid='fates_cwd_fcel', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "s_lignin"){            ## Lignin fraction for CWD
         ncvar_put(nc=fates.param.nc, varid='fates_cwd_flig', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som1"){            ## C:N for SOM pool 1. CLM param
         ncvar_put(nc=clm.param.nc, varid='cn_s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som2"){            ## C:N for SOM pool 2. CLM param
         ncvar_put(nc=clm.param.nc, varid='cn_s2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "c2n_som3"){            ## C:N for SOM pool 3. CLM param
         ncvar_put(nc=clm.param.nc, varid='cn_s3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "cnscalefactor"){            ## Scale factor on CN decomposition for assigning methane flux . CLM param
         ncvar_put(nc=clm.param.nc, varid='cnscalefactor', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "decomp_depth_efolding"){            ## e-folding depth for reduction in decomposition. 
                                                      ## Set to large number for depth-independance. CLM param
         ncvar_put(nc=clm.param.nc, varid='decomp_depth_efolding', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_fragmentation_rate"){            ## Fragmentation rate for CWD. units = "1/day", CLM param
         ncvar_put(nc=clm.param.nc, varid='k_frag', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_cwdl2_bgc"){            ## respiration fraction from CWD to litter 2
         ncvar_put(nc=fates.param.nc, varid='rf_cwdl2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_cwdl3_bgc"){            ## respiration fraction from CWD to litter 3
         ncvar_put(nc=fates.param.nc, varid='rf_cwdl3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l1s1_bgc"){            ## Respiration fraction for litter 1 -> SOM 1
         ncvar_put(nc=fates.param.nc, varid='rf_l1s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l2s1_bgc"){            ## respiration fraction litter 2 to SOM 1
         ncvar_put(nc=fates.param.nc, varid='rf_l2s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_l3s2_bgc"){            ## respiration fraction from litter 3 to SOM 2
         ncvar_put(nc=fates.param.nc, varid='rf_l3s2_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s2s1_bgc"){            ## respiration fraction SOM 2 to SOM 1
         ncvar_put(nc=fates.param.nc, varid='rf_s2s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s2s3_bgc"){            ## Respiration fraction for SOM 2 -> SOM 3
         ncvar_put(nc=fates.param.nc, varid='rf_s2s3_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "rf_s3s1_bgc"){            ## respiration fraction SOM 3 to SOM 1
         ncvar_put(nc=fates.param.nc, varid='rf_s3s1_bgc', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "Q10_frozen_soil"){            ## Separate q10 for frozen soil respiration rates
         ncvar_put(nc=fates.param.nc, varid='froz_q10', start = 1, count = 1,
                   vals=pft[v])
       }
       
       ## NONE indexed
       ##   -- FIRE
       if(var == "max_fire_duration"){            ## maximum duration of fire	none	hours
         ncvar_put(nc=fates.param.nc, varid='max_durat',vals=pft[v])
       }
       if(var == "nfires"){            ## The number of fires initiated per m2 per year, from lightning and humans
         ncvar_put(nc=fates.param.nc, varid='nfires',vals=pft[v])
       }
       if(var == "fuel_energy"){            ## energy content of fuel [kj kg-1]
         ncvar_put(nc=fates.param.nc, varid='fuel_energy',vals=pft[v])
       }
       if(var == "fuel_particle_density"){            ## particle density of fuel [kg m-3]
         ncvar_put(nc=fates.param.nc, varid='part_dens',vals=pft[v])
       }
       if(var == "durat_slope"){            ## SPITFIRE: change in fire duration with fire danger index. from Canadian Forest Service	
         ncvar_put(nc=fates.param.nc, varid='durat_slope',vals=pft[v])
       }
       if(var == "miner_damp"){            ## SPITFIRE mineral dampening coefficient
         ncvar_put(nc=fates.param.nc, varid='miner_damp',vals=pft[v])
       }
       if(var == "fuel_minerals"){            ## mineral content of fuel
         ncvar_put(nc=fates.param.nc, varid='miner_total',vals=pft[v])
       }
       if(var == "alpha_scorch_height"){            ## SPITFIRE scorch height parameter
         ncvar_put(nc=fates.param.nc, varid='alpha_SH',vals=pft[v])
       }
       if(var == "fdi_a"){            ## SPITFIRE Constant in calculation of dewpoint for Fire Danger Index (FDI)
         ncvar_put(nc=fates.param.nc, varid='fdi_a',vals=pft[v])
       }
       if(var == "fdi_b"){            ## SPITFIRE Constant in calculation of dewpoint for Fire Danger Index (FDI)
         ncvar_put(nc=fates.param.nc, varid='fdi_b',vals=pft[v])
       }
       # fdi_alpha	SPITFIRE fire danger index (FDI) coefficient	
       if(var == ""){            ## 
         ncvar_put(nc=fates.param.nc, varid='',vals=pft[v])
       }
       ##   -- CANOPY
       if(var == "canopy_max_spread"){            ## Maximum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
         ncvar_put(nc=fates.param.nc, varid='maxspread',vals=pft[v])
       }
       # 	
       if(var == "canopy_min_spread"){            ## Minimum allowable "dynamic ratio of dbh to canopy area" for cohorts in closed canopies.	-	[cm/m2]
         ncvar_put(nc=fates.param.nc, varid='minspread',vals=pft[v])
       }

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
       if(var == "CWD_frac1"){            ##Fraction of coarse woody debris (CWD) that is moved into each of the four woody fuel classes
         ncvar_put(nc=fates.param.nc, varid='fates_CWD_frac', start = 1, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac2"){
         ncvar_put(nc=fates.param.nc, varid='fates_CWD_frac', start = 2, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac3"){
         ncvar_put(nc=fates.param.nc, varid='fates_CWD_frac', start = 3, count = 1,
                   vals=pft[v])
       }
       if(var == "CWD_frac4"){
         ncvar_put(nc=fates.param.nc, varid='fates_CWD_frac', start = 4, count = 1,
                   vals=pft[v])
       }
       
       
     } ## end loop over VARIABLES
   } ## end loop over PFTs
   #ncdf4::nc_close(param.nc)
   ncdf4::nc_close(clm.param.nc)
   ncdf4::nc_close(fates.param.nc)
   
#   ## Write SETTINGS file
#     
}
#---------------------------------------------------------------------------------------------------------------------#
### EOF
