##-------------------------------------------------------------------------------------------------#
## Functions to prepare and write out MAAT model xml files for MA, SA, and Ensemble runs
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"
##-------------------------------------------------------------------------------------------------#

##------------------------------------------------------------------------------------------------#
##' convert parameters and parameter names from PEcAn database default units/names with MAAT
##'
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.MAAT
##' @title Convert samples for MAAT
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @param runid optional parameter for debugging
##' @return matrix or dataframe with values transformed
##' @export
##' @author Shawn Serbin, Anthony Walker
convert.samples.MAAT <- function(trait.samples, runid) {
  
  ### Convert object
  if (is.list(trait.samples)) {
    trait.samples <- as.data.frame(trait.samples)
  }
  
  ### first rename variables
  trait.names <- colnames(trait.samples)
  trait.names[trait.names == "leaf_respiration_rate_m2"]    <- "atref.rd"
  trait.names[trait.names == "Vcmax"]                       <- "atref.vcmax"
  trait.names[trait.names == "Jmax"]                        <- "atref.jmax"
  trait.names[trait.names == "Ev_Arrhenius"]                <- "Ha.vcmax"   # Arrhenius activation energy
  trait.names[trait.names == "Ej_Arrhenius"]                <- "Ha.jmax"    # Arrhenius activation energy
  trait.names[trait.names == "Ha_Modified_Arrhenius_Vcmax"] <- "Ha.vcmax"   # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "Hd_Modified_Arrhenius_Vcmax"] <- "Hd.vcmax"   # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "Ha_Modified_Arrhenius_Jmax"]  <- "Ha.jmax"    # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "Hd_Modified_Arrhenius_Jmax"]  <- "Hd.jmax"    # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "cuticular_cond"]              <- "g0"         # Medlyn and ball-berry min conductance value (i.e. g0, or the intercept of A/gs relationship)
  trait.names[trait.names == "stomatal_slope"]              <- "g1_leuning"
  trait.names[trait.names == "stomatal_slope.g1"]           <- "g1_medlyn"
  trait.names[trait.names == "stomatal_slope.BB"]           <- "g1_ball"
  trait.names[trait.names == "f_frac"]                      <- "f"
  trait.names[trait.names == "theta"]                       <- "theta_j"    # curvature of J quadratic in Farqhuar & Wong 1984       (unitless)
  trait.names[trait.names == "leaf_respiration_Q10"]        <- "q10.rd"     # Q10 of Rd (unitless)
  colnames(trait.samples) <- trait.names
  
  ### Conversions -- change to only use if Collatz, should also provide standard Rd oputput
  if ("atref.rd" %in% names(trait.samples)) {
    ## Calculate dark_resp_factor - rd as a proportion of Vcmax, Williams & Flannagan 1998 ~ 0.1
    ## (unitless)
    trait.samples[["b_rdv_25"]] <- trait.samples[["atref.rd"]] / trait.samples[["atref.vcmax"]]
  }
  if ("Ha.vcmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples$Ha.vcmax <- PEcAn.utils::ud_convert(trait.samples$Ha.vcmax, "kJ", "J")
  }
  if ("Hd.vcmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples$Hd.vcmax <- PEcAn.utils::ud_convert(trait.samples$Hd.vcmax, "kJ", "J")
  }
  if ("Ha.jmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples$Ha.jmax <- PEcAn.utils::ud_convert(trait.samples$Ha.jmax, "kJ", "J")
  }
  if ("Hd.jmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples$Hd.jmax <- PEcAn.utils::ud_convert(trait.samples$Hd.jmax, "kJ", "J")
  }
  if ("leaf_reflect_vis" %in% names(trait.samples) & "leaf_trans_vis" %in% names(trait.samples) ){
    leaf_abs <- 1-(trait.samples[["leaf_reflect_vis"]]+trait.samples[["leaf_trans_vis"]])
    trait.samples[["a"]] <- leaf_abs
    remove <- which(colnames(trait.samples)=="leaf_trans_vis" | colnames(trait.samples)=="leaf_reflect_vis")
    trait.samples <- trait.samples[,-remove]
  }
  if ("leaf_width" %in% names(trait.samples)) {
    ## Convert from mm to m
    trait.samples$leaf_width <- PEcAn.utils::ud_convert(trait.samples$leaf_width, "mm", "m")
  }
  if ("g0" %in% names(trait.samples)) {
    ## Convert from umol H2O m-2 s-1 to mol m-2s-1
    trait.samples$g0 <- PEcAn.utils::ud_convert(trait.samples$g0, "umol H2O m-2 s-1", "mol H2O m-2 s-1")
  }
  
  # for debugging conversions 
  #save(trait.samples, file = file.path(settings$host$outdir,runid,'trait.samples.Rdata'))
  
  ### Return trait.samples as modified by function
  return(trait.samples)
} # convert.samples.MAAT
##-------------------------------------------------------------------------------------------------#


##-------------------------------------------------------------------------------------------------#
##' Writes a MAAT config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.MAAT
##' @title Write MAAT model configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for MAAT for given run
##' @export
##' @author Shawn Serbin, Anthony Walker, Rob Kooper, Chris Black
##'
write.config.MAAT <- function(defaults = NULL, trait.values, settings, run.id) {
  
  # function needed to nest parameters in the appropriately the output MAAT XML.  See below
  nest_entries <- function(x, pattern, new_name = pattern){ 
    matches <- grepl(pattern, names(x))
    if(!any(matches)){
      return(x)
    }
    nested <- stats::setNames(x[matches], gsub(pattern, "", names(x[matches])))
    x <- x[!matches]
    x[[new_name]] <- nested
    x
  }
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  ### Move model files to run dirs. Use built-in MAAT script setup_MAAT_project.bs --- May need to revise this with 
  ### lastest MAAT v1.0 and changes within.  This script no longer completely fits within the PEcAn logic.  May be better
  ### to manually move/link needed script files within PEcAn and not use any built-in MAAT bash scripts.
  maat_mod_obj <- as.character(settings$model$config$mod_obj)
  settings$model$config$mod_obj <- NULL  # remove from final MAAT *_user_static.xml MAAT file
  system2(file.path(settings$model$binary, "run_scripts/setup_MAAT_project.bs"), 
          c(maat_mod_obj, rundir, file.path(settings$model$binary, "run_scripts"), 
            file.path(settings$model$binary, "src")))
  # remove leaf_user_dynamic.xml from rundir since PEcAn is not currently using dynamic variables (for now, revist later as-needed)
  # see: https://github.com/walkeranthonyp/MAAT/issues/8 for reference
  unlink(file.path(rundir,"leaf_user_dynamic.xml"), recursive = FALSE)

  # remove leaf_user_met.xml file if running without met drivers. Look for this file during model2netCDF step to select processing path
  if (is.null(settings$run$inputs$met)) {
    unlink(file.path(rundir,"leaf_user_met.xml"), recursive = FALSE)
  }

  # below is now required given that MAAT logic no longer moves or links to the run_MAAT.R script file
  run_maat_script <- file.path(settings$model$binary, "src", "run_MAAT.R")
  
  ### Parse config options to XML
  if (!is.null(settings$model$config$mod_mimic)) {
    PEcAn.logger::logger.info(paste0("Running with model mimic: ",settings$model$config$mod_mimic))
    mod_mimic <- as.character(settings$model$config$mod_mimic)
    settings$model$config$mod_mimic <- NULL
    xml <- PEcAn.settings::listToXml(settings$model$config, "default")
  } else {
    PEcAn.logger::logger.info("*** Model mimic not selected ***")
    mod_mimic <- 'NULL'
    xml <- PEcAn.settings::listToXml(settings$model$config, "default")
  }
  
  ### Run rename and conversion function on PEcAn trait values
  PEcAn.logger::logger.info("*** Convert input trait values to MAAT parameters and units ***")
  traits <- convert.samples.MAAT(trait.samples = trait.values[[settings$pfts$pft$name]],runid=run.id)
  # below for debugging
  #save(traits, file = file.path(settings$host$outdir,run.id,'trait.samples.converted.Rdata'))
  
  ### Convert traits to list
  # with MAAT v1.0 we need to generate nested lists
  # create full nested list and convert to MAAT XML format
  traits <- as.list(traits)
  traits.list <- list()
  maat_param_prefix_list <- list(param=c("Ha.","Hd.","atref.","reftemp.","Topt.","deltaS.","a_deltaS_t.","b_deltaS_t.","q10.","a_q10_t.",
                                         "b_q10_t.","tupp_cox.","tlow_cox.","exp_cox."),
                                 xml=c("Ha","Hd","atref","reftemp","Topt","deltaS","a_deltaS_t","b_deltaS_t","q10","a_q10_t",
                                       "b_q10_t","tupp_cox","tlow_cox","exp_cox"))
  q <- 1
  for (p in seq(seq_along(1:length(maat_param_prefix_list$param)))) {
    if (q==1) {
      traits.list <- nest_entries(traits, paste0(maat_param_prefix_list$param[p]), paste0(maat_param_prefix_list$xml[p]))
    } else {
      traits.list <- nest_entries(traits.list, paste0(maat_param_prefix_list$param[p]), paste0(maat_param_prefix_list$xml[p]))
    }
    q <- q+1
  }
  traits.xml <- PEcAn.settings::listToXml(traits.list, "pars")
  rm(p,q)
  
  ### Finalize XML
  xml[[1]] <- XML::addChildren(xml[[1]], traits.xml)
  
  ### Save final XML stack as a properly formatted MAAT parameter/option XML file
  XML::saveXML(xml, 
          file = file.path(settings$rundir, run.id, "leaf_user_static.xml"), 
          indent = TRUE, 
          prefix = PREFIX_XML)
  
  ### Setup job.sh script to run MAAT model
  if (is.null(settings$run$inputs$met)) {
    PEcAn.logger::logger.info("-- No met selected. Running without a met driver --")
    jobsh <- paste0("#!/bin/bash\n","Rscript ",run_maat_script," ",
                    "\"srcdir <- ","'",file.path(settings$model$binary, "src"),"'","\""," ",
                    "\"pdir <- ","'",rundir,"'","\""," ","\"mod_obj <- ","'",maat_mod_obj,"'","\""," ",
                    "\"xml<-T","\""," ","\"uq<-F","\""," ",
                    "\"factorial<-F","\""," ","\"mod_mimic<-",mod_mimic,"\""," ",
                    "\"odir <- ","'",outdir,"'","\""," > ",rundir,
                    "/logfile.txt","\n",'echo "',
                    ' library(PEcAn.MAAT); model2netcdf.MAAT(',
                    "'",rundir,"',","'",outdir,"',",
                    settings$run$site$lat,",",
                    settings$run$site$lon,", '",
                    settings$run$start.date,"', '",
                    settings$run$end.date,"') ",
                    '" | R --vanilla')
    
    # Run with met drivers 
  } else if (!is.null(settings$run$inputs$met)) {
    
    ## temporary fix for #2064
    #met.dir <- dirname(settings$run$inputs$met$path)
    met.dir <- dirname(as.character(settings$run$inputs$met$path))
    #met.file <- basename(settings$run$inputs$met$path)
    met.file <- basename(as.character(settings$run$inputs$met$path))
    
    file.copy(file.path(met.dir, list.files(met.dir, "*.xml")), 
              rundir, 
              overwrite = TRUE, 
              recursive = FALSE, 
              copy.mode = TRUE, 
              copy.date = TRUE)
    
    PEcAn.logger::logger.info("-- Met selected. Running with a met driver --")
    PEcAn.logger::logger.info(paste0("Running with met: ",met.file))
    jobsh <- paste0("#!/bin/bash\n","Rscript ",run_maat_script," ",
                    "\"srcdir <- ","'",file.path(settings$model$binary, "src"),"'","\""," ",
                    "\"pdir <- ","'",rundir,"'","\""," ","\"mod_obj <- ","'",maat_mod_obj,"'","\""," ",
                    "\"xml<-T","\""," ","\"uq<-F","\""," ",
                    "\"factorial<-F","\""," ","\"mod_mimic<-",mod_mimic,"\""," ",
                    "\"odir <- ","'",outdir,"'","\""," ","\"mdir <- ","'",met.dir,"'",
                    "\""," ","\"metdata <- ","'",met.file,"'","\""," > ",rundir,
                    "/logfile.txt","\n",'echo "',
                    ' library(PEcAn.MAAT); model2netcdf.MAAT(',
                    "'",rundir,"',","'",outdir,"',",
                    settings$run$site$lat,",",
                    settings$run$site$lon,", '",
                    settings$run$start.date,"', '",
                    settings$run$end.date,"') ",
                    '" | R --vanilla')
  }  #End if/else
  
  # Write the job.sh script
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
} # write.config.MAAT
##-------------------------------------------------------------------------------------------------#
## EOF
