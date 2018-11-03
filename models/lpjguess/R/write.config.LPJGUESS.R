#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a LPJ-GUESS config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LPJGUESS
##' @title Write LPJ-GUESS configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LPJ-GUESS for given run
##' @export
##' @author Istem Fer, Tony Gardella
write.config.LPJGUESS <- function(defaults, trait.values, settings, run.id) {
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  if (!file.exists(rundir)) {
    dir.create(rundir)
  }
  outdir <- file.path(settings$host$outdir, run.id)
  if (!file.exists(outdir)) {
    dir.create(outdir)
  }
  
  #-----------------------------------------------------------------------
  # write LPJ-GUESS specific instruction file
  settings <- write.insfile.LPJGUESS(settings, trait.values, rundir, outdir, run.id)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.LPJGUESS"), n = -1)
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@INSFILE@", settings$model$insfile, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.LPJGUESS

# ==================================================================================================#
#' @name write.insfile.LPJGUESS
#' @title Write LPJ-GUESS instruction script
#' @export
#' @param settings PEcAn settings list
#' @param trait.values trait.values
#' @param rundir rundir
#' @param outdir outdir
#' @param run.id PEcAn run ID
#' @return settings Updated list
#' @author Istem Fer
write.insfile.LPJGUESS <- function(settings, trait.values, rundir, outdir, run.id) {
  
  guessins  <- readLines(con = system.file("template.ins", package = "PEcAn.LPJGUESS"), n = -1)
  paramsins <- readLines(con = system.file("pecan.ins", package = "PEcAn.LPJGUESS"), n = -1)
  pftindx   <- 152:222 # should grab automatically
  pftblock  <- paramsins[pftindx] # lines with pft params
  
  # cp the grid indices file
  grid.file <- file.path(settings$host$rundir, "gridind.txt")
  gridind   <- system.file("gridind.txt", package = "PEcAn.LPJGUESS")
  system(paste("cp ", gridind, settings$rundir))
  guessins  <- gsub("@GRID_FILE@", grid.file, guessins)
  
  pft_names <- sapply(settings$pfts, `[[`,"name")
  load(system.file("lpjguess_params.Rdata",package = "PEcAn.LPJGUESS"))
  
  # name and unit conversion
  trait.values <- pecan2lpjguess(trait.values)
  
  # these are strings, should they be passed via xml?
  # e.g. defaults lifeform=tree phenology=evergreen leafphysiognomy=broadleaf landcover=natural
  noprior_params <- c("lifeform", "phenology", "leafphysiognomy", "landcover")
  
  write2pftblock <-  vector("list", length(settings$pfts))
  # write params with values from trait.values
  for (i in seq_along(settings$pfts)) {

      write2pftblock[[i]] <- pftblock
      write2pftblock[[i]] <- gsub(paste0("@pft@"), pft_names[i], write2pftblock[[i]])
      
      warning_list <- list()
      
      # pass param values
      # IMPORTANT : Ideally all params should have priors on them! Currently the defaults are only for a tropical broadleaved evergreen pft
      for(t in seq_along(lpjguess_param_list)){
        trait_name <- names(lpjguess_param_list)[t]
        if(trait_name != "pft" & !(trait_name %in% noprior_params)){
          if(trait_name %in% names(trait.values[[i]])){ # pass sample
            write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), trait.values[[i]][[trait_name]], write2pftblock[[i]])
          }else{ # use default
            write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), lpjguess_param_list[[trait_name]], write2pftblock[[i]])
            warning_list[[trait_name]] <- trait_name
          }
        }  
      }
      
      # handle the no prior params
      for(t in seq_along(noprior_params)){
        trait_name <- noprior_params[t]
        if(!is.null(settings$pfts[[i]][[trait_name]])){ # specified in xml
          write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), paste0("'", settings$pfts[[i]][[trait_name]], "'"), write2pftblock[[i]])
        }else{ #pass the default, add to warning
          write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), paste0("'", lpjguess_param_list[[trait_name]], "'"), write2pftblock[[i]])
          warning_list[[trait_name]] <- trait_name
        }
      }

      PEcAn.logger::logger.warn("***You have not specified the following parameters for your PFT,", pft_names[i],"- Be aware that the defaults may not work well for you.***", unlist(warning_list))
  } #end of pft-loop
  
  # erase the placeholder, write back the pft blocks
  paramsins <- paramsins[-pftindx] 
  paramsins <- c(paramsins, unlist(write2pftblock))
  
  
  # write clim file names
  
  tmp.file <- settings$run$inputs$met$path
  pre.file <- gsub(".tmp.nc", ".pre.nc", tmp.file)
  cld.file <- gsub(".tmp.nc", ".cld.nc", tmp.file)
  
  guessins <- gsub("@TEMP_FILE@", tmp.file, guessins)
  guessins <- gsub("@PREC_FILE@", pre.file, guessins)
  guessins <- gsub("@INSOL_FILE@", cld.file, guessins)
  
  # create and write CO2 file
  start.year <- lubridate::year(settings$run$start.date)
  end.year <- lubridate::year(settings$run$end.date)
  n.year <- length(start.year:end.year)
  co2.file <- file.path(settings$rundir, 
                        paste0("co2.", sprintf("%04d", start.year), ".", end.year, ".txt"))
  
  # for pre-industrial values just use 280 ppm
  if (end.year < 1850) {
    CO2 <- data.frame(start.year:end.year, rep(280, n.year))
  } else if (end.year < 2012) {
    data(co2.1850.2011, package = "PEcAn.LPJGUESS")
    if (start.year < 1850) {
      CO2_preind <- data.frame(year = start.year:1849, ppm = rep(280, length(start.year:1849)))
      CO2_postind <- co2.1850.2011[1:which(co2.1850.2011[, 1] == end.year), ]
      CO2 <- rbind(CO2_preind, CO2_postind)
    } else {
      CO2 <- co2.1850.2011[1:which(co2.1850.2011[, 1] == end.year), ]
    }
  } else {
    PEcAn.logger::logger.severe("End year should be < 2012 for CO2")
  }
  write.table(CO2, file = co2.file, row.names = FALSE, col.names = FALSE, sep = "\t", eol = "\n")
  guessins <- gsub("@CO2_FILE@", co2.file, guessins)
  
  # write soil file path
  soil.file <- settings$run$inputs$soil$path
  guessins <- gsub("@SOIL_FILE@", soil.file, guessins)
  
  settings$model$insfile <- file.path(settings$rundir, run.id, "guess.ins")
  
  writeLines(paramsins, con = file.path(settings$rundir, run.id, "params.ins"))
  writeLines(guessins, con = file.path(settings$rundir, run.id, "guess.ins"))
  
  return(settings)
} # write.insfile.LPJGUESS


# ==================================================================================================#
#' Function to translate pecan param names and units to lpjguess names and units
#' @export
#' @param trait.values trait.values, list
#' @return translated list
#' @author Istem Fer
pecan2lpjguess <- function(trait.values){
  
  # TODO : extend this list
  vartable <- tibble::tribble(
    ~pecanname, ~lpjguessname, ~pecanunits, ~lpjguessunits, 
    "root_turnover_rate", "turnover_root", NA, NA, 
    "sapwood_turnover_rate", "turnover_sap", NA, NA, 
    "leaf_turnover_rate", "turnover_leaf", NA, NA,
    "SLA", "sla", NA, NA)
  
  trait.values <- lapply(trait.values, function(x){
    names(x)[match(vartable$pecanname, names(x))] <- vartable$lpjguessname[vartable$pecanname %in% names(x)]
    return(x)
  })
  
  # TODO : unit conversions?
  return(trait.values)
} 
