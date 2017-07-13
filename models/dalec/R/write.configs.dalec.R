#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
# Template for functions to prepare and write out files model-specific configuration files for MA
#--------------------------------------------------------------------------------------------------#
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"

convert.samples.DALEC <- function(trait.samples) {
  
  DEFAULT.LEAF.C <- 0.48
  ## convert SLA from m2 / kg leaf to m2 / kg C
  
  if ("SLA" %in% names(trait.samples)) {
    trait.samples[["SLA"]] <- trait.samples[["SLA"]]/DEFAULT.LEAF.C/1000
  }
  
  # t1 rate variable controling decomposition from litter to soil organinc matter [day-1, ref T
  # 10C]
  if ("litter_decomposition_to_SOM" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "litter_decomposition_to_SOM")] <- "t1"
  }
  
  # t2 proportion of GPP lost to autotrophic respiration
  if ("autotrophic_respiration_fraction" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "autotrophic_respiration_fraction")] <- "t2"
  }
  
  # t3 proportion of NPP allocated to foliage
  if ("leaf_allocation_fraction" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "leaf_allocation_fraction")] <- "t3"
  }
  
  # t4 proportion of NPP allocated to roots
  if ("root_allocation_fraction" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "root_allocation_fraction")] <- "t4"
  }
  
  # t5 proportion of foliage becoming litter every time step
  if ("leaf_turnover_rate" %in% names(trait.samples)) {
    trait.samples[["leaf_turnover_rate"]] <- trait.samples[["leaf_turnover_rate"]]/365
    names(trait.samples)[which(names(trait.samples) == "leaf_turnover_rate")] <- "t5"
  }
  
  # t6 proportion of woody material becoming woody debris every time step
  if ("wood_turnover_rate" %in% names(trait.samples)) {
    trait.samples[["wood_turnover_rate"]] <- trait.samples[["wood_turnover_rate"]]/365
    names(trait.samples)[which(names(trait.samples) == "wood_turnover_rate")] <- "t6"
  }
  
  # t7 proportion of fine roots becoming soil/woody debris every time step
  if ("root_turnover_rate" %in% names(trait.samples)) {
    trait.samples[["root_turnover_rate"]] <- trait.samples[["root_turnover_rate"]]/365
    names(trait.samples)[which(names(trait.samples) == "root_turnover_rate")] <- "t7"
  }
  
  # t8 rate variable controlling respiration from litter [day-1, ref T 10C]
  if ("litter_respiration_rate" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "litter_respiration_rate")] <- "t8"
  }
  
  # t9 rate variable controlling respiration from soil organic matter and woody debris [day-1, ref
  # T 10C]
  if ("som_respiration_rate" %in% names(trait.samples)) {
    names(trait.samples)[which(names(trait.samples) == "som_respiration_rate")] <- "t9"
  }
  
  ### INITIAL CONDITIONS
  
  # cf0 initial canopy foliar carbon (g/m2)
  # cw0 initial pool of woody carbon (g/m2)
  # cr0 initial pool of fine root carbon (g/m2)
  # cl0 initial pool of litter carbon (g/m2)
  # cs0 initial pool of soil organic matter and woody debris carbon (g/m2)
  
  return(trait.samples)
} # convert.samples.DALEC

#--------------------------------------------------------------------------------------------------#
##' Writes a configuration files for your model
#--------------------------------------------------------------------------------------------------#
##' write Dalec Configuration files
##'
##' @title write.config.DALEC 
##' @param defaults 
##' @param trait.values 
##' @param settings 
##' @param run.id 
##' @return configuration files
##' @export write.config.DALEC
write.config.DALEC <- function(defaults, trait.values, settings, run.id) {
  
  ### CONVERT PARAMETERS
  cmdFlags <- ""
  for (group in names(trait.values)) {
    if (group == "env") {
      
      ## set defaults from config.header
      
    } else {
      if (!is.null(trait.values[[group]])) {
        params <- convert.samples.DALEC(trait.values[[group]])
        logger.info(names(params))
        for (i in seq_along(params)) {
          cmdFlags <- paste0(cmdFlags, " -", names(params)[i], " ", params[[i]])
        }
      }
    }
  }
  
  ### INITIAL CONDITIONS
  is.loaded <- function(var){
    return(all(!is.na(var) && is.numeric(var))) #check that ncvar was present (numeric) and a value was given it (not NA)
  }
  
  default.param <- read.table(system.file("default_param.dalec", package = "PEcAn.DALEC"))
  IC.param <- data.frame()
   if (!is.null(settings$run$inputs$poolinitcond$path)) {
    IC.path <- settings$run$inputs$poolinitcond$path
    IC.nc <- try(ncdf4::nc_open(IC.path)) 
    
    if(class(IC.nc) != "try-error"){
      #check/load biomass netcdf variables
      totBiom <- try(ncdf4::ncvar_get(IC.nc,"TotLivBiom"),silent = TRUE)
      leaf <- try(ncdf4::ncvar_get(IC.nc,"leaf_carbon_content"),silent = TRUE)
      AbvGrndWood <- try(ncdf4::ncvar_get(IC.nc,"AbvGrndWood"),silent = TRUE)
      roots <- try(ncdf4::ncvar_get(IC.nc,"root_carbon_content"),silent = TRUE)
      fine.roots <- try(ncdf4::ncvar_get(IC.nc,"fine_root_carbon_content"),silent = TRUE)
      coarse.roots <- try(ncdf4::ncvar_get(IC.nc,"coarse_root_carbon_content"),silent = TRUE)
      
      
      #check if total roots are partitioned
      if(is.loaded(roots) && !is.loaded(fine.roots) || !is.loaded(coarse.roots)){
        if("rtsize" %in% names(IC.nc$dim)){
          rtsize = IC.nc$dim$rtsize$vals
          if(length(rtsize) > 1 && length(rtsize) == length(roots)){
            threshold = .002
            epsilon <- .0005
            rtsize_thresh_idx = which.min(sapply(rtsize-threshold,abs))
            rtsize_thresh = rtsize[rtsize_thresh_idx]
            if(abs(rtsize_thresh-threshold) > epsilon){
              PEcAn.utils::logger.error(paste("Closest rtsize to fine root threshold of", threshold, "m (", rtsize_thresh, 
                                              ") is greater than", epsilon, 
                                              "m off; fine roots can't be partitioned. Please improve rtsize dimensions or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf."))
            } 
            else{
              fine.roots.temp <- sum(roots[1:rtsize_thresh_idx-1])
              coarse.roots.temp <- sum(roots) - fine.roots
              if(fine.roots.temp > 0 && coarse.roots.temp > 0){
                fine.roots <- fine.roots.temp
                coarse.roots <- coarse.roots.temp
              } else{
                PEcAn.utils::logger.error("Roots could not be partitioned (fine or coarse is less than 0); please provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
              }
            }
          } else {
            PEcAn.utils::logger.error("Not enough levels of rtsize to partition roots; please provide finer resolution for root_carbon_content or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
          }
        } else{
          PEcAn.utils::logger.error("Please provide rtsize dimension with root_carbon_content to allow partitioning or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        }
      }
     
      
      # cf0 initial canopy foliar carbon (g/m2)
      if (is.loaded(leaf)) {
        param[["cf0"]] <- leaf
      }
      else if(is.loaded(totBiom) && is.loaded(AbvGrndWood) && 
              is.loaded(fine.roots) && is.loaded(coarse.roots)){
          leaf <- totBiom - AbvGrndWood - fine.roots - coarse.roots
        }
      }
      # cw0 initial pool of woody carbon (g/m2)
      if (is.loaded(AbvGrndWood)) {
        if(is.loaded(fine.roots) && is.loaded(coarse.roots)){
          #wood <- partitioned coarse roots + abvgroundwood
        }
        else{
          #wood <- (roots-default.fine) + abvgroundwood
        }
        param[["cw0"]] <- wood
      }
      # cr0 initial pool of fine root carbon (g/m2)
        roots <- try(ncdf4::ncvar_get(IC.nc,"root_carbon_content"),silent = TRUE)
        if (!is.na(roots) && is.numeric(roots)) {
          #partition fine roots
          param[["cr0"]] <- roots
        }
      # cl0 initial pool of litter carbon (g/m2)
        litter <- try(ncdf4::ncvar_get(IC.nc,"litter_carbon_content"),silent = TRUE)
        if (!is.na(litter) && is.numeric(litter)) {
          param[["cl0"]] <- litter
        }
      # cs0 initial pool of soil organic matter and woody debris carbon (g/m2)
        soil <- try(ncdf4::ncvar_get(IC.nc,"soil_organic_carbon_content"),silent = TRUE)
        if(!is.numeric(soil)){
          soil <- try(ncdf4::ncvar_get(IC.nc,"soil_carbon_content"),silent = TRUE)
          if(is.numeric(soil)){
            wood <- try(ncdf4::ncvar_get(IC.nc,"wood_debris_carbon_content"),silent = TRUE)
            if(is.numeric(wood)){
              soil_and_wood <- soil + sum(wood)
              param[["cs0"]] <- soil_and_wood
            }
          }
        }
    }
    else{
      PEcAn.utils::logger.error("Bad initial conditions filepath; kept defaults")
    }
  }
 
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, as.character(run.id))
  outdir <- file.path(settings$host$outdir, as.character(run.id))
  if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  
  ### WRITE PARAMETERS
  config.file.name <- paste0("CONFIG.", run.id)
  writeLines(cmdFlags, con = file.path(rundir, config.file.name))
  
  ### WRITE JOB.SH
  jobsh <- paste0("#!/bin/bash\n", 
                  settings$model$binary, 
                  " $(cat ", rundir, "/", config.file.name, 
                  ") < ", as.character(settings$run$inputs$met$path), " > ", 
                  outdir, "/out.txt\n", 
                  # 'echo ".libPaths(',"'~/R/library');",
                  "echo \"", 
                  " library(PEcAn.DALEC); model2netcdf.DALEC(", "'", 
                  outdir, "',", 
                  settings$run$site$lat, ",", 
                  settings$run$site$lon, ", '", 
                  settings$run$start.date, "', '", 
                  settings$run$end.date, "') ", 
                  "\" | R --vanilla")
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ### Display info to the console.
  print(run.id)
} # write.config.DALEC
# ==================================================================================================#

remove.config.DALEC <- function(outdir, settings) {
  
} # remove.config.DALEC
