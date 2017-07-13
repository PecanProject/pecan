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
  
  return(trait.samples)
} # convert.samples.DALEC

####function to split root_carbon_content into fine and coarse roots by rtsize dimension at the .002 m threshold
partition_roots <- function(roots, rtsize){
  if(length(rtsize) > 1 && length(rtsize) == length(roots)){
    threshold <- .002
    epsilon <- .0005
    rtsize_thresh_idx <- which.min(sapply(rtsize-threshold,abs))
    rtsize_thresh <- rtsize[rtsize_thresh_idx]
    if(abs(rtsize_thresh-threshold) > epsilon){
      PEcAn.utils::logger.error(paste("Closest rtsize to fine root threshold of", threshold, "m (", rtsize_thresh, 
                                      ") is greater than", epsilon, 
                                      "m off; fine roots can't be partitioned. Please improve rtsize dimensions or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf."))
      return(NULL)
    } else{
      fine.roots.temp <- sum(roots[1:rtsize_thresh_idx-1])
      coarse.roots.temp <- sum(roots) - fine.roots.temp
      if(fine.roots.temp >= 0 && coarse.roots.temp >= 0){
        fine.roots <- fine.roots.temp
        coarse.roots <- coarse.roots.temp
        PEcAn.utils::logger.info("Using partitioned root values", fine.roots, "for fine and", coarse.roots, "for coarse.")
        return(list(fine.roots = fine.roots, coarse.roots = coarse.roots))
      } else{
        PEcAn.utils::logger.error("Roots could not be partitioned (fine or coarse is less than 0); please provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        return(NULL)
      }
    }
  } else {
    PEcAn.utils::logger.error("Not enough levels of rtsize associated with root_carbon_content to partition roots; please provide finer resolution for root_carbon_content or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
    return(NULL)
  }
}

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
  
  #function to check that ncvar was present (numeric) and a valid value was given it (not NA or negative)
  is.valid <- function(var){
    return(all(is.numeric(var) && !is.na(var) &&  var >= 0)) 
  }
  
  #default.param <- read.table(system.file("default_param.dalec", package = "PEcAn.DALEC"))
  IC.params <- data.frame()
  if (!is.null(settings$run$inputs$poolinitcond$path)) {
    IC.path <- settings$run$inputs$poolinitcond$path
    IC.nc <- try(ncdf4::nc_open(IC.path)) 
    
    if(class(IC.nc) != "try-error"){
      #check/load biomass netcdf variables
      TotLivBiom <- try(ncdf4::ncvar_get(IC.nc,"TotLivBiom"),silent = TRUE)
      leaf <- try(ncdf4::ncvar_get(IC.nc,"leaf_carbon_content"),silent = TRUE)
      AbvGrndWood <- try(ncdf4::ncvar_get(IC.nc,"AbvGrndWood"),silent = TRUE)
      roots <- try(ncdf4::ncvar_get(IC.nc,"root_carbon_content"),silent = TRUE)
      fine.roots <- try(ncdf4::ncvar_get(IC.nc,"fine_root_carbon_content"),silent = TRUE)
      coarse.roots <- try(ncdf4::ncvar_get(IC.nc,"coarse_root_carbon_content"),silent = TRUE)
      
      
      #check if total roots are partitioned (pull out as a function for readability)
      #note: if roots are patritionable, they will override fine_ and/or coarse_root_carbon_content if loaded
      if(is.valid(roots)){
        if("rtsize" %in% names(IC.nc$dim)){
          rtsize <- IC.nc$dim$rtsize$vals
          part_roots <- partition_roots(roots, rtsize)
          if(!is.null(part_roots)){
            fine.roots <- part_roots$fine.roots
            coarse.roots <- part_roots$coarse.roots
          } else{
            #couldn't partition roots; error messages handled by function
          }
        } else{
          PEcAn.utils::logger.error("Please provide rtsize dimension with root_carbon_content to allow partitioning or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        }
      } else{
        #proceed without error message
      }
     
    ###write initial conditions from netcdf (wherever valid input isn't available, DALEC default remains)
      # cf0 initial canopy foliar carbon (g/m2)
      if (is.valid(leaf)) {
        IC.params[["cf0"]] <- leaf * 1000 #standard kg C m-2
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
              is.valid(fine.roots) && is.valid(coarse.roots)){
          leaf <- (TotLivBiom - AbvGrndWood - fine.roots - coarse.roots) * 1000 #standard kg C m-2
          if(leaf >= 0){
            IC.params[["cf0"]] <- leaf
          } else{
            PEcAn.utils::logger.error("TotLivBiom is less than sum of AbvGrndWood and roots; using default for leaf biomass")
          }
      }
    
      # cw0 initial pool of woody carbon (g/m2)
      if (is.valid(AbvGrndWood)) {
        if(is.valid(coarse.roots)){
          IC.params[["cw0"]] <- (AbvGrndWood + coarse.roots) * 1000 #standard kg C m-2
        }
        else if (is.valid(TotLivBiom) && is.valid(leaf) && is.valid(fine.roots)){
          wood <- (TotLivBiom - leaf - fine.roots) * 1000 #standard kg C m-2
          if (wood >= 0){
            IC.params[["cw0"]] <- wood
          } else{
            PEcAn.utils::logger.error("TotLivBiom is less than sum of leaf and fine roots; using default for woody biomass")
          }
        } else{
          PEcAn.utils::logger.error("write.configs.DALEC IC can't calculate total woody biomass with only AbvGrndWood; using defaults. Please provide coarse_root_carbon_content OR root_carbon_content with rtsize dimensions OR leaf_carbon_content, fine_root_carbon_content, and TotLivBiom in netcdf")
        }
      } else if (is.valid(TotLivBiom) && is.valid(leaf) && is.valid(fine.roots)){
        wood <- (TotLivBiom - leaf - fine.roots) * 1000 #standard kg C m-2
        if (wood >= 0){
          IC.params[["cw0"]] <- wood
        }else{
          PEcAn.utils::logger.error("TotLivBiom is less than sum of leaf and fine roots; using default for woody biomass")
        }
      } 
    
      # cr0 initial pool of fine root carbon (g/m2)
      if (is.valid(fine.roots)) {
        IC.params[["cr0"]] <- fine.roots * 1000 #standard kg C m-2
      }
    
      # cl0 initial pool of litter carbon (g/m2)
      litter <- try(ncdf4::ncvar_get(IC.nc,"litter_carbon_content"),silent = TRUE)
      if (is.valid(litter)) {
        IC.params[["cl0"]] <- litter * 1000 #standard kg C m-2
      }
        
      # cs0 initial pool of soil organic matter and woody debris carbon (g/m2)
      soil <- try(ncdf4::ncvar_get(IC.nc,"soil_organic_carbon_content"),silent = TRUE)
      wood.debris <- try(ncdf4::ncvar_get(IC.nc,"wood_debris_carbon_content"),silent = TRUE)
      if(is.valid(soil) && is.valid(wood.debris)){
        IC.params[["cs0"]] <- (soil + sum(wood.debris)) * 1000 #standard kg C m-2
      } else if(!is.valid(soil) && is.valid(wood.debris)){
        soil <- try(ncdf4::ncvar_get(IC.nc,"soil_carbon_content"),silent = TRUE)
        if(is.valid(soil)){
            IC.params[["cs0"]] <- (soil + sum(wood.debris)) * 1000 #standard kg C m-2
        } else{
          PEcAn.utils::logger.error("write.configs.DALEC IC can't calculate soil matter pool without soil carbon; using default. Please provide soil_organic_carbon_content in netcdf.")
        }
      } else if(is.valid(soil) && !is.valid(wood.debris)){
        PEcAn.utils::logger.error("write.configs.DALEC IC can't calculate soil matter pool without wood debris; using default. Please provide wood_debris_carbon_content in netcdf.")
      } 
      
      ###Write to command line file
      PEcAn.utils::logger.info(names(paste("Adding IC tags to file:", IC.params))
      for (i in seq_along(IC.params)) {
        cmdFlags <- paste0(cmdFlags, " -", names(IC.params)[i], " ", IC.params[[i]])
      }
      
    } else{
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
