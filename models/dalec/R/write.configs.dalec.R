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
  ## convert SLA from m2 / kg leaf to m2 / g C
  
  if ("SLA" %in% names(trait.samples)) {
    trait.samples[["SLA"]] <- trait.samples[["SLA"]]/DEFAULT.LEAF.C/1000
  }
  
  # t1 rate variable controlling decomposition from litter to soil organinc matter [day-1, ref T
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
        PEcAn.logger::logger.info(names(params))
        for (i in seq_along(params)) {
          cmdFlags <- paste0(cmdFlags, " -", names(params)[i], " ", params[[i]])
        }
      }
    }
  }
  
  ### INITIAL CONDITIONS
  
  #function to check that ncvar was loaded (numeric) and has a valid value (not NA or negative)
  is.valid <- function(var){
    return(all(is.numeric(var) && !is.na(var) &&  var >= 0)) 
  }
  
  default.param <- read.table(system.file("default_param.dalec", package = "PEcAn.DALEC"), header = TRUE)
  IC.params <- list()
 
   if (!is.null(settings$run$inputs$poolinitcond$path)) {
    IC.path <- settings$run$inputs$poolinitcond$path
    IC.nc <- try(ncdf4::nc_open(IC.path)) 
    
    if(class(IC.nc) != "try-error"){
      #check/load biomass netcdf variables
      TotLivBiom <- try(ncdf4::ncvar_get(IC.nc,"TotLivBiom"),silent = TRUE)
      leaf <- try(ncdf4::ncvar_get(IC.nc,"leaf_carbon_content"),silent = TRUE)
      LAI <- try(ncdf4::ncvar_get(IC.nc,"LAI"),silent = TRUE)
      AbvGrndWood <- try(ncdf4::ncvar_get(IC.nc,"AbvGrndWood"),silent = TRUE)
      roots <- try(ncdf4::ncvar_get(IC.nc,"root_carbon_content"),silent = TRUE)
      fine.roots <- try(ncdf4::ncvar_get(IC.nc,"fine_root_carbon_content"),silent = TRUE)
      coarse.roots <- try(ncdf4::ncvar_get(IC.nc,"coarse_root_carbon_content"),silent = TRUE)
      
      if(!all(sapply(c(TotLivBiom,leaf,LAI,AbvGrndWood,roots,fine.roots,coarse.roots),is.numeric))){
        PEcAn.logger::logger.info("DALEC IC: Any missing vars will be calculated from those provided or replaced by DALEC's defaults")
      }
      
      #check if total roots are partitionable
      #note: if roots are patritionable, they will override fine_ and/or coarse_root_carbon_content if loaded
      if(is.valid(roots)){
        if("rtsize" %in% names(IC.nc$dim)){
          PEcAn.logger::logger.info("DALEC IC: Attempting to partition root_carbon_content")
          rtsize <- IC.nc$dim$rtsize$vals
          part_roots <- PEcAn.data.land::partition_roots(roots, rtsize)
          if(!is.null(part_roots)){
            fine.roots <- part_roots$fine.roots
            coarse.roots <- part_roots$coarse.roots
          } else{
            PEcAn.logger::logger.error("DALEC IC: could not partition roots; please provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
          }
        } else{
          PEcAn.logger::logger.error("DALEC IC: Please provide rtsize dimension with root_carbon_content to allow partitioning or provide fine_root_carbon_content and coarse_root_carbon_content in netcdf.")
        }
      } else{
        #proceed without error message
      }
     
     
    ###Write initial conditions from netcdf (Note: wherever valid input isn't available, DALEC default remains)
      
      # cf0 initial canopy foliar carbon (g/m2)
      if (is.valid(leaf)) {
        IC.params[["cf0"]] <- leaf * 1000 #from standard kg C m-2
      } else if(is.valid(LAI)){
        if("SLA" %in% names(params)){
          SLA <- 1/params[1,"SLA"] #SLA converted to m2/gC in convert.samples
          leaf <- LAI * SLA
          IC.params[["cf0"]] <- leaf
        } else{
          SLA <- default.param[which(default.param$cmdFlag == "SLA"),"val"] 
          leaf <- LAI * 1/SLA #check that leaf isn't higher than total biomass if given?
          IC.params[["cf0"]] <- leaf
        }
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
              is.valid(fine.roots) && is.valid(coarse.roots)){
          leaf <- (TotLivBiom - AbvGrndWood - fine.roots - coarse.roots) * 1000 #from standard kg C m-2
          if(leaf >= 0){
            IC.params[["cf0"]] <- leaf
          } else{
            PEcAn.logger::logger.error("TotLivBiom is less than sum of AbvGrndWood and roots; using default for leaf biomass")
          }
      }
    
      # cw0 initial pool of woody carbon (g/m2)
      if (is.valid(AbvGrndWood)) {
        if(is.valid(coarse.roots)){
          IC.params[["cw0"]] <- (AbvGrndWood + coarse.roots) * 1000 #from standard kg C m-2
        } else{
          PEcAn.logger::logger.error("write.configs.DALEC IC can't calculate total woody biomass with only AbvGrndWood; checking for total biomass.")
        }
      } else if (is.valid(TotLivBiom) && is.valid(leaf) && is.valid(fine.roots)){
        if(is.valid(LAI)){
          wood <- (1000*(TotLivBiom - fine.roots)) - leaf #convert TotLivBiom and fine.roots to g C m-2 from standard kg C m-2; leaf already converted via SLA
        }
        else{
          wood <- (TotLivBiom - leaf - fine.roots) * 1000 #from standard kg C m-2
        }
        if (wood >= 0){
          IC.params[["cw0"]] <- wood
        }else{
          PEcAn.logger::logger.error(paste("TotLivBiom (", TotLivBiom, ") is less than sum of leaf (", leaf, ") and fine roots(",fine.roots,"); using default for woody biomass."))
        }
      } else{
        PEcAn.logger::logger.error("write.configs.DALEC IC could not calculate woody biomass; using defaults. Please provide AbvGrndWood and coarse_root_carbon OR leaf_carbon_content/LAI, fine_root_carbon_content, and TotLivBiom in netcdf.")
      }
    
      # cr0 initial pool of fine root carbon (g/m2)
      if (is.valid(fine.roots)) {
        IC.params[["cr0"]] <- fine.roots * 1000 #from standard kg C m-2
      } else if(is.valid(TotLivBiom) && is.valid(AbvGrndWood) && 
                is.valid(leaf) && is.valid(coarse.roots)){
        if(is.valid(LAI)){
          fine.roots <- ((TotLivBiom - AbvGrndWood - coarse.roots) * 1000) - leaf #from standard kg C m-2; leaf already converted
        }else{
          fine.roots <- (TotLivBiom - AbvGrndWood - leaf - coarse.roots) * 1000 #from standard kg C m-2
        }
        if(fine.roots >= 0){
          IC.params[["cr0"]] <- fine.roots
        } else{
          PEcAn.logger::logger.error("TotLivBiom is less than sum of AbvGrndWood, coarse roots, and leaf; using default for fine.roots biomass")
        }
      }
    
      ###non-living variables
      # cl0 initial pool of litter carbon (g/m2)
      litter <- try(ncdf4::ncvar_get(IC.nc,"litter_carbon_content"),silent = TRUE)
      if (is.valid(litter)) {
        IC.params[["cl0"]] <- litter * 1000 #from standard kg C m-2
      }
        
      # cs0 initial pool of soil organic matter and woody debris carbon (g/m2)
      soil <- try(ncdf4::ncvar_get(IC.nc,"soil_organic_carbon_content"),silent = TRUE)
      wood.debris <- try(ncdf4::ncvar_get(IC.nc,"wood_debris_carbon_content"),silent = TRUE)
      
      if(is.valid(soil) && is.valid(wood.debris)){
        IC.params[["cs0"]] <- (soil + sum(wood.debris)) * 1000 #from standard kg C m-2
      } else if(!is.valid(soil) && is.valid(wood.debris)){
        soil <- try(ncdf4::ncvar_get(IC.nc,"soil_carbon_content"),silent = TRUE)
        if(is.valid(soil)){
            IC.params[["cs0"]] <- (soil + sum(wood.debris)) * 1000 #from standard kg C m-2
        } else{
          PEcAn.logger::logger.error("write.configs.DALEC IC can't calculate soil matter pool without soil carbon; using default. Please provide soil_organic_carbon_content in netcdf.")
        }
      } else if(is.valid(soil) && !is.valid(wood.debris)){
        IC.params[["cs0"]] <- soil * 1000 #from standard kg C m-2
        PEcAn.logger::logger.warn("write.configs.DALEC IC: Loading soil carbon pool without woody debris.")
      } 
      
      ###Write to command line file
      for (i in seq_along(IC.params)) {
        cmdFlags <- paste0(cmdFlags, " -", names(IC.params)[i], " ", IC.params[[i]])
      }
      PEcAn.logger::logger.info(paste("All command flags:",cmdFlags))
      
    } else{
      PEcAn.logger::logger.error("Bad initial conditions filepath; kept defaults")
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
