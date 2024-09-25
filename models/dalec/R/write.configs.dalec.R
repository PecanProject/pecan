#--------------------------------------------------------------------------------------------------#
# Template for functions to prepare and write out files model-specific configuration files for MA
#--------------------------------------------------------------------------------------------------#
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"

convert.samples.DALEC <- function(trait.samples) {

  DEFAULT.LEAF.C <- 0.48
  ## convert SLA from PEcAn m2 / kg leaf to m2 / g C

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
##' @param defaults ignored
##' @param trait.values vector of samples for a given trait
##' @param settings a PEcAn settings object
##' @param run.id Unique identifier for the run,
##'  used here to construct output directories and filenames.
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
  IC.params <- list()

  if(!is.null(settings$run$inputs$poolinitcond$path)) {
    IC.path <- settings$run$inputs$poolinitcond$path
    
    #grab SLA from parameters and convert to PECAN standard
    sla <- NULL
    if("SLA" %in% names(params)){
      sla <- PEcAn.utils::ud_convert(params[1,"SLA"], 'm2 g-1', 'm2 kg-1') #convert SLA to m2/kgC from m2/gC (revert convert.samples conversion to dalec default; need standard for prepare.pools)
    } else{
      default.param <- utils::read.table(system.file("default_param.dalec", package = "PEcAn.DALEC"), header = TRUE)
      sla <- PEcAn.utils::ud_convert(default.param[which(default.param$cmdFlag == "SLA"),"val"], 'm2 g-1', 'm2 kg-1') #convert SLA to m2/kgC from m2/gC (dalec default)
    }

    IC.pools <- PEcAn.data.land::prepare_pools(IC.path, constants = list(sla = sla))

    if(!is.null(IC.pools)){
      ###Write initial conditions from netcdf (Note: wherever valid input isn't available, DALEC default remains)

      # cf0 initial canopy foliar carbon (g/m2)
      if ("leaf" %in% names(IC.pools)) {
        IC.params[["cf0"]] <- PEcAn.utils::ud_convert(IC.pools$leaf, 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
      }

      # cw0 initial pool of woody carbon (g/m2)
      if ("wood" %in% names(IC.pools)) {
        IC.params[["cw0"]] <- PEcAn.utils::ud_convert(IC.pools$wood, 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
      }

      # cr0 initial pool of fine root carbon (g/m2)
      if ("fine.roots" %in% names(IC.pools)) {
        IC.params[["cr0"]] <- PEcAn.utils::ud_convert(IC.pools$fine.roots, 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
      }

      ###non-living variables
      # cl0 initial pool of litter carbon (g/m2)
      if ("litter" %in% names(IC.pools)) {
        IC.params[["cl0"]] <- PEcAn.utils::ud_convert(IC.pools$litter, 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
      }

      # cs0 initial pool of soil organic matter and woody debris carbon (g/m2)
      if("soil" %in%  names(IC.pools)){
        if("wood.debris" %in%  names(IC.pools)){
          IC.params[["cs0"]] <- PEcAn.utils::ud_convert(IC.pools$soil + sum(IC.pools$wood.debris), 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
        } else {
          IC.params[["cs0"]] <- PEcAn.utils::ud_convert(IC.pools$soil, 'kg m-2', 'g m-2') #from PEcAn standard kg C m-2
          PEcAn.logger::logger.warn("write.configs.DALEC IC: Loading soil carbon pool without woody debris.")
        }
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
