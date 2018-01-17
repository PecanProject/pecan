#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------------#
##' convert parameters, do unit conversions and update parameter names from PEcAn database default
##' to units/names within dvmdostem
##'
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##'
##' @name convert.samples.dvmdostem
##' @title Convert samples for dvmdostem
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @export
##' @author Shawn Serbin, Tobey Carman
##' @importFrom udunits2 ud.convert

convert.samples.dvmdostem <- function(trait_values) {

  if("SLA" %in% names(trait_values)) {
    # Convert from m2 / kg to m2 / g
    trait_values[["SLA"]] <- trait_values[["SLA"]] / 1000.0
  }
  if ("cuticular_cond" %in% names(trait_values)) {
    # Convert from umol H2O m-2 s-1 to ???
    # Original values in dvmdostem param files not making sense, no good
    # comments as to units. This conversion seems to make the values match
    # what we expect from the other data in the PEcAn/bety database.
    trait_values[["cuticular_cond"]] <- trait_values[["cuticular_cond"]] / 10^6
  }

  # Return the modifed version
  return (trait_values)
}
##-------------------------------------------------------------------------------------------------#


##-------------------------------------------------------------------------------------------------#
##' Writes a dvmdostem PEcAn config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.dvmdostem
##' @title Write dvmdostem model configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Tobey Carman, Shawn Serbin
##' @importFrom ncdf4 ncvar_put ncvar_get
##'
write.config.dvmdostem <- function(defaults = NULL, trait.values, settings, run.id) {

  ## site information
  site <- settings$run$site
  site.id <- as.numeric(site$id)

  # Setup some local variables for this function for easily referencing
  # common locations for input, output, and the application binary.
  local_rundir <- file.path(settings$rundir, run.id)    # on local machine for staging
  rundir <- file.path(settings$host$rundir, run.id)     # on remote machine for execution
  outdir <- file.path(settings$host$outdir, run.id)
  appbinary <- settings$model$binary
  appbinary_path <- dirname(appbinary)                  # path of dvmdostem binary file

  # Subset the trait.values list to get only the traits for the PFT we are
  # interested in. The trait.values list should be something like this:
  # $`CMT04-Salix`
  #      SW_albedo    gcmax    cuticular_cond       SLA
  #            1.0     3.4               2.5       11.0
  # $`CMT04-Betula`
  #      SW_albedo    gcmax    cuticular_cond       SLA
  #            1.0      3.4               2.5      11.0
  #
  # Where there is a sub-list for each PFT. We want to reduce this to just
  # the PFT we are interested in, and with all the unit conversions taken
  # care of. So result will be something like this:
  # SW_albedo    gcmax    cuticular_cond       SLA
  #      1.0       3.4               2.5      11.0
  traits <- convert.samples.dvmdostem(trait.values[[settings$pfts$pft$name]])

  # Copy the base set of dvmdostem parameters and configurations into the
  # run directory. Some of the values in these files will be overwritten in
  # subsequent steps, but copying everything up makes sure that all the
  # necessary files exist for a dvmdostem run - the config files and the
  # parameter files in this case.
  if (dir.exists(file.path(rundir, 'config'))) {
    unlink(file.path(rundir, 'config'), recursive=TRUE)
  }
  system2(paste0("cp"),
          wait=TRUE,
          args=(c("-r",
                  file.path(appbinary_path, 'config'),
                  file.path(rundir, 'config'))))

  if (dir.exists(file.path(rundir, 'parameters'))) {
    unlink(file.path(rundir, 'parameters'), recursive=TRUE)
  }
  system2(paste0("cp"),
          wait=TRUE,
          args=(c("-r",
                  file.path(appbinary_path, 'parameters'),
                  file.path(rundir, 'parameters'))))


  # (1)
  # Read in a parameter data block from dvmdostem

  # Pull out the community name/number for use below in extracting
  # the correct block of data from the dvmdostem parameter files.
  # The settings$pfts$pft$name variable will be something like this: "CMT04-Salix"
  cmtname <- unlist(strsplit(settings$pfts$pft$name, "-", fixed=TRUE))[1]
  cmtnum <- as.numeric(unlist(strsplit(cmtname, "CMT"))[2]) #

  # Now we have to read the appropriate values out of the trait_df
  # and get those values written into the parameter file(s) that dvmdostem will
  # need when running. Because the dvmdostem parameters have a sort of
  # interesting, semi-standardized, space delimited, column format, we'll
  # use some helper scripts from dvmdostem that allow us to more easily
  # handle the parameter files and write our new trait values into the correct
  # place. The basic flow will be like this:
  #  - Read dvmdostem parameter file into json object, load into memory
  #  - Update the in-memory json object
  #  - Write the json object back out to a new dvmdostem parameter file

  # Next, use a helper script distributed with dvmdostem to read the dvmdostem
  # parameter data into memory as a json object, using a temporaroy json file
  # to hold a representation of each dvmdostem parameter file.
  dimveg_params <- paste(appbinary_path, "parameters", 'cmt_dimvegetation.txt', sep="/")
  envcanopy_params <- paste(appbinary_path, "parameters", 'cmt_envcanopy.txt', sep="/")

  # Call the helper script and write out the data to a temporary file
  # This gets just the block we are interested in (based on community type)
  # create rundir temp directory
  if (! file.exists(file.path(local_rundir, "tmp"))) {
    dir.create(file.path(local_rundir, "tmp"), recursive=TRUE)
  }
  dimveg_jsonfile <- file.path(local_rundir, "tmp",'dvmdostem-dimveg.json')
  PEcAn.logger::logger.info(paste0("dimveg_jsonfile: ", dimveg_jsonfile))
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--dump-block-to-json", dimveg_params, cmtnum)),
          stdout=dimveg_jsonfile, wait=TRUE)

  envcanopy_jsonfile <- file.path(local_rundir, "tmp",'dvmdostem-envcanopy.json')
  PEcAn.logger::logger.info(paste0("envcanopy_jsonfile: ", envcanopy_jsonfile))
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--dump-block-to-json", envcanopy_params, cmtnum)),
          stdout=envcanopy_jsonfile, wait=TRUE)

  # Read the json file into memory
  library("rjson")
  dimveg_jsondata <- fromJSON(paste(readLines(dimveg_jsonfile), collapse=""))
  envcanopy_jsondata <- fromJSON(paste(readLines(envcanopy_jsonfile), collapse=""))

  # (2)
  # Overwrite certain parameter values with (ma-posterior) trait data
  # from pecan, then write back out to disk...

  for (curr_trait in names(traits)) {
    for (jd in list(envcanopy_jsondata, dimveg_jsondata)) {
      for (i in names(jd)) {
        if (grepl("pft", i)) {
          # The PFT name stored w/in betydb is a combo of the community name
          # and the "common" pft name, always separated by a hyphen. Something
          # like this: "CMT04-Salix". The pft name in the json datastructure
          # will be simply the common name, as stored in the dvmdostem parameter
          # files. So here we extract the "common name" from the betydb PFT
          # name to make sure we are updating the correct spot in the json
          # data structure.
          pft_common_name <- unlist(strsplit(settings$pfts$pft$name, "-"))[2]
          if (identical(jd[[i]]$name, pft_common_name)) {
            if (curr_trait == "SLA") {
              jd[[i]]$sla = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_10") {
              jd[[i]]$`frprod[0]` = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_20") {
              jd[[i]]$`frprod[1]` = traits[[curr_trait]]
            }
            if (curr_trait == "extinction_coefficient_diffuse") {
              jd[[i]]$er = traits[[curr_trait]]
            }
            if (curr_trait == "SW_albedo") {
              jd[[i]]$albvisnir = traits[[curr_trait]]
            }
            if (curr_trait == "cuticular_cond") {
              jd[[i]]$gl_c = traits[[curr_trait]]
            }
            if (curr_trait == "gcmax") {
              jd[[i]]$glmax = traits[[curr_trait]]
            }
          }
        }
      }
    }
  }



  # Write it back out to disk (overwriting ok??)
  dimveg_exportJson <- toJSON(dimveg_jsondata)
  write(dimveg_exportJson, file.path(local_rundir, "tmp","dimveg_newfile.json"))

  envcanopy_exportJson <- toJSON(envcanopy_jsondata)
  write(envcanopy_exportJson, file.path(local_rundir, "tmp","envcanopy_newfile.json"))

  # (3)
  # Format a new dvmdostem parameter file using the new json file as a source.

  if (dir.exists(file.path(rundir, "parameters/"))) {
    # pass
  } else {
    print("No parameter/ directory in run directory! Need to create...")
    dir.create(file.path(rundir,"parameters" ))
  }

  ref_file <- paste0(file.path(appbinary_path, "parameters/"), 'cmt_dimvegetation.txt')
  new_param_file <- paste0(file.path(local_rundir, "parameters/"), "cmt_dimvegetation.txt")
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--fmt-block-from-json", file.path(local_rundir, "tmp","dimveg_newfile.json"), ref_file)),
          stdout=new_param_file, wait=TRUE)

  ref_file <- paste0(file.path(appbinary_path, "parameters/"), 'cmt_envcanopy.txt')
  new_param_file <- paste0(file.path(local_rundir, "parameters/"), "cmt_envcanopy.txt")
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--fmt-block-from-json", file.path(local_rundir, "tmp","envcanopy_newfile.json"), ref_file)),
          stdout=new_param_file, wait=TRUE)

  ## Cleanup rundir temp directory - comment out for debugging
  #unlink(file.path(local_rundir, "tmp"), recursive = TRUE, force = FALSE)  # comment out for debugging

  # TODO:
  #  [x] finish with parameter update process
  #  [x] dynamically copy parameters to right place
  #  - dynamically copy the output_spec from insts folder to the
  #    right place (see variable above for getting stuff from inst)
  #  - figure out how to handle the met.
  #     -> step one is symlink from raw data locations (Model install folder)
  #        into pecan run folder, maybe do this within job.sh?

  ## Update dvm-dos-tem config.js file

  # Get a copy of the config file written into the run directory with the
  # appropriate template parameters substituted.
  if (!is.null(settings$model$configtemplate) && file.exists(settings$model$configtemplate)) {
    config_template <- readLines(con=settings$model$config_template, n=-1)
  } else {
    config_template <- readLines(con=system.file("config.js.template", package = "PEcAn.dvmdostem"), n=-1)
  }

  config_template <- gsub("@INPUT_DATA_DIR@", file.path(dirname(appbinary), "DATA/SewardPen_10x10"), config_template)
  # !remove hard-coding of SewPen here!
  config_template <- gsub("@MODEL_OUTPUT_DIR@", outdir, config_template)

  if (! file.exists(file.path(settings$rundir, run.id,"config"))) dir.create(file.path(settings$rundir, run.id,"config"),
                                                                             recursive = TRUE)

  writeLines(config_template, con=file.path(settings$rundir, run.id,"config/config.js"))


  ## Update job template file below


  ### create launch script (which will create symlink) - needs to be created
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("job.sh.template", package = "PEcAn.dvmdostem"), n=-1)
  }

  ### create host specific setttings - stubbed for now, nothing to do yet, ends up as empty
  ### string that is put into the job.sh file
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

  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)

  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@BINARY@", appbinary, jobsh)

  ## model specific options from the pecan.xml file
  jobsh <- gsub("@PRERUN@", settings$model$dvmdostem_prerun, jobsh)
  jobsh <- gsub("@EQUILIBRIUM@", settings$model$dvmdostem_equil, jobsh)
  jobsh <- gsub("@SPINUP@", settings$model$dvmdostem_spinup, jobsh)
  jobsh <- gsub("@TRANSIENT@", settings$model$dvmdostem_transient, jobsh)
  jobsh <- gsub("@SCENERIO@", settings$model$dvmdostem_scenerio, jobsh)
  jobsh <- gsub("@LOGLEVEL@", settings$model$dvmdostem_loglevel, jobsh)

  writeLines(jobsh, con=file.path(settings$rundir, run.id,"job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id,"job.sh"))

}
