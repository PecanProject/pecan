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

convert.samples.dvmdostem <- function(trait.samples) {

  ### Convert object
  if (is.list(trait.samples)) {
    trait.samples <- as.data.frame(trait.samples)
  }

  ### first rename variables (needed??)

  ### Conversions (for example, convert SLA to m2/g?)

  ### Return trait.samples as modified by function
  return(trait.samples)
} # convert.samples.dvmdostem
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

  # Copy the base set of dvmdostem parameters and configurations into the
  # run directory. Some of the values in these files will be overwritten in
  # subsequent steps, but copying everything up makes sure that all the
  # necessary files exist for a tem run - the config file and the parameter
  # files in this case.
  system2(paste0("cp"),
          wait=TRUE,
          args=(c("-r",
                  file.path(appbinary_path, 'config'),
                  file.path(rundir, 'config'))))

  system2(paste0("cp"),
          wait=TRUE,
          args=(c("-r",
                  file.path(appbinary_path, 'parameters'),
                  file.path(rundir, 'parameters'))))

  # (1)
  # Read in a parameter data block from dvmdostem

  # Build a dataframe from the incoming trait.values. trait.values
  # contains the meta-analysis posteriors values for each parameter (trait)
  # that we are hoping to "inject" into dvmdostem. Convert to a dataframe
  # for easier indexing...
  trait_df <- as.data.frame(trait.values)

  # Now we have to read the approporate values out of the trait_df
  # and get those values written into the parameter file(s) that dvmdostem will
  # need when running. Because the dvmdostem parameters have a sort of
  # interesting, semi-standardized, space delimited, column format, we'll
  # use some helper scripts from dvmdostem that allow us to more easily
  # handle the parameter files and write our new trait values into the correct
  # place. The basic flow will be like this:
  #  - Read dvmdostem parameter file into json object, load into memory
  #  - Update the in-memory json object
  #  - Write the json object back out to a new dvmdostem parameter file

  # Start by figuring out which community number we are working on.
  # This is important because we need to pull the correct "community block"
  # from the dvmdostem parameter files. Each trait in the traits dataframe
  # should have a name like this:
  #   CMT04.Salix.extinction_coefficient_diffuse
  # So we can parse the first name in the dataframe to determine the CMT number.
  cmtnum <- strsplit(unlist(strsplit(names(trait_df)[1], '.', fixed=TRUE))[1], "CMT")
  cmtnum <- unlist(cmtnum)[2]
  cmtnum <- as.numeric(cmtnum)

  # Next, use a helper script distributed with dvmdostem to read the dvmdostem
  # parameter data into memory as a json object, using a temporaroy json file
  # to hold a representation of each dvmdostem parameter file.
  dimveg_params <- paste(appbinary_path, "parameters", 'cmt_dimvegetation.txt', sep="/")
  envcanopy_params <- paste(appbinary_path, "parameters", 'cmt_envcanopy.txt', sep="/")

  dimveg_jsonfile <- '/tmp/dvmdostem-dimveg.json'
  envcanopy_jsonfile <- '/tmp/dvmdostem-envcanopy.json'

  # Call the helper script and write out the data to a temporary file
  # This gets just the block we are interested in (based on community type)
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--dump-block-to-json", dimveg_params, cmtnum)),
          stdout=dimveg_jsonfile, wait=TRUE)

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

  # loop over all the incoming samples (traits)
  for (cur_t_name in names(trait_df)) {
    #print(cur_t_name)

    # In converting trait.values into a dataframe, we get 
    # these long column names that are dot separted and seem
    # to contain cmtkey.pftname.traitname
    # So the following splits out the various components into 
    # convenient local variables
    cmtkey <- unlist(strsplit(cur_t_name, '.', fixed=TRUE))[1]
    pftname <- unlist(strsplit(cur_t_name, '.', fixed=TRUE))[2]
    tname <- unlist(strsplit(cur_t_name, '.', fixed=TRUE))[3]

    # NOTE:
    # This is brittle with respect to changes in comments in dvmdostem
    # parameter files. The parameter utility script determines names for
    # variables based on parsing the comment string in the parameter files
    # and looking for the part preceeding a colon. So changes in those
    # files will require modifications to the following sections.

    # Loop over all the items in our json structure of the dvmdostem parameters
    # and find the item that matches the PFT
    for (jd_name in names(dimveg_jsondata)) {
      # First make sure that we are looking at the pftN entries, not 
      # comments, or other non-PFT variables
      if (grepl("pft", jd_name)) {
        cur_pft_name <- dimveg_jsondata[[jd_name]]$name

        # Then pick out only the entry for the correct pft 
        if (identical(cur_pft_name, pftname)) {
          print(cur_t_name)
          print(tname)
          if (tname == "SLA") {
            dimveg_jsondata[[jd_name]]$sla = trait_df[[cur_t_name]]
          } else if (tname == "frprod_perc_10") {
            dimveg_jsondata[[jd_name]]$`frprod[0]` = trait_df[[cur_t_name]]
          } else {
            # pass...variable not in this file or datastructure
          }
        }
      }
    } # end loop over json for dimvveg


    # Loop over all the items in our json structure of the dvmdostem parameters
    # and find the item that matches the PFT
    for (jd_name in names(envcanopy_jsondata)) {
      # First make sure that we are looking at the pftN entries, not
      # comments, or other non-PFT variables
      if (grepl("pft", jd_name)) {
        cur_pft_name <- envcanopy_jsondata[[jd_name]]$name

        # Then pick out only the entry for the correct pft
        if (identical(cur_pft_name, pftname)) {
          print(cur_t_name)
          print(tname)
          if (tname == "extinction_coefficient_diffuse") {
            envcanopy_jsondata[[jd_name]]$er = trait_df[[cur_t_name]]
          } else {
            # pass...variable not in this file or datastructure
          }
        }
      }
    } # end loop over json for env canopy
  }

  # Write it back out to disk (overwriting ok??)
  dimveg_exportJson <- toJSON(dimveg_jsondata)
  write(dimveg_exportJson, "/tmp/dimveg_newfile.json")

  envcanopy_exportJson <- toJSON(envcanopy_jsondata)
  write(envcanopy_exportJson, "/tmp/envcanopy_newfile.json")


  # (3)
  # Format a new dvmdostem parameter file using the new json file as a source.

  if (dir.exists(file.path(rundir, "parameters/"))) {
    # pass
  } else {
    print("No parameter/ directory in run directory! Need to create...")
    dir.create(file.path(rundir,"parameters" ))
  }
  ref_file <- paste0(file.path(appbinary_path, "parameters/"), 'cmt_dimvegetation.txt')
  new_param_file <- paste0(file.path(rundir, "parameters/"), "cmt_dimvegetation.txt")
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--fmt-block-from-json", "/tmp/dimveg_newfile.json", ref_file)),
          stdout=new_param_file, wait=TRUE)

  ref_file <- paste0(file.path(appbinary_path, "parameters/"), 'cmt_envcanopy.txt')
  new_param_file <- paste0(file.path(rundir, "parameters/"), "cmt_envcanopy.txt")
  system2(paste0(appbinary_path,"/scripts/param_util.py"),
          args=(c("--fmt-block-from-json", "/tmp/envcanopy_newfile.json", ref_file)),
          stdout=new_param_file, wait=TRUE)


  # TODO:
  #  [x] finish with parameter update process
  #  [x] dynamically copy parameters to right place
  #  - dynamically copy the output_spec from insts folder to the
  #    right place (see variable above for getting stuff from inst)
  #  - figure out how to handle the met.
  #     -> step one is symlink from raw data locations (Model install folder)
  #        into pecan run folder, maybe do this within job.sh?


  # Get a copy of the config file written into the run directory with the
  # appropriate template parameters substituted.
  if (!is.null(settings$model$configtemplate) && file.exists(settings$model$configtemplate)) {
    config_template <- readLines(con=settings$model$config_template, n=-1)
  } else {
    config_template <- readLines(con=system.file("config.js.template", package = "PEcAn.dvmdostem"), n=-1)
  }

  config_template <- gsub("@INPUT_DATA_DIR@", file.path(dirname(appbinary), "DATA/SewardPen_10x10"), config_template)
  config_template <- gsub("@MODEL_OUTPUT_DIR@", outdir, config_template)

  if (! file.exists(file.path(settings$rundir, run.id,"config"))) dir.create(file.path(settings$rundir, run.id,"config"), recursive = TRUE)

  writeLines(config_template, con=file.path(settings$rundir, run.id,"config/config.js"))

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

  writeLines(jobsh, con=file.path(settings$rundir, run.id,"job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id,"job.sh"))

}






