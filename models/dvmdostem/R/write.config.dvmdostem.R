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
  
  if("vpd_open" %in% names(trait_values)) {
    # Convert from kPa to Pa
    trait_values[["vpd_open"]] <- udunits2::ud.convert(trait_values[["vpd_open"]],"kPa","Pa")
  }
  
  if("vpd_close" %in% names(trait_values)) {
    # Convert from kPa to Pa
    trait_values[["vpd_close"]] <- udunits2::ud.convert(trait_values[["vpd_close"]],"kPa","Pa")
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
##' @importFrom rjson fromJSON toJSON
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

  # On the VM, these seem to be the same.
  PEcAn.logger::logger.info(paste0("local_rundir: ", local_rundir))
  PEcAn.logger::logger.info(paste0("rundir: ", rundir))

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
                  file.path(rundir, 'config'))))  # this seems like a problem with below since we copy this first
                                                  # and below ask if it exists and if so don't copy the template version

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

  PEcAn.logger::logger.info(paste("cmtname: ", cmtname, " cmtnum: ", cmtnum))

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
  dimveg_jsondata <- fromJSON(paste(readLines(dimveg_jsonfile), collapse=""))
  envcanopy_jsondata <- fromJSON(paste(readLines(envcanopy_jsonfile), collapse=""))

  # (2)
  # Overwrite parameter values with (ma-posterior) trait data from pecan
  PEcAn.logger::logger.info(paste0("PFT Name: ",cmtname))
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
          #PEcAn.logger::logger.info(paste0("PFT Name: ",cmtname)) # too verbose
          if (identical(jd[[i]]$name, pft_common_name)) {
            if (curr_trait == "SLA") {
              dimveg_jsondata[[i]]$sla = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_10") {
              dimveg_jsondata[[i]]$`frprod[0]` = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_20") {
              dimveg_jsondata[[i]]$`frprod[1]` = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_30") {
              dimveg_jsondata[[i]]$`frprod[2]` = traits[[curr_trait]]
            }
            if (curr_trait == "frprod_perc_40") {
              dimveg_jsondata[[i]]$`frprod[3]` = traits[[curr_trait]]
            }
            if (curr_trait == "klai") {
              dimveg_jsondata[[i]]$klai = traits[[curr_trait]]
            }
            if (curr_trait == "ilai") {
              dimveg_jsondata[[i]]$lai = traits[[curr_trait]]
            }
            if (curr_trait == "extinction_coefficient_diffuse") {
              envcanopy_jsondata[[i]]$er = traits[[curr_trait]]
            }
            if (curr_trait == "SW_albedo") {
              envcanopy_jsondata[[i]]$albvisnir = traits[[curr_trait]]
            }
            if (curr_trait == "cuticular_cond") {
              envcanopy_jsondata[[i]]$gl_c = traits[[curr_trait]]
            }
            if (curr_trait == "gcmax") {
              envcanopy_jsondata[[i]]$glmax = traits[[curr_trait]]
            }
            if (curr_trait == "ppfd50") {
              envcanopy_jsondata[[i]]$ppfd50 = traits[[curr_trait]]
            }
            if (curr_trait == "vpd_open") {
              envcanopy_jsondata[[i]]$vpd_open = traits[[curr_trait]]
            }
            if (curr_trait == "vpd_close") {
              envcanopy_jsondata[[i]]$vpd_close = traits[[curr_trait]]
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
  unlink(file.path(local_rundir, "tmp"), recursive = TRUE, force = FALSE)  # comment out for debugging

  # TODO:
  #  [x] finish with parameter update process
  #  [x] dynamically copy parameters to right place
  #  - dynamically copy the output_spec from insts folder to the
  #    right place (see variable above for getting stuff from inst)
  #  - figure out how to handle the met.
  #     -> step one is symlink from raw data locations (Model install folder)
  #        into pecan run folder, maybe do this within job.sh?

  # Met info
  met_driver_dir <- dirname(settings$run$inputs$met$path)
  PEcAn.logger::logger.info(paste0("Using met driver path: ", met_driver_dir))
  
  # Pick up the site and pixel settings from the xml file if they exist
  if (is.null(settings$model$dvmdostem_site)){
    siteDataPath <- met_driver_dir
  } else {
    siteDataPath <- settings$model$dvmdostem_site
  }
  PEcAn.logger::logger.info(paste0("Using siteDataPath: ", siteDataPath))

  if (is.null(settings$model$dvmdostem_pixel_y)){
    pixel_Y <- 1
  } else {
    pixel_Y <- settings$model$dvmdostem_pixel_y
  }
  if (is.null(settings$model$dvmdostem_pixel_x)){
    pixel_X <- 1
  } else {
    pixel_X <- settings$model$dvmdostem_pixel_x
  }


  # Build a custom run-mask - in case the run-mask that ships with dvmdostem
  # it not what we want.

  # Copy the run-mask from the input data directory to the run directory
  system2(paste0("cp"),
          wait=TRUE,
          args=(c("-r",
                  file.path(siteDataPath, 'run-mask.nc'),
                  file.path(rundir, 'run-mask.nc'))))

  # # Turn off all pixels except the 0,0 pixel in the mask
  # Can't seem to use this as python-netcdf4 is not available. WTF.
  # system2(paste0(file.path(appbinary_path, "scripts/runmask-util.py")),
  #         wait=TRUE,
  #         args=c("--reset", "--yx", pixel_Y, pixel_X, file.path(rundir, 'run-mask.nc')))

  ## !!DANGER!!
  ## - crazy R dimension ordering, with X first! (Y first in all other implementations!)
  ## - R does not have a 64bit integer datatype so we get a warning about casting
  ##   and that there may be information lost (unlikely in this case)
  ncMaskFile <- ncdf4::nc_open(file.path(rundir, 'run-mask.nc'), write = TRUE)
  new_data <- matrix(0, ncMaskFile$dim$X$len, ncMaskFile$dim$Y$len)
  new_data[[strtoi(pixel_X), strtoi(pixel_Y)]] <- 1
  ncdf4::ncvar_put(ncMaskFile, ncMaskFile$var$run, new_data, verbose=TRUE)
  ncdf4::nc_close(ncMaskFile)


  # Open the input veg file, check that the pixel that is enabled in the
  # run mask is the right veg type to match the cmt/pft that is selected
  # for the run.
  ncVegCMTFile <- ncdf4::nc_open(file.path(siteDataPath, "vegetation.nc"), write=FALSE)
  veg_class <- ncdf4::ncvar_get(ncVegCMTFile, ncVegCMTFile$var$veg_class)
  if (cmtnum != veg_class[[strtoi(pixel_X), strtoi(pixel_Y)]]) {
    PEcAn.logger::logger.error("INCORRECT PIXEL!! THIS RUN WILL NOT WORK!")
    PEcAn.logger::logger.error("STOPPING NOW TO PREVENT FUTURE HEARTACHE!")
    stop()
  }




  ## Update dvm-dos-tem config.js file

  # Get a copy of the config file written into the run directory with the
  # appropriate template parameters substituted.
  if (!is.null(settings$model$configtemplate) && file.exists(settings$model$configtemplate)) {
    config_template <- readLines(con=settings$model$config_template, n=-1)
  } else {
    config_template <- readLines(con=system.file("config.js.template", package = "PEcAn.dvmdostem"), n=-1)
  }

  config_template <- gsub("@MET_DRIVER_DIR@", met_driver_dir, config_template)
  #config_template <- gsub("@INPUT_DATA_DIR@", file.path(dirname(appbinary), siteDataPath), config_template)
  config_template <- gsub("@INPUT_DATA_DIR@", siteDataPath, config_template)
  config_template <- gsub("@MODEL_OUTPUT_DIR@", outdir, config_template)
  config_template <- gsub("@CUSTOM_RUN_MASK@", file.path(rundir), config_template )

  if (! file.exists(file.path(settings$rundir, run.id,"config")) ) {
    dir.create(file.path(settings$rundir, run.id,"config"),recursive = TRUE)
  }

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
  # setup defaults if missing - may not want to do this long term
  if (is.null(settings$model$dvmdostem_prerun)){
    jobsh <- gsub("@PRERUN@", 100, jobsh)
  } else {
    jobsh <- gsub("@PRERUN@", settings$model$dvmdostem_prerun, jobsh)
  }
  
  if (is.null(settings$model$dvmdostem_equil)){
    jobsh <- gsub("@EQUILIBRIUM@", 1000, jobsh)
  } else {
    jobsh <- gsub("@EQUILIBRIUM@", settings$model$dvmdostem_equil, jobsh)
  }

  if (is.null(settings$model$dvmdostem_spinup)){
    jobsh <- gsub("@SPINUP@", 450, jobsh)
  } else {
    jobsh <- gsub("@SPINUP@", settings$model$dvmdostem_spinup, jobsh)
  }
  
  if (is.null(settings$model$dvmdostem_transient)){
    jobsh <- gsub("@TRANSIENT@", 109, jobsh)  # what if this isn't the case?  Do we want to hard code backup or just end in error?
  } else {                                    # could just invoke a stop() here for these if missing and provide an error message
    jobsh <- gsub("@TRANSIENT@", settings$model$dvmdostem_transient, jobsh)
  }

  if (is.null(settings$model$dvmdostem_scenerio)){
    jobsh <- gsub("@SCENERIO@", 91, jobsh)
  } else {
    jobsh <- gsub("@SCENERIO@", settings$model$dvmdostem_scenerio, jobsh)
  }

  if (is.null(settings$model$dvmdostem_loglevel)){
    jobsh <- gsub("@LOGLEVEL@", "err", jobsh)
  } else {
    jobsh <- gsub("@LOGLEVEL@", settings$model$dvmdostem_loglevel, jobsh)
  }

  writeLines(jobsh, con=file.path(settings$rundir, run.id,"job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id,"job.sh"))

} # end of function
#------------------------------------------------------------------------------------------------#
### EOF
