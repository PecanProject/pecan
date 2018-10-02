#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials 
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
## Functions to prepare and write out ED2.2 config.xml files for MA, SA, and Ensemble runs
##-------------------------------------------------------------------------------------------------#

##-------------------------------------------------------------------------------------------------#
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"ed.dtd\">\n"

## TODO: Update this script file to use the database for setting up ED2IN and config files
##-------------------------------------------------------------------------------------------------#

##-------------------------------------------------------------------------------------------------#
##' convert parameters from PEcAn database default units to ED defaults
##' 
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @author Shawn Serbin, David LeBauer, Carl Davidson, Ryan Kelly
convert.samples.ED <- function(trait.samples) {
  DEFAULT.LEAF.C <- 0.48
  DEFAULT.MAINTENANCE.RESPIRATION <- 1 / 2
  ## convert SLA from m2 / kg leaf to m2 / kg C
  
  # IF: trait.samples not being a list throws an error later in the write.config.xml.ED2
  trait.samples <- as.list(trait.samples)
  
  if ("SLA" %in% names(trait.samples)) {
    sla <- as.numeric(trait.samples[["SLA"]])
    trait.samples[["SLA"]] <- sla/DEFAULT.LEAF.C
  }
  
  # for model version compatibility (q and fineroot2leaf are the same)
  if ("fineroot2leaf" %in% names(trait.samples)) {
    trait.samples[["q"]] <- as.numeric(trait.samples[["fineroot2leaf"]])
  }
  
  ## convert leaf width / 1000
  if ("leaf_width" %in% names(trait.samples)) {
    lw <- as.numeric(trait.samples[["leaf_width"]])
    trait.samples[["leaf_width"]] <- lw / 1000
  }
  
  if ("root_respiration_rate" %in% names(trait.samples)) {
    rrr1 <- as.numeric(trait.samples[["root_respiration_rate"]])
    rrr2 <- rrr1 * DEFAULT.MAINTENANCE.RESPIRATION
    trait.samples[["root_respiration_rate"]] <- arrhenius.scaling(rrr2, old.temp = 25, new.temp = 15)
    # model version compatibility (rrr and rrf are the same)
    trait.samples[["root_respiration_factor"]] <- trait.samples[["root_respiration_rate"]]
  }
  
  if ("Vcmax" %in% names(trait.samples)) {
    vcmax <- as.numeric(trait.samples[["Vcmax"]])
    trait.samples[["Vcmax"]] <- arrhenius.scaling(vcmax, old.temp = 25, new.temp = 15)
    # write as Vm0 for version compatibility (Vm0 = Vcmax @ 15C)
    trait.samples[["Vm0"]] <- trait.samples[["Vcmax"]]
    
    ## Convert leaf_respiration_rate_m2 to dark_resp_factor; requires Vcmax
    if ("leaf_respiration_rate_m2" %in% names(trait.samples)) {
      leaf_resp <- as.numeric(trait.samples[["leaf_respiration_rate_m2"]])
      
      ## First scale variables to 15 degC
      trait.samples[["leaf_respiration_rate_m2"]] <- 
        arrhenius.scaling(leaf_resp, old.temp = 25, new.temp = 15)
      # convert leaf_respiration_rate_m2 to Rd0 (variable used in ED2)
      trait.samples[["Rd0"]] <- trait.samples[["leaf_respiration_rate_m2"]]
      
      ## Calculate dark_resp_factor -- Will be depreciated when moving from older versions of ED2
      trait.samples[["dark_respiration_factor"]] <- 
        trait.samples[["leaf_respiration_rate_m2"]] / trait.samples[["Vcmax"]]
      
      
    }  ## End dark_respiration_factor loop
  }  ## End Vcmax  
  # for debugging conversions save(trait.samples, file = file.path(settings$outdir,
  # 'trait.samples.Rdata'))
  
  # return converted samples
  return(trait.samples)
}
# ==================================================================================================#


##-------------------------------------------------------------------------------------------------#
##' Write ED configuration files
##'
##' Writes an xml and ED2IN config files for use with the Ecological Demography 
##' model. Requires a pft xml object, a list of trait values for a single model 
##' run, and the name of the file to create
#'
##' @param trait.values Named list of trait values, with names corresponding to PFT
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @param defaults list of defaults to process. Default=settings$constants
##' @param check Logical. If `TRUE`, check ED2IN validity before running and 
##' throw an error if anything is wrong (default = `FALSE`)
##' 
##' @return configuration file and ED2IN namelist for given run
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov, Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.ED2 <- function(trait.values, settings, run.id, defaults = settings$constants, check = FALSE, ...) {
  
  
  jobsh <- write.config.jobsh.ED2(settings = settings, run.id = run.id)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ## Write ED2 config.xml file
  xml <- write.config.xml.ED2(defaults = defaults, settings = settings, trait.values = trait.values)
  
  saveXML(xml, file = file.path(settings$rundir, run.id, "config.xml"), indent = TRUE, prefix = PREFIX_XML)
  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  
  ##----------------------------------------------------------------------
  ## Edit ED2IN file for runs
  revision <- settings$model$revision
  if (is.null(revision)) {
    model <- db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params = settings$database$bety)
    revision <- model$revision
  }
  revision <- gsub("^r", "", revision)

  if (!is.null(settings$model$edin) && file.exists(settings$model$edin)) {
    ed2in.text <- read_ed2in(settings$model$edin)
  } else {
    filename <- system.file(settings$model$edin, package = "PEcAn.ED2")
    if (filename == "") {
      filename <- system.file(paste0("ED2IN.r", revision), package = "PEcAn.ED2")
    }
    if (filename == "") {
      PEcAn.logger::logger.severe("Could not find ED template")
    }
    PEcAn.logger::logger.info("Using", filename, "as template")
    ed2in.text <- read_ed2in(filename)
  }

  metstart <- tryCatch(format(as.Date(settings$run$site$met.start), "%Y"), 
                       error = function(e) settings$run$site$met.start)
  metend <- tryCatch(format(as.Date(settings$run$site$met.end), "%Y"), 
                     error = function(e) settings$run$site$met.end)
  

  ed2in.text <- modify_ed2in(
    ed2in.text,
    latitude = as.numeric(settings$run$site$lat),
    longitude = as.numeric(settings$run$site$lon),
    met_driver = settings$run$inputs$met$path,
    start_date = startdate,
    end_date = enddate,
    MET_START = metstart,
    MET_END = metend,
    IMETAVG = -1,   # See below,
    add_if_missing = TRUE,
    check_paths = check
  )

  # The flag for IMETAVG tells ED what to do given how input radiation was 
  # originally averaged
  # -1 = I don't know, use linear interpolation
  # 0 = No average, the values are instantaneous
  # 1 = Averages ending at the reference time
  # 2 = Averages beginning at the reference time
  # 3 = Averages centered at the reference time Deafult is -1
  
  
  if (is.null(settings$model$phenol.scheme)) {
    PEcAn.logger::logger.error(paste0("no phenology scheme set; \n",
                                     "need to add <phenol.scheme> ",
                                     "tag under <model> tag in settings file"))
  } else if (settings$model$phenol.scheme == 1) {
    ## Set prescribed phenology switch in ED2IN
    ed2in.text <- modify_ed2in(
      ed2in.text,
      IPHEN_SCHEME   = as.numeric(settings$model$phenol.scheme),
      PHENPATH       = settings$model$phenol,
      IPHENYS1       = settings$model$phenol.start,
      PIPHENYSF      = settings$model$phenol.end,
      IPHENYF1       = settings$model$phenol.start,
      IPHENYFF       = settings$model$phenol.end,
      add_if_missing = TRUE,
      check_paths    = check
    )
  } else {
    ## If not prescribed set alternative phenology scheme.
    ed2in.text <- modify_ed2in(
      ed2in.text,
      IPHEN_SCHEME   = as.numeric(settings$model$phenol.scheme),
      PHENPATH       = "",
      IPHENYS1       = settings$model$phenol.start,
      PIPHENYSF      = settings$model$phenol.end,
      IPHENYF1       = settings$model$phenol.start,
      IPHENYFF       = settings$model$phenol.end,
      add_if_missing = TRUE,
      check_paths    = check
    )
  }

  ## -------------
  # Special parameters for SDA
  # 
  if (!is.null(settings$state.data.assimilation)) {
    # Default values
    sda_tags <- list(
      ISOUTPUT = 3,     # Save history state file
      UNITSTATE = 3,    # History state frequency is years
      FRQSTATE = 1      # Write history file every 1 year
    )
    
    # Overwrite defaults with values from settings$model$ed2in_tags list
    if(!is.null(settings$model$ed2in_tags)){
      sda_tags <- modifyList(sda_tags, settings$model$ed2in_tags[names(sda_tags)])
    }
    ed2in.text <- modify_ed2in(ed2in.text, .dots = sda_tags, add_if_missing = TRUE, check_paths = check)
  }

  ##----------------------------------------------------------------------
  # Get prefix of filename, append to dirname.
  # Assumes pattern 'DIR/PREFIX.lat<REMAINDER OF FILENAME>'
  # Slightly overcomplicated to avoid error if path name happened to contain .lat'
  
  
  # when pss or css not exists, case 0
  if (is.null(settings$run$inputs$pss$path) | is.null(settings$run$inputs$css$path)) {
    ed2in.text <- modify_ed2in(
      ed2in.text,
      IED_INIT_MODE = 0,
      SFILIN = "",
      add_if_missing = TRUE,
      check_paths = check
    )
  } else {
    lat_rxp <- "\\.lat.*lon.*\\.(css|pss|site)"
    prefix.css <- sub(lat_rxp, "", settings$run$inputs$css$path)
    prefix.pss <- sub(lat_rxp, "", settings$run$inputs$pss$path)
    # pss and css prefix is not the same, kill
    if (!identical(prefix.pss, prefix.css)) {
      PEcAn.logger::logger.info(paste("pss prefix:", prefix.pss))
      PEcAn.logger::logger.info(paste("css prefix:", prefix.css))
      PEcAn.logger::logger.severe("ED2 css/pss/ files have different prefix")
    }
    # pss and css are both present
    value <- 2
    # site exists
    if (!is.null(settings$run$inputs$site$path)) {
      prefix.site <- sub(lat_rxp, "", settings$run$inputs$site$path)
      # sites and pss have different prefix name, kill
      if (!identical(prefix.site, prefix.pss)) {
        PEcAn.logger::logger.info(paste("site prefix:", prefix.site))
        PEcAn.logger::logger.info(paste("pss prefix:", prefix.pss))
        PEcAn.logger::logger.severe("ED2 sites/pss/ files have different prefix")
      } else {
        # sites and pass same prefix name, case 3
        value <- 3
      }
      
    }
    ed2in.text <- modify_ed2in(
      ed2in.text,
      IED_INIT_MODE = value,
      SFILIN = paste0(prefix.pss, "."),
      add_if_missing = TRUE,
      check_paths = check
    )
  }
  

  thsum <- settings$run$inputs$thsum$path
  if (!grepl("/$", thsum)) {
    thsum <- paste0(thsum, "/")
  }

  ed2in.text <- modify_ed2in(
    ed2in.text,
    VEG_DATABASE = settings$run$inputs$veg$path,
    SOIL_DATABASE = settings$run$inputs$soil$path,
    LU_DATABASE = settings$run$inputs$lu$path,
    THSUMS_DATABASE = thsum,
    add_if_missing = TRUE,
    check_paths = check
  )
  
  ##----------------------------------------------------------------------
  if (is.null(settings$host$scratchdir)) {
    modeloutdir <- file.path(settings$host$outdir, run.id)
  } else {
    modeloutdir <- file.path(settings$host$scratchdir, settings$workflow$id, run.id)
  }
  ed2in.text <- modify_ed2in(
    ed2in.text,
    run_name = paste0("ED2 v", revision, " PEcAn ", run.id),
    run_dir = file.path(settings$host$rundir, run.id),    # For `config.xml`
    output_dir = modeloutdir,   # Sets analysis and history paths
    add_if_missing = TRUE,
    check_paths = check
  )
  
  ##---------------------------------------------------------------------
  # Modify any additional tags provided in settings$model$ed2in_tags
  ed2in.text <- modify_ed2in(ed2in.text, .dots = settings$model$ed2in_tags, add_if_missing = TRUE, check_paths = check)
  
  ##----------------------------------------------------------------------
  if (check) {
    check_ed2in(ed2in.text)
  }
  write_ed2in(ed2in.text, file.path(settings$rundir, run.id, "ED2IN"))
} # write.config.ED2
# ==================================================================================================#

##-------------------------------------------------------------------------------------------------#
##' Clear out old config and ED model run files.
##'
##' @name remove.config.ED2
##' @title Clear out old config and ED model run files.
##' @return nothing, removes config files as side effect
##' @export
##' @author Shawn Serbin, David LeBauer, Alexey Shikomanov
remove.config.ED2 <- function(main.outdir = settings$outdir, settings) {
  
  print(" ")
  print("---- Removing previous ED2 config files and output before starting new run ----")
  print(" ")
  
  todelete <- dir(settings$outdir, pattern = c("/c.*", "/ED2INc.*"), recursive = TRUE, full.names = TRUE)
  
  if (length(todelete > 0)) {
    file.remove(todelete)
  }
  rm(todelete)
  
  ## Remove model run configs and model run log files on local/remote host
  if (!settings$host$name == "localhost") {
    ## Remove model run congfig and log files on remote host
    remote_ls <- function(path, pattern) {
      PEcAn.remote::remote.execute.cmd(host = settings$host, cmd = "ls", args = file.path(path, pattern))
    }
    config <- remote_ls(settings$host$rundir, "c.*")
    ed2in <- remote_ls(settings$host$rundir, "ED2INc.*")
    output_remote <- remote_ls(settings$host$outdir, ".")
    output <- file.path(settings$host$outdir, output_remote)
    
    if (length(config) > 0 | length(ed2in) > 0) {
      todelete <- c(config, ed2in[-grep("log", ed2in)], output)  ## Keep log files
      PEcAn.remote::remote.execute.cmd(settings$host, "rm", c("-f", todelete))
    }
  }
} # remove.config.ED2
# ==================================================================================================#

#' @name write.config.xml.ED2
#' @title Write ED2 config.xml file
#' @details Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @export
#' @param settings PEcAn settings file. Settings required for this script are: model$revision, model$config.header, constants
#' @param trait.values List of trait values with which to replace defaults
#' @param defaults List of defaults to process. Default = settings$constants
#' @return R XML object containing full ED2 XML file
#' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov
write.config.xml.ED2 <- function(settings, trait.values, defaults = settings$constants) {

  ## Find history file TODO this should come from the database
  ed2_package_data <- data(package="PEcAn.ED2")
  histfile <- paste0("history.r", settings$model$revision) # set history file name to look for in ed2_package_data
  if (histfile %in% ed2_package_data$results[, "Item"]) {
    PEcAn.logger::logger.debug(paste0("--- Using ED2 History File: ", histfile))
    data(list=histfile, package = 'PEcAn.ED2')
    edhistory <- get(histfile)
  } else {
    PEcAn.logger::logger.debug("--- Using Generic ED2 History File: history.csv")
    histfile <- "history"
    data(list=histfile, package = 'PEcAn.ED2')
    edhistory <- get(histfile)
  }

  edtraits <- names(edhistory)
  data(pftmapping, package = 'PEcAn.ED2')
  
  ## Get ED2 specific model settings and put into output config xml file
  xml <- PEcAn.settings::listToXml(settings$model$config.header, "config")

  ## Process the names in defaults. Runs only if names(defaults) are null or have at least one
  ## instance of name attribute 'pft'. Otherwise, AS assumes that names in defaults are already set
  ## to the corresponding PFT names.
  currentnames <- names(defaults)
  if (is.null(currentnames) | "pft" %in% currentnames) {
    newnames <- sapply(defaults, "[[", "name")
    newnames.notnull <- which(!sapply(newnames, is.null))
    names(defaults)[newnames.notnull] <- newnames[newnames.notnull]
  }

  for (i in seq_along(trait.values)) {
    group <- names(trait.values)[i]
    if (group == "env") {

      ## set defaults from config.header

    } else {
      # Make this agnostic to the way PFT names are defined in `trait.values` -- either directly as
      # list names or as object 'name' within each sublist is fine
      if (group == "pft") {
        pft <- trait.values[[i]]$name
      } else {
        pft <- group
      }

      # RyK: Changed this so that pftmapping is required. pft$constants$num is the number that will be
      # written to config.xml, and it's used by fia.to.ed when mapping spp to a PFT #.  But if you're
      # overriding an existing ED2 pft, then you might not want that PFT number to be used to look up
      # default param values.  e.g. if you're trying to run temperate.Hydric PFT, which which doesn't
      # exist in ED, you would need to assign it either an existing but currently unused ED2 PFT number
      # (e.g. #2 = 'early tropical'), or an unused one (e.g. #18). Either way, that number isn't
      # necessarily what you'd want to look up default parameters for the PFT.  What really should
      # happen is for there to be two settings for each PFT: the 'num' to use to represent the PFT to
      # ED, and the 'defaults.PFT' (name or number) to use for pulling default parameter values.
      pft.number <- pftmapping$ED[which(pftmapping == pft)]

      if(grepl("soil", pft)){
        data(soil, package = "PEcAn.ED2")
        vals <- as.list(soil)
        names(vals) <- colnames(soil)

        converted.trait.values <- convert.samples.ED(trait.values[[i]])
        vals <- modifyList(vals, converted.trait.values)

        decompositon.xml <- PEcAn.settings::listToXml(vals, "decomposition")
        xml <- XML::append.xmlNode(xml, decompositon.xml)
      } else if(length(pft.number) == 0) {
        PEcAn.logger::logger.error(pft, "was not matched with a number in settings$constants or pftmapping data. Consult the PEcAn instructions on defining new PFTs.")
        stop("Unable to set PFT number")
      }else{
        # TODO: Also modify web app to not default to 1

        ## Get default trait values from ED history
        vals <- as.list(edhistory[edhistory$num == pft.number, ])

        ## Convert trait values to ED units
        converted.trait.values <- convert.samples.ED(trait.values[[i]])

        ## Selectively replace defaults with trait values
        vals <- modifyList(vals, converted.trait.values)

        ## Convert settings constants to ED units
        converted.defaults <- convert.samples.ED(defaults[[pft]]$constants)

        ## Selectively replace defaults and trait values with constants from settings
        if (!is.null(converted.defaults)){
          vals <- modifyList(vals, converted.defaults)
        }

        pft.xml <- PEcAn.settings::listToXml(vals, "pft")
        xml <- XML::append.xmlNode(xml, pft.xml)
      }

    }
  }
  return(xml)
} # write.config.xml.ED2

# ==================================================================================================#
#' @name write.config.jobsh.ED2
#' @title Write ED2 config.xml file
#' @description Function for writing job.sh file for ED2 runs
#' @details Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @export
#' @param settings PEcAn settings list. For this function, need the following:
#' run$host$rundir, run$host$outdir, run$host$scratchdir,
#' run$host$clearscratch, model$jobtemplate, model$job.sh, run$host$job.sh,
#' run$site$lat, run$site$lon, run$inputs$met$path, run$start.date,
#' run$end.date, model$binary
#' @param run.id PEcAn run ID
#' @return Character vector containing job.sh file
#' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov
write.config.jobsh.ED2 <- function(settings, run.id) {
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)

  # command if scratch is used
  if (is.null(settings$host$scratchdir)) {
    modeloutdir <- outdir
    mkdirscratch <- "# no need to mkdir for scratch"
    copyscratch <- "# no need to copy from scratch"
    clearscratch <- "# no need to clear scratch"
  } else {
    modeloutdir <- file.path(settings$host$scratchdir, settings$workflow$id, run.id)
    mkdirscratch <- paste("mkdir -p", modeloutdir)
    copyscratch <- paste("rsync", "-a",
                         paste0("\"", file.path(modeloutdir, ""), "\""),
                         paste0("\"", file.path(outdir, ""), "\""))
    if (is.null(settings$host$clearscratch) || is.na(as.logical(settings$host$clearscratch)) ||
        as.logical(settings$host$clearscratch)) {
      clearscratch <- paste("rm", "-rf", paste0("\"", modeloutdir, "\""))
    } else {
      clearscratch <- "# scratch is not cleared"
    }
  }
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.ED2"), n = -1)
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
  jobsh <- gsub("@SITE_MET@", settings$run$inputs$met$path, jobsh)

  jobsh <- gsub("@SCRATCH_MKDIR@", mkdirscratch, jobsh)
  jobsh <- gsub("@SCRATCH_COPY@", copyscratch, jobsh)
  jobsh <- gsub("@SCRATCH_CLEAR@", clearscratch, jobsh)

  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  
  pft_names <- unlist(sapply(settings$pfts, `[[`, "name"))
  pft_names <- paste0("c('", paste(pft_names, collapse = "','"), "')")
  jobsh <- gsub("@PFT_NAMES@", pft_names, jobsh)
  
  return(jobsh)
} # write.config.jobsh.ED2
