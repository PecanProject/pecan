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
##' Convert parameters from PEcAn database default units to ED defaults
##'
##' Performs model specific unit conversions on a a list of trait values, such
##' as those provided to write.config
##' @param trait.samples a matrix or dataframe of samples from the trait
##'   distribution
##' @return matrix or dataframe with values transformed
##' @author Shawn Serbin, David LeBauer, Carl Davidson, Ryan Kelly
convert.samples.ED <- function(trait.samples) {
  DEFAULT.LEAF.C <- 0.48
  DEFAULT.MAINTENANCE.RESPIRATION <- 1 / 2
  ## convert SLA from m2 / kg leaf to m2 / kg C
  
  # IF: trait.samples not being a list throws an error later in the
  # write.config.xml.ED2
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
    trait.samples[["root_respiration_rate"]] <- PEcAn.utils::arrhenius.scaling(rrr2, old.temp = 25, new.temp = 15)
    # model version compatibility (rrr and rrf are the same)
    trait.samples[["root_respiration_factor"]] <- trait.samples[["root_respiration_rate"]]
  }
  
  if ("Vcmax" %in% names(trait.samples)) {
    vcmax <- as.numeric(trait.samples[["Vcmax"]])
    trait.samples[["Vcmax"]] <- PEcAn.utils::arrhenius.scaling(vcmax, old.temp = 25, new.temp = 15)
    # write as Vm0 for version compatibility (Vm0 = Vcmax @ 15C)
    trait.samples[["Vm0"]] <- trait.samples[["Vcmax"]]
    
    ## Convert leaf_respiration_rate_m2 to dark_resp_factor; requires Vcmax
    if ("leaf_respiration_rate_m2" %in% names(trait.samples)) {
      leaf_resp <- as.numeric(trait.samples[["leaf_respiration_rate_m2"]])
      
      ## First scale variables to 15 degC
      trait.samples[["leaf_respiration_rate_m2"]] <- 
        PEcAn.utils::arrhenius.scaling(leaf_resp, old.temp = 25, new.temp = 15)
      # convert leaf_respiration_rate_m2 to Rd0 (variable used in ED2)
      trait.samples[["Rd0"]] <- trait.samples[["leaf_respiration_rate_m2"]]
      
      ## Calculate dark_resp_factor -- Will be depreciated when moving from
      ## older versions of ED2
      trait.samples[["dark_respiration_factor"]] <-
        trait.samples[["leaf_respiration_rate_m2"]] / trait.samples[["Vcmax"]]
      
      
    }  ## End dark_respiration_factor loop
  }  ## End Vcmax  
  
  if ("plant_min_temp" %in% names(trait.samples)) {
    trait.samples[["plant_min_temp"]] <- PEcAn.utils::ud_convert(trait.samples[["plant_min_temp"]], "degC", "K")
  }
  # for debugging conversions save(trait.samples, file =
  # file.path(settings$outdir, 'trait.samples.Rdata'))
  
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
##'
##' @param trait.values Named list of trait values, with names corresponding to
##'   PFT
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @param defaults list of defaults to process. Default=settings$constants
##' @param check Logical. If `TRUE`, check ED2IN validity before running and
##'   throw an error if anything is wrong (default = `FALSE`)
##' @param ... unused
##'
##' @return configuration file and ED2IN namelist for given run
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson, Alexey Shiklomanov,
##'   Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.ED2 <- function(trait.values, settings, run.id, defaults = settings$constants, check = FALSE, ...) {
  
  
  jobsh <- write.config.jobsh.ED2(settings = settings, run.id = run.id)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ## Write ED2 config.xml file
  xml <- write.config.xml.ED2(defaults = defaults, settings = settings, trait.values = trait.values)
  
  XML::saveXML(xml, file = file.path(settings$rundir, run.id, "config.xml"), indent = TRUE, prefix = PREFIX_XML)
  
  startdate <- as.Date(settings$run$start.date)
  enddate <- as.Date(settings$run$end.date)
  
  ##----------------------------------------------------------------------
  ## Edit ED2IN file for runs
  revision <- settings$model$revision
  if (is.null(revision)) {
    PEcAn.logger::logger.severe("ED2 model `revision` not specified in PEcAn settings file.")
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

  ed2in.text <- modify_ed2in(
    ed2in.text,
    latitude = as.numeric(settings$run$site$lat),
    longitude = as.numeric(settings$run$site$lon),
    met_driver = settings$run$inputs$met$path,
    start_date = startdate,
    end_date = enddate,
    IMETAVG = -1,   # See below,
    add_if_missing = TRUE,
    check_paths = check
  )

  # The flag for IMETAVG tells ED what to do given how input radiation
  # was originally averaged
  # -1 = I don't know, use linear interpolation (default)
  # 0 = No average, the values are instantaneous
  # 1 = Averages ending at the reference time
  # 2 = Averages beginning at the reference time
  # 3 = Averages centered at the reference time

  # By default, modify_ed2in sets the met start and end dates to run
  # start and end dates, respectively. However, the `met.start` and
  # `met.end` XML tags may be used to set these to different values,
  # e.g. if you want to run ED under constant, looped meteorology.
  proc_met_startend <- function(rawval, ed2in_tag) {
    stopifnot(
      ed2in_tag %in% c("METCYC1", "METCYCF"),
      length(rawval) == 1
    )
    if (is.null(rawval)) return(ed2in.text)
    # The corresponding ED2IN tags METCYC1 and METCYCF expect a year,
    # so we try to extract the year from the input value here.
    value <- tryCatch(
      lubridate::year(rawval),
      error = function(e) as.numeric(rawval)
    )
    if (!is.finite(value)) {
      PEcAn.logger::logger.error(
        "Unable to parse custom met ", ed2in_tag, " value: ", rawval, ". ",
        "Setting it based on run dates."
      )
      return(ed2in.text)
    }
    if (value < 1000 || value > 3000) {
      PEcAn.logger::logger.info(
        "WARNING: Suspicious ", ed2in_tag, " value: ", value
      )
    }
    ed2in.text[[ed2in_tag]] <- value
    return(ed2in.text)
  }
  ed2in.text <- proc_met_startend(settings[[c("run", "site", "met.start")]], "METCYC1")
  ed2in.text <- proc_met_startend(settings[[c("run", "site", "met.end")]], "METCYCF")

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
      sda_tags <- utils::modifyList(sda_tags, settings$model$ed2in_tags[names(sda_tags)])
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
  # Use all PFTs, or just the ones configured in config.xml?
  all_pfts <- settings$model$all_pfts
  if (!is.null(all_pfts) && tolower(all_pfts) != "false") {
    # Use all ED PFTs
    use_pfts <- 1:17
    PEcAn.logger::logger.debug("Running with all PFTs (1:17)")
  } else {
    # Use only PFTs configured in ED2 config.xml
    # xml object (`config.xml`) created above. Here, we pull out the
    # <num> tags from each PFT to get the numbers.
    #
    # I'm sure there is a better way to pull values out of an XML node
    # object, but I have no idea what it is. If you know, please fix this.
    use_pfts <- numeric(length(xml))
    for (i in seq_along(xml)) {
      use_pfts[i] <- as.numeric(XML::xmlValue(xml[[i]][['num']]))
    }
    use_pfts <- use_pfts[is.finite(use_pfts)]
    PEcAn.logger::logger.debug(
      "Running with only these PFTs (set by config.xml): ",
      paste(use_pfts, collapse = ", ")
    )
  }
  ed2in.text <- modify_ed2in(ed2in.text, include_these_pft = use_pfts)
  
  ##---------------------------------------------------------------------
  # Modify any additional tags provided in settings$model$ed2in_tags
  custom_tags <- settings$model$ed2in_tags
  if (!is.null(custom_tags)) {
    # Convert numeric tags to numeric
    # Anything that isn't converted to NA via `as.numeric` is numeric
    custom_tags <- lapply(custom_tags, function(x)
                          tryCatch(as.numeric(x), warning = function(e) x))
    # Figure out what is a numeric vector
    # Look for a list of numbers like: "1,2,5"
    # Works for decimals, negatives, and arbitrary spacing: "1.3,2.6,   -7.8  ,  8.1"
    numvec_rxp <- paste0("^ *-?[[:digit:]]+.?[[:digit:]]*",
                         "([[:space:]]*,[[:space:]]*-?[[:digit:]]+.?[[:digit:]]*)+")
    are_numvec <- vapply(custom_tags, function(x) grepl(numvec_rxp, x), logical(1))
    custom_tags[are_numvec] <- lapply(
      custom_tags[are_numvec],
      function(x) as.numeric(strsplit(x, ",")[[1]])
    )
    ed2in.text <- modify_ed2in(ed2in.text, .dots = custom_tags, add_if_missing = TRUE, check_paths = check)
  }
  
  ##----------------------------------------------------------------------
  if (check) {
    check_ed2in(ed2in.text)
  }

  barebones <- settings$model$barebones_ed2in
  if (!is.null(barebones) && !isFALSE(barebones) && barebones != "false") barebones <- TRUE
  write_ed2in(ed2in.text, file.path(settings$rundir, run.id, "ED2IN"), barebones = barebones)
} # write.config.ED2
# ==================================================================================================#

##-------------------------------------------------------------------------------------------------#
#' Clear out old config and ED model run files.
#'
#' @param main.outdir ignored
#' @param settings PEcAn settings object
#' @return nothing, removes config files as side effect
#' @export
#' @author Shawn Serbin, David LeBauer, Alexey Shikomanov
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

  # TODO this should come from the database
  
  # Internal data sets stored in sysdata.RDA are used to override defaults in
  # config.xml.  This code looks for a history dataset that matches the
  # "revision" number for ED2 set in settings (e.g. PEcAn.ED2:::history.r85) and
  # if it doesn't find it, it uses a generic file (PEcAn.ED2:::history).  To add
  # a new history file, add the .csv file to models/ed/data-raw and run the
  # sysdata.R script in that folder
  
  if(is.null(settings$model$revision)) {
    PEcAn.logger::logger.debug("--- Using Generic ED2 History File")
    edhistory <- history
  } else {
    histfile <- paste0("history.r", settings$model$revision)
    edhistory <- try(eval(str2lang(histfile)), silent = TRUE)
  } 
  
  if(inherits(edhistory, "try-error")) {
    PEcAn.logger::logger.debug("--- Using Generic ED2 History File")
    edhistory <- history
  } else {
    PEcAn.logger::logger.debug(paste0("--- Using ED2 History File: ", histfile))
  }

  edtraits <- names(edhistory)
  pftmapping <- PEcAn.ED2::pftmapping
  
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

      # This is the ED PFT number (1-17; see comments in ED2IN file
      # for PFT definitions). It is used to set any ED parameters that
      # are not set explicitly from PEcAn.
      pft.number <- settings[["pfts"]][[i]][["ed2_pft_number"]]

      if (is.null(pft.number)) {
        pft.number <- pftmapping$ED[which(pftmapping == pft)]
        PEcAn.logger::logger.debug(glue::glue(
          "Automatically assigning PFT `{pft}` number `{pft.number}` ",
          "based on `pftmapping`. If you want to avoid this behavior, ",
          "set the PFT number explicitly via the `<ed2_pft_number>` XML tag."
        ))
      }

      if (grepl("soil", pft)) {
        utils::data(soil, package = "PEcAn.ED2", envir = environment())
        vals <- as.list(soil)
        names(vals) <- colnames(soil)

        converted.trait.values <- convert.samples.ED(trait.values[[i]])
        vals <- utils::modifyList(vals, converted.trait.values)

        decompositon.xml <- PEcAn.settings::listToXml(vals, "decomposition")
        xml <- XML::append.xmlNode(xml, decompositon.xml)

      } else if (length(pft.number) == 0) {
        PEcAn.logger::logger.severe(glue::glue(
          "Unable to set PFT number automatically. ",
          "PFT `{pft}` was not matched in `pftmapping`. ",
          "Either set the PFT number explicitly via the ",
          "`<ed2_pft_number>` XML tag (recommended), ",
          "or add the PFT to `pftmapping.csv` file in ",
          "`models/ed/data/pftmapping.csv`."
        ))

      } else {
        ## Get default trait values from ED history
        vals <- as.list(edhistory[edhistory$num == pft.number, ])

        ## Convert trait values to ED units
        converted.trait.values <- convert.samples.ED(trait.values[[i]])

        ## Selectively replace defaults with trait values
        vals <- utils::modifyList(vals, converted.trait.values)

        ## Convert settings constants to ED units
        converted.defaults <- convert.samples.ED(defaults[[pft]]$constants)

        ## Selectively replace defaults and trait values with constants from settings
        if (!is.null(converted.defaults)){
          vals <- utils::modifyList(vals, converted.defaults)
        }
        
        ## Make sure that include_pft is set to 1
        vals$include_pft = 1

        pft.xml <- PEcAn.settings::listToXml(vals, "pft")
        xml <- XML::append.xmlNode(xml, pft.xml)
      }

    }
  }
  return(xml)
} # write.config.xml.ED2

# ==================================================================================================#
#' @name write.config.jobsh.ED2
#' @title Write ED2 job.sh file
#' @description Function for writing job.sh file for ED2 runs
#' @details Refactored by Alexey Shiklomanov to allow use in PEcAn RTM module.
#' @export
#' @param settings PEcAn settings list. For this function, need the following:
#' run$host$rundir, run$host$outdir, run$host$scratchdir,
#' run$host$clearscratch, model$jobtemplate, model$job.sh, run$host$job.sh,
#' run$site$lat, run$site$lon, run$inputs$met$path, run$start.date,
#' run$end.date, model$binary, model$binary_args
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
  
  if (is.null(settings$model$binary_args)) {
    # If argument is missing but running on RabbitMQ, assume you need
    # -s flag. If you want to force run ED without -s, use a blank
    # binary_args tag.
    if (!is.null(settings$host$rabbitmq)) {
      settings$model$binary_args <- "-s"
    } else {
      settings$model$binary_args <- ""
    }
  }
  jobsh <- gsub("@BINARY_ARGS@", settings$model$binary_args, jobsh)
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  
  pft_names <- extract_pfts(settings$pfts)
  pft_names <- deparse1(dput(pft_names))
  jobsh <- gsub("@PFT_NAMES@", pft_names, jobsh)
  
  return(jobsh)
} # write.config.jobsh.ED2
