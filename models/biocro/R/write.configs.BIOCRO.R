
PREFIX_XML <- "<?xml version=\"1.0\"?>\n<!DOCTYPE config SYSTEM \"biocro.dtd\">\n"

#' convert parameters from PEcAn database default units to biocro defaults
#'
#' Performs model specific unit conversions on a list of trait values,
#' such as those provided to write.config
#'
#' @param trait.samples a matrix or dataframe of samples from the trait distribution
#' @param biocro_version numeric, but currently only checks whether version is less than 1.0
#' @return dataframe with values transformed
#' @export
#' @author David LeBauer
convert.samples.BIOCRO <- function(trait.samples, biocro_version=1.0) {

  trait.samples <- as.data.frame(trait.samples)
  trait.names <- colnames(trait.samples)

  ## transform values with different units cuticular conductance - BETY default is
  ## umol; BioCro uses mol
  if ("cuticular_cond" %in% trait.names) {
    trait.samples$cuticular_cond = PEcAn.utils::ud_convert(trait.samples$cuticular_cond, "umol", "mol")
  }
  if ("SLA" %in% trait.names) {
    trait.samples$SLA = PEcAn.utils::ud_convert(trait.samples$SLA, "kg/m2", "g/cm2")
  }

  ## rename bety variables to match active version of biocro
  names(trait.names) <- trait.names #looks weird, but naming this vector simplifies indexing below

  name_lookup <- dplyr::tribble(
    ~bety_name, ~biocro_0.9_name, ~biocro_1.0_name,
    "Vcmax", "vmax", "vmax1",
    "Jmax", "jmax", "jmax",
    "leaf_respiration_rate_m2", "Rd", "Rd",
    "cuticular_cond", "b0", "b0",
    "stomatal_slope.BB", "b1", "b1",
    "SLA", "Sp", "iSp",
    "growth_respiration_coefficient", "GrowthRespFraction", "growth_respiration_fraction",
    "extinction_coefficient_diffuse", "kd", "kd",
    "chi_leaf", "chi.l", "chil",
    "quantum_efficiency", "alpha", "alpha"
  )
  name_lookup <- name_lookup[name_lookup$bety_name %in% trait.names,]

  if (biocro_version >= 1.0) {
    biocro_name <- "biocro_1.0_name"
  } else {
    biocro_name <- "biocro_0.9_name"
  }
  trait.names[name_lookup[["bety_name"]]] <- name_lookup[[biocro_name]]
  colnames(trait.samples) <- trait.names

  return(trait.samples)
}  # convert.samples.BIOCRO


#' Writes a configuration files for the biocro model
#'
#' @param defaults named list with default model parameter values
#' @param trait.values named list (or dataframe of trait values)
#'  can either be a data.frame or named list of traits, e.g.
#' \code{data.frame(vmax = 1, b0 = 2)} or \code{list(vmax = 1, b0 = 2)}
#' @param settings pecan settings file configured for BioCro
#' @param run.id integer; a unique identifier for the run.
#' @export
#' @return nothing, writes configuration file as side effect
#' @author David LeBauer
write.config.BIOCRO <- function(defaults = NULL, trait.values, settings, run.id) {

  ## find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, as.character(run.id))
  outdir <- file.path(settings$host$outdir, as.character(run.id))
  if (is.null(settings$host$qsub) && (settings$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }

  ## create launch script (which will create symlink)
  writeLines(c("#!/bin/bash",
               paste(settings$model$job.sh),
               paste("mkdir -p", outdir),
               paste("cd", rundir),
               paste(settings$model$binary, normalizePath(rundir, mustWork = FALSE), normalizePath(outdir, mustWork = FALSE)),
               "if [ $? -ne 0 ]; then",
               "    echo ERROR IN MODEL RUN >&2",
               "    exit 1",
               "fi",
               paste("cp ", file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))),
             con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))

  ## write configuration file
  traits <- convert.samples.BIOCRO(
    trait.samples = trait.values[[settings$pfts$pft$name]],
    biocro_version = settings$model$revision)
  
  pft_member_file <- file.path(settings$pfts$pft$outdir, "species.csv")
  if(!file.exists(pft_member_file)){
    pft_member_file <- file.path(settings$pfts$pft$outdir, "cultivars.csv")
  }
  if(!file.exists(pft_member_file)){
    PEcAn.logger::logger.severe("Can't find PFT info: No species.csv nor cultivars.csv in",
                                settings$pfts$pft$outdir)
  }

  pft_members <- utils::read.csv(pft_member_file)
  genus <- unique(pft_members$genus)
  if (length(genus) > 1) {
    PEcAn.logger::logger.severe("BioCro can not combine multiple genera")
  }

  ### Set defaults
  defaults.file <- defaults$pft$constants$file
  defaults <- NULL
  if (!is.null(defaults.file)) {
    if (grepl("xml", defaults.file)) {
      defaults.xml <- defaults.file
      defaults <- XML::xmlToList(XML::xmlParse(defaults.xml))
    } else if (grepl("RData", defaults.file)) {
      load(defaults.file)
    } else {
      PEcAn.logger::logger.warn("Defaults file", defaults.file, " not found; using package defaults")
      defaults.file <- NULL
    }
  }

  if (is.null(defaults.file) && settings$model$revision >= 1.0 && utils::packageVersion("BioCro") >= 1.0) {
    # Look for defaults provided as datasets in the BioCro model package
     defaults = get_biocro_defaults(genus)
  }

 if (is.null(defaults) && is.null(defaults.file)) {
    defaults.file <- system.file(file.path("extdata/defaults", paste0(tolower(genus), ".xml")),
                                 package = "PEcAn.BIOCRO")
    if(defaults.file != ""){
      defaults <- XML::xmlToList(XML::xmlParse(defaults.file))
    } else{
      PEcAn.logger::logger.severe("no defaults file given and ", genus, "not supported in BioCro")
    }
  }

  if (is.null(defaults)) {
    PEcAn.logger::logger.error("No defaults values set")
  }

  traits.used <- sapply(defaults, is.null)
  for (parm.type in names(defaults)) {
    for (parm in names(defaults[[parm.type]])) {
      if (!is.null(traits[[parm]])) {
        defaults[[parm.type]][[parm]] <- as.character(traits[[parm]])
        traits.used[[parm]] <- TRUE
      }
    }
  }

  defaults$type$name <- settings$pfts$pft$name

  ### Replace Defaults with meta-analysis results
  unused.traits <- !traits.used
  ## a clunky way to only use logger for MEDIAN rather than all runs
  if (any(grepl("MEDIAN", scan(file = file.path(settings$rundir, run.id, "README.txt"),
                               character(0),
                               sep = ":",
                               strip.white = TRUE)))) {
    if (sum(unused.traits) > 0) {
      PEcAn.logger::logger.warn("the following traits parameters are not added to config file:",
                  PEcAn.utils::vecpaste(names(unused.traits)[unused.traits == TRUE]))
    }
  }

  ## this is where soil parms can be set defaults$soilControl$FieldC <-

  ## Put defaults and other parts of config file together
  parms.xml <- PEcAn.settings::listToXml(defaults, "pft")
  location.xml <- PEcAn.settings::listToXml(list(latitude = settings$run$site$lat,
                                 longitude = settings$run$site$lon),
                            "location")
  run.xml <- PEcAn.settings::listToXml(list(start.date = settings$run$start.date,
                            end.date = settings$run$end.date,
                            met.path = settings$run$inputs$met$path,
                            soil.file = settings$run$inputs$soil$path),
                       "run")

  slashdate <- function(x) substr(gsub("-", "/", x), 1, 10)
  simulationPeriod.xml <- PEcAn.settings::listToXml(list(dateofplanting = slashdate(settings$run$start.date),
                                         dateofharvest = slashdate(settings$run$end.date)),
                                    "simulationPeriod")

  config.xml <- XML::xmlNode("config")
  config.xml <- XML::append.xmlNode(config.xml, run.xml)
  config.xml <- XML::append.xmlNode(config.xml, location.xml)
  config.xml <- XML::append.xmlNode(config.xml, simulationPeriod.xml)
  config.xml <- XML::append.xmlNode(config.xml, parms.xml)

  XML::saveXML(config.xml, file = file.path(settings$rundir, run.id, "config.xml"),
          indent = TRUE)
}  # write.config.BIOCRO


#' Clear out previous config and parameter files.
#'
#' @param main.outdir Primary PEcAn output directory (will be depreciated)
#' @param settings PEcAn settings file
#' @return nothing, removes config files as side effect
#' @export
#' @author Shawn Serbin, David LeBauer
remove.config.BIOCRO <- function(main.outdir, settings) {

  ## Remove files on localhost
  if (settings$host$name == "localhost") {
    files   <- paste0(settings$outdir, list.files(path = settings$outdir, recursive = FALSE))  # Need to change this to the run folder when implemented
    files   <- files[-grep("*.xml", files)]  # Keep pecan.xml file
    pft.dir <- strsplit(settings$pfts$pft$outdir, "/")[[1]]
    ln      <- length(pft.dir)
    pft.dir <- pft.dir[ln]
    files <- files[-grep(pft.dir, files)]  # Keep pft folder
    # file.remove(files,recursive=TRUE)
    system(paste("rm -r ", files, sep = "", collapse = " "), ignore.stderr = TRUE)  # remove files/dirs

    ## On remote host
  } else {
    print("*** WARNING: Removal of files on remote host not yet implemented ***")
  }
}  # remove.config.BIOCRO
