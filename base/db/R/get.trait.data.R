#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------------#
##' Check two lists. Identical does not work since one can be loaded
##' from the database and the other from a CSV file.
##'
##' @name check.lists
##' @title Compares two lists
##' @param x first list
##' @param y second list
##' @return true if two list are the same
##' @author Rob Kooper
##'
check.lists <- function(x, y) {
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  cols <- c('id', 'genus', 'species', 'scientificname')
  xy_match <- vapply(cols, function(i) identical(as.character(x[[i]]), as.character(y[[i]])), logical(1))
  return(all(unlist(xy_match)))
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database for a single pft
##'
##' @name get.trait.data.pft
##' @title Gets trait data from the database
##' @param pft the pft whos traits to retrieve
##' @param modeltype type of model that is used, this is is used to distinguis between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @return updated pft with posteriorid
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
##'
get.trait.data.pft <- function(pft, modeltype, dbfiles, dbcon,
                               forceupdate = FALSE,
                               trait.names = traitdictionary$id) {

  # Create directory if necessary
  if (!file.exists(pft$outdir) && !dir.create(pft$outdir, recursive = TRUE)) {
    PEcAn.logger::logger.error(paste0("Couldn't create PFT output directory: ", pft$outdir))
  }

  ## Remove old files.  Clean up.
  old.files <- list.files(path = pft$outdir, full.names = TRUE, include.dirs = FALSE)
  file.remove(old.files)

  # find appropriate pft
  if (is.null(modeltype)) {
    pftid <- db.query(
      query = paste0("SELECT id FROM pfts WHERE name='", pft$name, "'"),
      con = dbcon
    )[['id']]
  } else {
    pftid <- db.query(
      query = paste0(
        "SELECT pfts.id FROM pfts, modeltypes WHERE pfts.name='", pft$name,
        "' and pfts.modeltype_id=modeltypes.id and modeltypes.name='", modeltype,
        "'"
      ),
      con = dbcon
    )[['id']]
  }
  if (is.null(pftid)) {
    PEcAn.logger::logger.severe("Could not find pft, could not store file", filename)
    return(NA)
  }

  # get the species, we need to check if anything changed
  species <- PEcAn.DB::query.pft_species(pft = pft$name, modeltype = modeltype, con = dbcon)
  spstr <- PEcAn.utils::vecpaste(species$id)

  # get the priors
  prior.distns <- PEcAn.DB::query.priors(pft = pftid, trstr = PEcAn.utils::vecpaste(trait.names), out = pft$outdir, con = dbcon)
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in% names(pft$constants)),]
  traits <- rownames(prior.distns)

  # get the trait data (don't bother sampling derived traits until after update check)
  trait.data.check <- PEcAn.DB::query.traits(spstr = spstr, priors = traits, con = dbcon, update.check.only = TRUE)
  traits <- names(trait.data.check)

  # Set forceupdate FALSE if it's a string (backwards compatible with 'AUTO' flag used in the past)
  if (!is.logical(forceupdate)) {
    forceupdate <- FALSE
  }

  # check to see if we need to update
  if (!forceupdate) {
    if (is.null(pft$posteriorid)) {
      pft$posteriorid <- db.query(
        query = paste0(
          "SELECT id FROM posteriors WHERE pft_id=", pftid,
          " ORDER BY created_at DESC LIMIT 1"
        ),
        con = dbcon
      )[['id']]
    }
    if (!is.null(pft$posteriorid)) {
      files <- dbfile.check(type = 'Posterior', container.id = pft$posteriorid, con = dbcon)
      ids <- match(c('trait.data.Rdata', 'prior.distns.Rdata', 'species.csv'), files$file_name)
      if (!any(is.na(ids))) {
        foundallfiles <- TRUE
        for(id in ids) {
          PEcAn.logger::logger.info(files$file_path[[id]], files$file_name[[id]])
          if (!file.exists(file.path(files$file_path[[id]], files$file_name[[id]]))) {
            foundallfiles <- FALSE
            PEcAn.logger::logger.error("can not find posterior file: ", file.path(files$file_path[[id]], files$file_name[[id]]))
          } else if (files$file_name[[id]] == "species.csv") {
            PEcAn.logger::logger.debug("Checking if species have changed")
            testme <- read.csv(file = file.path(files$file_path[[id]], files$file_name[[id]]))
            if (!check.lists(species, testme)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("species have changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if (files$file_name[[id]] == "prior.distns.Rdata") {
            PEcAn.logger::logger.debug("Checking if priors have changed")
            prior.distns.tmp <- prior.distns
            if(file.exists(files$file_path[[id]], files$file_name[[id]])){
              load(file.path(files$file_path[[id]], files$file_name[[id]]))#HERE IS THE PROBLEM
            }else{
              PEcAn.logger::logger.debug("Prior file does not exist. If empty (zero-byte) input file error is recived, set forceupdate to TRUE for one run.")
            }
            testme <- prior.distns
            prior.distns <- prior.distns.tmp
            if (!identical(prior.distns, testme)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("priors have changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if (files$file_name[[id]] == "trait.data.Rdata") {
            PEcAn.logger::logger.debug("Checking if trait data has changed")
            load(file.path(files$file_path[[id]], files$file_name[[id]]))

            # For trait data including converted data, only check unconverted
            converted.stats2na <- function(x) {
              if (all(c("mean", "stat", "mean_unconverted", "stat_unconverted") %in% names(x)))
                x[,c("mean","stat")] <- NA
              return(x)
            }
            trait.data <- lapply(trait.data, converted.stats2na)
            trait.data.check <- lapply(trait.data.check, converted.stats2na)

            if (!identical(trait.data.check, trait.data)) {
              foundallfiles <- FALSE
              PEcAn.logger::logger.error("trait data has changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(trait.data, trait.data.check)
          }
        }
        if (foundallfiles) {
          PEcAn.logger::logger.info("Reusing existing files from posterior", pft$posteriorid, "for", pft$name)
          for (id in seq_len(nrow(files))) {
            file.copy(from = file.path(files[[id, 'file_path']], files[[id, 'file_name']]),
                      to = file.path(pft$outdir, files[[id, 'file_name']]))
          }

          # May need to symlink the generic post.distns.Rdata to a specific post.distns.*.Rdata file.
          if (length(dir(pft$outdir, "post.distns.Rdata")) == 0) {
            all.files <- dir(pft$outdir)
            post.distn.file <- all.files[grep("post.distns.*.Rdata", all.files)]
            if (length(post.distn.file) > 1)
              stop("get.trait.data.pft() doesn't know how to handle multiple post.distns.*.Rdata files")
            else if (length(post.distn.file) == 1) {
              # Found exactly one post.distns.*.Rdata file. Use it.
              file.symlink(from = file.path(pft$outdir, post.distn.file),
                           to = file.path(pft$outdir, 'post.distns.Rdata')
              )
            }
          }
          return(pft)
        }
      }
    }
  }

  # get the trait data (including sampling of derived traits, if any)
  trait.data <- query.traits(spstr, traits, con = dbcon, update.check.only = FALSE)
  traits <- names(trait.data)

  # get list of existing files so they get ignored saving
  old.files <- list.files(path = pft$outdir)

  # create a new posterior
  now <- format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  db.query(query = paste0("INSERT INTO posteriors (pft_id, created_at, updated_at) VALUES (", pftid, ", '", now, "', '", now, "')"),
           con = dbcon)
  pft$posteriorid <- db.query(query = paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND created_at='", now, "'"),
                              con = dbcon)[['id']]

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## 1. get species list based on pft
  write.csv(species, file.path(pft$outdir, "species.csv"), row.names = FALSE)

  ## save priors
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  write.csv(prior.distns,
            file = file.path(pft$outdir, "prior.distns.csv"), row.names = TRUE)

  ## 3. display info to the console
  PEcAn.logger::logger.info('Summary of Prior distributions for: ', pft$name)
  PEcAn.logger::logger.info(colnames(prior.distns))
  apply(X = cbind(rownames(prior.distns), prior.distns), MARGIN = 1, FUN = PEcAn.logger::logger.info)

  ## traits = variables with prior distributions for this pft
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  write.csv(plyr::ldply(trait.data),
            file = file.path(pft$outdir, "trait.data.csv"), row.names = FALSE)

  PEcAn.logger::logger.info("number of observations per trait for", pft$name)
  for (t in names(trait.data)) {
    PEcAn.logger::logger.info(nrow(trait.data[[t]]), "observations of", t)
  }


  ### save and store in database all results except those that were there already
  for (file in list.files(path = pft$outdir)) {
    if (file %in% old.files) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    dbfile.insert(in.path = pathname, in.prefix = file, type = 'Posterior', id = pft$posteriorid, con = dbcon)
  }

  return(pft)
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database.
##'
##' This will use the following items from setings:
##' - settings$pfts
##' - settings$model$type
##' - settings$database$bety
##' - settings$database$dbfiles
##' - settings$meta.analysis$update
##' @name get.trait.data
##' @title Gets trait data from the database
##' @param pfts the list of pfts to get traits for
##' @param modeltype type of model that is used, this is is used to distinguis between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @param forceupdate set this to true to force an update, false to check to see if an update is needed.
##' @param trait.names list of traits to query. If TRUE, uses trait.dictionary
##' @return list of pfts with update posteriorids
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function(pfts, modeltype, dbfiles, database, forceupdate, trait.names=NULL) {
  if (!is.list(pfts)) {
    PEcAn.logger::logger.severe('pfts must be a list')
  }
  # Check that all PFTs have associated outdir entries
  pft_outdirs <- lapply(pfts, '[[', 'outdir')
  if (any(sapply(pft_outdirs, is.null))) {
    PEcAn.logger::logger.severe('At least one pft in settings is missing its "outdir"')
  }
  ##---------------- Load trait dictionary --------------#
  if (is.logical(trait.names)) {
    if (trait.names) {
      data(trait.dictionary, package = "PEcAn.utils")
      trait.names <- trait.dictionary$id
    }
  }

  # process all pfts
  dbcon <- db.open(database)
  on.exit(db.close(dbcon))
  result <- lapply(pfts, get.trait.data.pft,
                   modeltype = modeltype,
                   dbfiles = dbfiles,
                   dbcon = dbcon,
                   forceupdate = forceupdate,
                   trait.names = trait.names)

  invisible(result)
}
##==================================================================================================#

##' Get trait data for all PFTs in a settings list
##'
##' @param settings PEcAn configuration list. Must have class `Settings` or `MultiSettings`
##' @export
runModule.get.trait.data <- function(settings) {
  if (is.null(settings$meta.analysis)) return(settings) ## if there's no MA, there's no need for traits
  if (PEcAn.settings::is.MultiSettings(settings)) {
    pfts <- list()
    pft.names <- character(0)
    for (i in seq_along(settings)) {
      pfts.i <- settings[[i]]$pfts
      if (!is.list(pfts.i)) {
        PEcAn.logger::logger.severe("settings[[i]]$pfts is not a list")
      }
      pft.names.i <- sapply(pfts.i, function(x) x$name)
      ind <- which(pft.names.i %in% setdiff(pft.names.i, pft.names))
      pfts <- c(pfts, pfts.i[ind])
      pft.names <- sapply(pfts, function(x) x$name)
    }

    PEcAn.logger::logger.info(paste0("Getting trait data for all PFTs listed by any Settings object in the list: ",
                paste(pft.names, collapse = ", ")))

    modeltype <- settings$model$type
    dbfiles <- settings$database$dbfiles
    database <- settings$database$bety
    forceupdate <- ifelse(is.null(settings$meta.analysis$update), FALSE, settings$meta.analysis$update)
    settings$pfts <- get.trait.data(pfts = pfts, modeltype = modeltype, dbfiles = dbfiles, database = database, forceupdate = forceupdate)
    return(settings)
  } else if (PEcAn.settings::is.Settings(settings)) {
    pfts <- settings$pfts
    if (!is.list(pfts)) {
      PEcAn.logger::logger.severe("settings$pfts is not a list")
    }
    modeltype <- settings$model$type
    dbfiles <- settings$database$dbfiles
    database <- settings$database$bety
    forceupdate <- ifelse(is.null(settings$meta.analysis$update), FALSE, settings$meta.analysis$update)
    settings$pfts <- get.trait.data(pfts = pfts, modeltype = modeltype, dbfiles = dbfiles, database = database, forceupdate = forceupdate)
    return(settings)
  } else {
    stop("runModule.get.trait.data only works with Settings or MultiSettings")
  }
}



####################################################################################################
### EOF.  End of R script file.
####################################################################################################
