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
  if (!identical(as.character(x$id), as.character(y$id))) {
    return(FALSE)
  }
  if (!identical(as.character(x$genus), as.character(y$genus))) {
    return(FALSE)
  }
  if (!identical(as.character(x$species), as.character(y$species))) {
    return(FALSE)
  }
  if (!identical(as.character(x$scientificname), as.character(y$scientificname))) {
    return(FALSE)
  }
  return(TRUE)
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database for a single pft
##'
##' @name get.trait.data.pft
##' @title Gets trait data from the database
##' @param pft the pft whos traits to retrieve
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @return updated pft with posteriorid
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
##'
get.trait.data.pft <- function(pft, dbfiles, dbcon,
                               forceupdate = TRUE,
                               trait.names = traitdictionary$id) {
  ## Remove old files.  Clean up.
  old.files <- list.files(path=pft$outdir, full.names=TRUE, include.dirs=FALSE)
  file.remove(old.files)

  # find appropriate pft
  pftid <- db.query(paste0("SELECT id FROM pfts WHERE name='", pft$name, "'"), dbcon)[['id']]
  if (is.null(pftid)) {
    logger.severe("Could not find pft, could not store file", filename)
    return(NA)
  }

  # get the species, we need to check if anything changed
  species <- query.pft_species(pft$name, con=dbcon)
  spstr <- vecpaste(species$id)

  # get the priors
  prior.distns <- query.priors(pft$name, vecpaste(trait.names), out = pft$outdir, con = dbcon)
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in% names(pft$constants)),]
  traits <- rownames(prior.distns) 

  # get the trait data
  trait.data <- query.traits(spstr, traits, con = dbcon)
  traits <- names(trait.data)

  # check to see if we need to update
  if ((forceupdate == 'AUTO') || !as.logical(forceupdate)) {
    if (is.null(pft$posteriorid)) {
      pft$posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " ORDER BY created_at DESC LIMIT 1"), dbcon)[['id']]  
    }
    if (!is.null(pft$posteriorid)) {
      db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " ORDER BY created_at DESC LIMIT 1"), dbcon)[['id']]   
      files <- dbfile.check('Posterior', pft$posteriorid, dbcon)
      ids <- match(c('trait.data.Rdata', 'prior.distns.Rdata', 'species.csv'), files$file_name)
      if (!any(is.na(ids))) {
        foundallfiles <- TRUE
        for(id in ids) {
          logger.info(files$file_path[[id]], files$file_name[[id]])
          if (!file.exists(file.path(files$file_path[[id]], files$file_name[[id]]))) {
            foundallfiles <- FALSE
            logger.error("can not find posterior file: ", file.path(files$file_path[[id]], files$file_name[[id]]))
          } else if ((forceupdate == 'AUTO') && (files$file_name[[id]] == "species.csv")) {
            logger.debug("Checking if species have changed")
            testme <- read.csv(file.path(files$file_path[[id]], files$file_name[[id]]))
            if (!check.lists(species, testme)) {
              remove(testme)
              foundallfiles <- FALSE
              logger.error("species have changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if ((forceupdate == 'AUTO') && (files$file_name[[id]] == "prior.distns.Rdata")) {
            logger.debug("Checking if priors have changed")
            prior.distns.tmp <- prior.distns
            load(file.path(files$file_path[[id]], files$file_name[[id]]))
            testme <- prior.distns
            prior.distns <- prior.distns.tmp
            if (!identical(prior.distns, testme)) {
              remove(testme)
              foundallfiles <- FALSE
              logger.error("priors have changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          } else if ((forceupdate == 'AUTO') && (files$file_name[[id]] == "trait.data.Rdata")) {
            logger.debug("Checking if trait data has changed")
            trait.data.tmp <- trait.data
            load(file.path(files$file_path[[id]], files$file_name[[id]]))
            testme <- trait.data
            trait.data <- trait.data.tmp
            if (!identical(trait.data, testme)) {
              remove(testme)
              foundallfiles <- FALSE
              logger.error("trait data has changed: ", file.path(files$file_path[[id]], files$file_name[[id]]))
            }
            remove(testme)
          }
        }
        if (foundallfiles) {
          logger.info("Reusing existing files from posterior", pft$posteriorid, "for", pft$name)
          for(id in 1:nrow(files)) {
            file.copy(file.path(files[[id, 'file_path']], files[[id, 'file_name']]), file.path(pft$outdir, files[[id, 'file_name']]))
          }
          return(pft)
        }
      }
    }
  }

  # get list of existing files so they get ignored saving
  old.files <- list.files(path=pft$outdir)

  # create a new posterior
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  db.query(paste0("INSERT INTO posteriors (pft_id, created_at, updated_at) VALUES (", pftid, ", '", now, "', '", now, "')"), dbcon)
  pft$posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " AND created_at='", now, "'"), dbcon)[['id']]

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## 1. get species list based on pft
  write.csv(species, file.path(pft$outdir, "species.csv"), row.names = FALSE)

  ## save priors
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  write.csv(prior.distns,
            file = file.path(pft$outdir, "prior.distns.csv"), row.names = FALSE)

  ## 3. display info to the console
  logger.info('Summary of Prior distributions for: ', pft$name)
  logger.info(colnames(prior.distns))
  apply(cbind(rownames(prior.distns), prior.distns), MARGIN=1, logger.info)

  ## traits = variables with prior distributions for this pft 
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  write.csv(ldply(trait.data),
            file = file.path(pft$outdir, "trait.data.csv"), row.names = FALSE)
  
  logger.info("number of observations per trait for", pft$name)
  for(t in names(trait.data)){
    logger.info(nrow(trait.data[[t]]), "observations of", t)
  }
    

  ### save and store in database all results except those that were there already
  for(file in list.files(path=pft$outdir)) {
    if (file %in% old.files) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    dbfile.insert(filename, 'Posterior', pft$posteriorid, dbcon)
  }

  return(pft)
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database.
##'
##' This will use the following items from setings:
##' - settings$pfts
##' - settings$database$bety
##' - settings$run$dbfiles
##' - settings$meta.analysis$update
##' @name get.trait.data
##' @title Gets trait data from the database
##' @param pfts the list of pfts to get traits for
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @param trait.names list of traits to query. If TRUE, uses trait.dictionary
##' @return list of pfts with update posteriorids
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function(pfts, dbfiles, database, forceupdate,trait.names=NULL) {
  ##---------------- Load trait dictionary --------------#
  if(is.logical(trait.names)){
    if(trait.names){
      data(trait.dictionary, package = "PEcAn.utils")
      trait.names <- trait.dictionary$id
    }
  }

  # process all pfts
  dbcon <- db.open(database)
  result <- lapply(pfts, get.trait.data.pft, dbfiles, dbcon, forceupdate, trait.names)
  db.close(dbcon)

  invisible(result)
}
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################
