#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
get.trait.data.pft <- function(pft, dbfiles, dbcon, forceupdate, trait.names) {
  ## Remove old files.  Clean up.
  old.files <- list.files(path=pft$outdir, full.names=TRUE, include.dirs=FALSE)
  file.remove(old.files)

  # find appropriate pft
  pftid <- db.query(paste0("SELECT id FROM pfts WHERE name='", pft$name, "'"), dbcon)[['id']]
  if (is.null(pftid)) {
    logger.severe("Could not find pft, could not store file", filename)
    return(NA)
  }

  # check to see if we need to update
  if ((forceupdate == 'AUTO') || !as.logical(forceupdate)) {
    if (is.null(pft$posteriorid)) {
      pft$posteriorid <- db.query(paste0("SELECT id FROM posteriors WHERE pft_id=", pftid, " ORDER BY created_at DESC LIMIT 1"), dbcon)[['id']]  
    }
    if (!is.null(pft$posteriorid)) {
      files <- dbfile.check('Posterior', pft$posteriorid, dbcon)
      ids <- match(c('trait.data.Rdata', 'prior.distns.Rdata', 'species.csv'), files$file_name)
      if (!any(is.na(ids))) {
        foundallfiles <- TRUE
        for(id in ids) {
          if (!file.exists(file.path(files$file_path[[id]], files$file_name[[id]]))) {
            foundallfiles <- FALSE
            break
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
  species <- query.pft_species(pft$name, con=dbcon)
  spstr <- vecpaste(species$id)
  write.csv(species, file.path(pft$outdir, "species.csv"), row.names = FALSE)

  ## 2. get priors available for pft  
  prior.distns <- query.priors(pft$name, vecpaste(trait.names),
                               out = pft$outdir, con = dbcon)
  
  ## exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                     names(pft$constants)),]

  ## save priors
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  write.csv(prior.distns,
            file = file.path(pft$outdir, "prior.distns.csv"), row.names = FALSE)

  ## 3. display info to the console
  logger.info('Summary of Prior distributions for: ', pft$name)
  logger.info(prior.distns)

  ## traits = variables with prior distributions for this pft 
  traits <- rownames(prior.distns) 
  
  trait.data <- query.traits(spstr, traits, con = dbcon)
  traits <- names(trait.data)
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  write.csv(ldply(trait.data),
            file = file.path(pft$outdir, "trait.data.csv"), row.names = FALSE)
  
  logger.info("number of observations per trait for", pft$name)
  logger.info(ldply(trait.data, nrow))

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
##' - settings$database
##' - settings$run$dbfiles
##' - settings$meta.analysis$update
##' @name get.trait.data
##' @title Gets trait data from the database
##' @param pfts the list of pfts to get traits for
##' @param dbfiles location where previous results are found
##' @param database database connection parameters
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @return list of pfts with update posteriorids
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function(pfts, dbfiles, database, forceupdate) {
  ##---------------- Load trait dictionary --------------#
  data(trait.dictionary, package = "PEcAn.utils")
  trait.names <- trait.dictionary$id

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
