##' Get trait data from the database.
##'
##' This will use the following items from settings:
##' - `settings$pfts`
##' - `settings$model$type`
##' - `settings$database$bety`
##' - `settings$database$dbfiles`
##' - `settings$meta.analysis$update`
##' 
##' @param pfts the list of pfts to get traits for
##' @param modeltype type of model that is used, this is is used to distinguish
##'   between different PFTs with the same name.
##' @param dbfiles location where previous results are found
##' @param database database connection parameters (see `params`
##'   argument to [db.query()])
##' @param forceupdate (Logical) If `TRUE`, force a database update
##'   whether or not it is needed. If `FALSE`, only update if an
##'   update is needed.
##' @param write (Logical) If `TRUE` updated posteriors will be written to
##'   BETYdb.  Defaults to FALSE.
##' @param trait.names Character vector of trait names to search. If
##'   `NULL` (default), use all traits that have a prior for at least
##'   one of the `pfts`.
##' @return list of PFTs with update posteriorids
##' @author David LeBauer, Shawn Serbin, Alexey Shiklomanov
##' @export
get.trait.data <-
  function(pfts,
           modeltype,
           dbfiles,
           database,
           forceupdate,
           write = FALSE,
           trait.names = NULL) {
    
  if (!is.list(pfts)) {
    PEcAn.logger::logger.severe('pfts must be a list')
  }
  # Check that all PFTs have associated outdir entries
  pft_outdirs <- lapply(pfts, '[[', 'outdir')
  if (any(sapply(pft_outdirs, is.null))) {
    PEcAn.logger::logger.severe('At least one pft in settings is missing its "outdir"')
  }
  
  dbcon <- db.open(database)
  on.exit(db.close(dbcon), add = TRUE)
  
  if (is.null(trait.names)) {
    PEcAn.logger::logger.debug(paste0(
      "`trait.names` is NULL, so retrieving all traits ",
      "that have at least one prior for these PFTs."
    ))
    pft_names <- vapply(pfts, "[[", character(1), "name")
    pft_ids <- query_pfts(dbcon, pft_names, modeltype, strict = TRUE)[["id"]]
    # NOTE: Use `format` here to avoid implicit (incorrect) coercion
    # to double by `lapply`. This works fine if we switch to
    # `query_priors`, but haven't done so yet because that requires
    # prepared statements and therefore requires the Postgres driver. 
    all_priors_list <- lapply(format(pft_ids, scientific = FALSE), query.priors,
                              con = dbcon, trstr = trait.names)
    trait.names <- unique(unlist(lapply(all_priors_list, rownames)))
    # Eventually, can replace with this:
    # all_priors <- query_priors(pfts, params = database)
    # trait.names <- unique(all_priors[["name"]])
  }
  
  # process all pfts
  result <- lapply(pfts, get.trait.data.pft,
                   modeltype = modeltype,
                   dbfiles = dbfiles,
                   dbcon = dbcon,
                   write = write,
                   forceupdate = forceupdate,
                   trait.names = trait.names)
  
  invisible(result)
}