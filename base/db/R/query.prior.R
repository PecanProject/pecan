#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Query priors associated with a string of traits and plant functional type
##'
##' @name query.priors
##' @title Query Priors
##' @param pft ID number of the PFT in the database
##' @param con database connection, can be list of arguments for connecting to database
##' @param trstr String of traits to query priors for. If passed as a
##'   character vector, it will be concatenated to a single string
##'   using [PEcAn.utils::vecpaste()].
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return priors for a given pft
##' @export query.priors
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.priors('ebifarm.pavi', vecpaste('SLA', 'Vcmax', 'leaf_width'))
##' }
query.priors <- function(pft, trstr = NULL, con = NULL, ...){

  if (is.null(con)) {
    con <- db.open(settings$database$bety)
    on.exit(db.close(con))
  }
  if (is.list(con)) {
    print("query.priors")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }

  query.text <- paste(
    "select variables.name, distn, parama, paramb, n",
      "from priors",
      "join variables on priors.variable_id = variables.id",
      "join pfts_priors on pfts_priors.prior_id = priors.id",
      "join pfts on pfts.id = pfts_priors.pft_id",
      "where pfts.id = ", pft)

  if (!is.null(trstr) && trstr != "''") {
    if (length(trstr) > 1) {
      PEcAn.logger::logger.debug(paste0(
        "Multiple values passed to `trstr`. ",
        "Concatenating with `PEcAn.utils::vecpaste`."
      ))
      trstr <- PEcAn.utils::vecpaste(trstr)
    }
    query.text <- paste(query.text, "and variables.name in (", trstr, ");")
  }


  priors <- db.query(query = query.text, con = con)


  if(nrow(priors) <= 0){
    warning(paste("No priors found for pft(s): ", pft))
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
  else {
    rownames(priors) <- priors$name
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
}


#' Query priors using prepared statements
#'
#' @param pfts Character vector of PFT names
#' @param 
query_priors <- function(pft_names = NULL, traits = NULL, pft_ids = NULL, expand = TRUE, ...) {
  if (!is.null(pft_names) && !is.null(pft_ids)) {
    PEcAn.logger::logger.severe(
      "Provide either `pft_names` or `pft_ids`, not both."
    )
  }
  query_string <- paste(
    "SELECT variables.name, distn, parama, paramb, n",
    "FROM priors",
    "JOIN variables ON priors.variable_id = variables.id",
    "JOIN pfts_priors ON pfts_priors.prior_id = priors.id",
    "JOIN pfts ON pfts.id = pfts_priors.pft_id",
    "WHERE pfts.name = $1 OR pfts.id = $2"
  )

  if (is.null(traits)) {
    return(db.query(query_string, values = list(pfts), ...))
  }

  query_string <- paste(query_string, "AND variables.name = $3")
  npft <- length(pfts)
  ntrait <- length(traits)
  if (npft != ntrait || npft == 1 || ntrait == 1) {
    if (!expand) {
      PEcAn.logger::logger.severe(sprintf(
        "Expand is `FALSE`, but %d PFTs and %d traits were provided. ",
        "Unclear how to recycle, so throwing an error instead."
      ), npft, ntrait)
    }
    # Query the full trait x PFT combination
    pfts_traits <- expand.grid(pft = pfts, trait = traits, stringsAsFactors = FALSE)
    pfts <- pfts_traits[["pft"]]
    traits <- pfts_traits[["trait"]]
  }
  db.query(query_string, values = list(pfts, traits), ...)
}
