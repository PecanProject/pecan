##' Query Priors
##'
##' Query priors associated with a plant functional type and a set of traits.
##'
##' @details If neither `con` nor `...` are provided, this will try to
##'   connect to BETY using a `settings` object in the current
##'   environment. 
##'
##' @param pft ID number of the PFT in the database
##' @param trstr String of traits to query priors for. If passed as a
##'   character vector, it will be concatenated to a single string
##'   using [PEcAn.utils::vecpaste()].
##' @param con Database connection object.
##' @param ... Optional arguments for connecting to database (e.g.
##'   password, user name, database).
##'
##' @return `data.frame` of priors for each trait and the given PFT.
##' @export query.priors
##' @author David LeBauer, Alexey Shiklomanov
##' @examples
##' \dontrun{
##'   con <- db.open(...)
##'   query.priors("ebifarm.pavi", c("SLA", "Vcmax", "leaf_width"), con = con)
##' }
query.priors <- function(pft, trstr = NULL, con = NULL, ...){

  if (inherits(pft, "integer64")) {
    # Convert to character with correct representation
    pft <- format(pft, scientific = FALSE)
  }
  
  if (is.null(con)) {
    params <- list(...)
    if (!length(params)) {
      PEcAn.logger::logger.severe(
        "No connection (`con`) specified and no connection parameters given in `...`.",
        "Unable to connect to database.")
    }
    con <- db.open(params)
    on.exit(db.close(con), add = TRUE)
  }

  query.text <- paste(
    "SELECT variables.name, distn, parama, paramb, n",
      "FROM priors",
      "JOIN variables ON priors.variable_id = variables.id",
      "JOIN pfts_priors ON pfts_priors.prior_id = priors.id",
      "JOIN pfts ON pfts.id = pfts_priors.pft_id",
      "WHERE pfts.id = ", format(pft, scientific = FALSE))

  if (!is.null(trstr) && trstr != "''") {
    if (length(trstr) > 1) {
      PEcAn.logger::logger.debug(paste0(
        "Multiple values passed to `trstr`. ",
        "Concatenating with `PEcAn.utils::vecpaste`."
      ))
      trstr <- PEcAn.utils::vecpaste(trstr)
    }
    query.text <- paste(query.text, "AND variables.name IN (", trstr, ");")
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
#' @param pft_names Character vector of PFT names (`name` column of
#'   BETY `pfts` table). You cannot pass both this and `pft_ids`.
#' @param traits Character vector of trait names (`name` column of
#'   BETY `traits` table). If `NULL` (default), return information for
#'   all traits available for that PFT. 
#' @param pft_ids Numeric vector of PFT IDs (`id` column of BETY
#'   `pfts` table). You cannot pass both this and `pft_names`.
#' @param expand (Logical) If `TRUE` (default), search every trait-PFT
#'   combination. If `FALSE`, assume that input traits and PFTs are paired.
#' @param strict (Logical) If `TRUE`, throw an error if any of the
#'   input `pft_names/ids` or `traits` are missing from the output. If
#'   `FALSE` (default), only throw a warning.
#' @param ... Additional arguments to [db.query()]
#' @return `data.frame` containing prior information for the given
#'   PFTs and traits.
#' @examples
#' \dontrun{
#'   con <- db.open(...)
#'
#'   # No trait provided, so return all available traits
#'   pdat <- query_priors(
#'     c("temperate.Early_Hardwood", "temperate.North_Mid_Hardwood",
#'       "temperate.Late_Hardwood"),
#'     con = con
#'   )
#'
#'   # Traits provided, so restrict to only those traits. Note that
#'   # because `expand = TRUE`, this will search for these traits for
#'   # every PFT.
#'   pdat2 <- query_priors(
#'     c("Optics.Temperate_Early_Hardwood",
#'       "Optics.Temperate_Mid_Hardwood",
#'       "Optics.Temperate_Late_Hardwood"),
#'     c("leaf_reflect_vis", "leaf_reflect_nir"),
#'     con = con
#'   )
#'
#'   # With `expand = FALSE`, search the first trait for the first PFT,
#'   # the second trait for the second PFT, etc. Note that this means
#'   # PFT and trait input vectors must be the same length.
#'   pdat2 <- query_priors(
#'     c("Optics.Temperate_Early_Hardwood",
#'       "Optics.Temperate_Early_Hardwood",
#'       "Optics.Temperate_Mid_Hardwood",
#'       "Optics.Temperate_Late_Hardwood"),
#'     c("leaf_reflect_vis",
#'       "leaf_reflect_nir",
#'       "leaf_reflect_vis",
#'       "leaf_reflect_nir"),
#'     con = con
#'   )
#' }
#' @export
query_priors <- function(pft_names = NULL, traits = NULL, pft_ids = NULL,
                         expand = TRUE, strict = FALSE, ...) {
  if (!is.null(pft_names) && !is.null(pft_ids)) {
    PEcAn.logger::logger.severe(
      "Provide either `pft_names` or `pft_ids`, not both."
    )
  } 
  if (is.null(pft_names)) {
    # Assume PFT ID
    where_stmt <- "WHERE pfts.id = $1"
    pft_val <- pft_ids
    pft_col <- "pft_id"
  } else if (is.null(pft_ids)) {
    # Assume PFT name
    where_stmt <- "WHERE pfts.name = $1"
    pft_val <- pft_names
    pft_col <- "pft_name"
  }
  query_string <- paste(
    # These columns are back-compatible with `query.traits`
    "SELECT variables.name AS name,",
    "distn, parama, paramb, n,",
    # These columns are not in `query.traits`
    "pfts.id AS pft_id, pfts.name AS pft_name,",
    "variables.id AS variable_id,", "pfts.modeltype_id AS modeltype_id",
    "FROM priors",
    "JOIN variables ON priors.variable_id = variables.id",
    "JOIN pfts_priors ON pfts_priors.prior_id = priors.id",
    "JOIN pfts ON pfts.id = pfts_priors.pft_id",
    where_stmt
  )

  # If all we provide is PFT, we are done here...
  if (is.null(traits)) {
    result <- db.query(query_string, values = list(pft_val), ...)
  } else {
    # ...but if we also pass traits, we do some additional filtering.
    query_string <- paste(query_string, "AND variables.name = $2")
    npft <- length(pft_val)
    ntrait <- length(traits)
    if (npft != ntrait || npft == 1 || ntrait == 1) {
      if (!expand) {
        PEcAn.logger::logger.severe(sprintf(paste0(
          "Expand is `FALSE`, but %d PFTs and %d traits were provided. ",
          "Unclear how to recycle, so throwing an error instead."
        ), npft, ntrait))
      }
      # Query the full trait x PFT combination
      pfts_traits <- expand.grid(pft = pft_val, trait = traits, stringsAsFactors = FALSE)
      pft_val <- pfts_traits[["pft"]]
      traits <- pfts_traits[["trait"]]
    }
    result <- db.query(query_string, values = list(pft_val, traits), ...)
  }

  # Check that all inputs are present in output
  result_pfts <- unique(result[[pft_col]])
  missing_pfts <- setdiff(pft_val, result_pfts)
  missing_traits <- character(0)
  if (!is.null(traits)) {
    result_traits <- unique(result[["name"]])
    missing_traits <- setdiff(traits, result_traits)
  }
  if (length(missing_pfts) + length(missing_traits) > 0) {
    msg <- paste(
      "Data for the following inputs were not found in the database:",
      sprintf("* Trait: '%s'", missing_traits),
      sprintf("* PFT: '%s'", missing_pfts),
      sep = "\n", collapse = "\n"
    )
    if (strict) {
      PEcAn.logger::logger.severe(paste0(
        msg, "\n", "Throwing error because `strict` is `TRUE.`"
      ), wrap = FALSE)
    } else {
      PEcAn.logger::logger.warn(msg, wrap = FALSE)
    }
  }

  result
}
