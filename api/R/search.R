#' Search for sites or models
#'
#' @inheritParams param_query
#' @param name Model/PFT (depending on function) name search string (character)
#' @param sitename Model name search string (character)
#' @param definition PFT definition search string (character)
#' @param modeltype Model type search string (character)
#' @param revision Model version search string (character)
#' @param auto_pct Logical. If `TRUE` (default), automatically
#'   surround search strings in `%`. If this is `FALSE`, you should
#'   explicitly specify `"%%"` for one or both other arguments.
#' @param ignore.case Logical. If `TRUE` (default) use
#'   case-insensitive search (SQL `ILIKE` operator); otherwise, use
#'   case-sensitive search (SQL `LIKE` operator).
#' @return Bety `models` table (`data.frame`) subset to matching model
#'   name or version
#' @author Alexey Shiklomanov
#' @examples
#' \dontrun{
#' search_models(con, "SIPNET")
#'
#' # Partial match
#' search_models(con, "ED")
#' search_models(con, modeltype = "ED")
#' search_sites(con, "UMBS")
#' search_pfts(con, "early", modeltype = "ED")
#'
#' # Case sensitivity
#' search_models(con, "ed")
#' search_models(con, "ed", ignore.case = FALSE)
#'
#' # Starts with UMBS
#' search_sites(con, "UMBS%", auto_pct = FALSE)
#'
#' # SQL wildcards can still be used inside search strings.
#' search_pfts(con, "early%hardwood")
#' }
#' @rdname search
#' @export
search_models <- function(con, name = "", revision = "", modeltype = "", auto_pct = TRUE, ignore.case = TRUE) {
  if (auto_pct) {
    name <- paste0("%", name, "%")
    revision <- paste0("%", revision, "%")
    modeltype <- paste0("%", modeltype, "%")
  }
  like <- "LIKE"
  if (ignore.case) like <- "ILIKE"
  param_query(con, paste(
    "SELECT models.*, modeltypes.name AS modeltype FROM models",
    "INNER JOIN modeltypes ON (models.modeltype_id = modeltypes.id)",
    "WHERE model_name", like, "$1 AND revision", like, "$2 AND modeltypes.name", like, "$3"
  ), list(name, revision, modeltype))
}

#' @rdname search
#' @export
search_sites <- function(con, sitename = "", auto_pct = TRUE, ignore.case = TRUE) {
  if (auto_pct) {
    sitename <- paste0("%", sitename, "%")
  }
  like <- "LIKE"
  if (ignore.case) like <- "ILIKE"
  param_query(con, paste(
    "SELECT * FROM sites WHERE sitename", like, "$1"
  ), list(sitename))
}

#' @rdname search
#' @export
search_pfts <- function(con, name = "", definition = "", modeltype = "", auto_pct = TRUE, ignore.case = TRUE) {
  if (auto_pct) {
    name <- paste0("%", name, "%")
    definition <- paste0("%", definition, "%")
    modeltype <- paste0("%", modeltype, "%")
  }
  like <- "LIKE"
  if (ignore.case) like <- "ILIKE"
  param_query(con, paste(
    "SELECT pfts.id AS id, pfts.name AS name, pfts.definition AS definition, pfts.pft_type AS pft_type,",
    "modeltypes.name AS modeltype, modeltypes.id AS modeltype_id",
    "FROM pfts INNER JOIN modeltypes",
    "ON (pfts.modeltype_id = modeltypes.id)",
    "WHERE pfts.name", like, "$1 AND pfts.definition", like, "$2 AND modeltypes.name", like, "$3"
  ), list(name, definition, modeltype))
}
