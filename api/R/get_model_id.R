#' Retrieve database ID of a particular version of a model
#'
#' @param con Database connection object (Pqconnection)
#' @param name Model name (character)
#' @param revision Model version/revision (character)
#' @param multi_action Action to take if multiple models found
#'   (character). Must be one of "first", "last" (default), "all", or "error".
#' @return Model ID, as `integer64`
#' @author Alexey Shiklomanov
#' @export
get_model_id <- function(con, name, revision, multi_action = "last") {
  qry <- DBI::dbSendQuery(con, paste(
    "SELECT id FROM models WHERE model_name = $1 and revision = $2 ORDER BY id DESC"
  ))
  res <- DBI::dbBind(qry, list(name, revision))
  on.exit(DBI::dbClearResult(res))
  id <- DBI::dbFetch(res)[["id"]]
  if (length(id) == 0) {
    stop("Model ", name, " with revision ", revision, " not found.")
  }
  if (length(id) > 1) {
    warning("Multiple models with name ", name, " and revision ", revision, "found. ",
            "Returning ", multi_action, " result.")
    if (multi_action == "first") {
      id <- head(id, 1)
    } else if (multi_action == "last") {
      id <- tail(id, 1)
    } else if (multi_action == "all") {
      # Return all IDs -- leave as is
    } else if (multi_action == "error") {
      stop("Multiple models found, and 'error' action selected.")
    } else {
      stop("Unknown multi_action: ", multi_action)
    }
  }
  id
}
