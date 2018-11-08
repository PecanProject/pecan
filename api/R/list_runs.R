#' List all runs associated with a particular workflow
#'
#' @inheritParams param_query
#' @param workflow_id ID of target workflow (character or numeric)
#' @return Runs `data.frame` subset to rows containing specific workflow
#' @author Alexey Shiklomanov
#' @export
list_runs <- function(con, workflow_id) {
  param_query(con, paste(
    "SELECT runs.* FROM runs",
    "INNER JOIN ensembles ON (runs.ensemble_id = ensembles.id)",
    "INNER JOIN workflows ON (ensembles.workflow_id = workflows.id)",
    "WHERE workflows.id = $1"
  ), list(workflow_id))
}
