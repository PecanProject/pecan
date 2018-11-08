#' Insert a new workflow into PEcAn database, returning the workflow
#' as a `data.frame`
#'
#' @inheritParams param_query
#' @param site_id Site ID from `sites` table (numeric)
#' @param model_id Model ID from `models` table (numeric)
#' @param start_date Model run start date (character or POSIX)
#' @param end_date Model run end date (character or POSIX)
#' @param user_id User ID from `users` table (numeric or NULL (default))
#' @param hostname Workflow server hostname (character; default =
#'   option `pecanapi.workflow_hostname`)
#' @param folder_prefix Output directory prefix (character; default =
#'   option `pecanapi.workflow_prefix`). Workflow ID will be appended
#'   to the end with `paste0`
#' @param params Additional workflow parameters, stored in
#'   `workflows.params` (character or NULL (default))
#' @param notes Additional workflow notes, stored in `workflows.notes`
#'   (character or NULL (default))
#' @return `data.frame` containing new workflow(s), including all
#'   columns from `workflows` table.
#' @author Alexey Shiklomanov
#' @export
insert_new_workflow <- function(con,
                                site_id,
                                model_id,
                                start_date,
                                end_date,
                                user_id = NULL,
                                hostname = getOption("pecanapi.workflow_hostname"),
                                folder_prefix = getOption("pecanapi.workflow_prefix"),
                                params = NULL,
                                notes = NULL) {
  if (is.null(notes)) notes <- ""
  if (is.null(params)) params <- ""
  stopifnot(
    # Must be scalar 
    length(folder_prefix) == 1,
    length(user_id) <= 1,
    length(con) == 1,
    # Must be RPostgres connection for prepared queries 
    class(con) == "PqConnection"
  )
  lens <- lengths(list(site_id, model_id, start_date, end_date))
  n_workflow <- max(lens)
  if (!all(lens == 1 | lens == n_workflow)) {
    stop(
      "All inputs must be either the same length or length 1. ",
      "You provided the following: ",
      paste(sprintf("%s (%s)", c("site_id", "model_id", "start_date", "end_date"), lens),
            collapse = ", ")
    )
  }
  id <- bit64::integer64()
  for (i in seq_len(n_workflow)) {
    id[i] <- get_next_workflow_id(con)[[1]]
  }
  stopifnot(length(id) >= 1)
  folder <- paste0(folder_prefix, id)
  query_string <- paste(
    "INSERT INTO workflows",
    "(id, site_id, model_id, folder,",
    "hostname, start_date, end_date, params,",
    "notes,",
    (if (!is.null(user_id)) "user_id,"),
    "advanced_edit, started_at, created_at)",
    "VALUES",
    "($1, $2, $3, $4,",
    "$5, $6, $7, $8,",
    "$9,",
    (if (!is.null(user_id)) "$10,"),
    "false, NOW(), NOW())",
    "RETURNING *"
  )
  params <- list(id, site_id, model_id, folder,
                 hostname, start_date, end_date, params,
                 notes)
  if (!is.null(user_id)) params <- c(params, user_id)
  param_query(con, query_string, params)
}

#' Get next workflow ID from sequence. Note that this also updates the
#'   workflow sequence.
#'
#' @inheritParams param_query
#' @return Workflow ID, as numeric/base64 integer
#' @author Alexey Shiklomanov
#' @export
get_next_workflow_id <- function(con) {
  DBI::dbGetQuery(con, "SELECT nextval('workflows_id_seq')")[[1]]
}

#' Set workflow ID sequence to a specific value
#'
#' @inheritParams param_query
#' @param value Workflow ID to which to reset workflow ID sequence (base64 integer)
#' @return Input `value`
#' @author Alexey Shiklomanov
#' @export
set_workflow_id_seq <- function(con, value) {
  stmt <- DBI::dbSendStatement(con, "SELECT setval('workflows_id_seq', $1)")
  res <- DBI::dbBind(stmt, list(value))
  on.exit(DBI::dbClearResult(res))
  invisible(value)
}

#' Reset workflow ID sequence to ID of last workflow in `workflows` table.
#'
#' @inheritParams param_query
#' @return Workflow ID of last workflow (64bit integer)
#' @author Alexey Shiklomanov
#' @export
reset_workflow_id_seq <- function(con) {
  last_workflow <- DBI::dbGetQuery(con, "SELECT * FROM workflows ORDER BY id DESC limit 1")
  last_id <- last_workflow[["id"]]
  set_workflow_id_seq(con, last_id)
}
