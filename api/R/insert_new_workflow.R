#' Insert a new workflow into PEcAn database, returning the workflow
#' as a `data.frame`
#'
#' @inheritParams prepared_query
#' @param site_id Site ID from `sites` table (numeric)
#' @param model_id Model ID from `models` table (numeric)
#' @param start_date Model run start date (character or POSIX)
#' @param end_date Model run end date (character or POSIX)
#' @param user_id User ID from `users` table (default = option
#'   `pecanapi.user_id`). Note that this option is _not set by
#'   default_, and this function will not run without a set `user_id`.
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
                                user_id = getOption("pecanapi.user_id"),
                                hostname = getOption("pecanapi.workflow_hostname"),
                                folder_prefix = getOption("pecanapi.workflow_prefix"),
                                params = NULL,
                                notes = NULL) {
  if (is.null(notes)) notes <- ""
  if (is.null(params)) params <- ""
  if (is.null(user_id)) {
    stop("API-based inserts into the workflows table are not allowed without a user ID. ",
         "Either pass the user_id directly, or set it via `options(pecanapi.user_id = <myuserID>)`")
  }
  stopifnot(
    # Must be scalar 
    length(folder_prefix) == 1,
    length(user_id) <= 1,
    length(con) == 1,
    # Must be RPostgres connection for prepared queries 
    inherits(con, "PqConnection")
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
    "user_id,",
    "advanced_edit)",
    "VALUES",
    "($1, $2, $3, $4,",
    "$5, $6, $7, $8,",
    "$9,",
    "$10,",
    "false)",
    "RETURNING *"
  )
  params <- list(id, site_id, model_id, folder,
                 hostname, start_date, end_date, params,
                 notes, user_id)
  prepared_query(con, query_string, params)
}

#' Get current workflow ID and update internal workflow ID PostgreSQL
#' sequence
#'
#' The `workflows` table has an internal
#' [sequence](https://www.postgresql.org/docs/9.6/sql-createsequence.html)
#' that keeps track of and automatically updates the workflow ID
#' (that's why inserting into the table without explicitly setting a
#' workflow ID is a safe and robust operation). This function is a
#' wrapper around the
#' [`nextval` function](https://www.postgresql.org/docs/9.6/functions-sequence.html),
#' which retrieves the current value of the sequence _and_ augments
#' the sequence by 1.
#'
#' @inheritParams prepared_query
#' @return Workflow ID, as numeric/base64 integer
#' @author Alexey Shiklomanov
#' @export
get_next_workflow_id <- function(con) {
  DBI::dbGetQuery(con, "SELECT nextval('workflows_id_seq')")[[1]]
}
