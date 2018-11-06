#' Add information from workflow data frame into settings list. 
#'
#' @param settings Partially completed PEcAn `settings` list.
#' @param workflow_df Workflow `data.frame`, such as that returned by
#'   [insert_new_workflow], or from running query `SELECT * FROM
#'   workflows`
#' @param overwrite Whether or not to overwrite existing `settings`
#'   tags (logical; default = `FALSE`)
#' @return PEcAn settings list with workflow information added
#' @author Alexey Shiklomanov
#' @export
add_workflow <- function(settings, workflow_df, overwrite = FALSE) {
  workflow_settings <- list(
    workflow = list(id = workflow_df[["id"]]),
    outdir = workflow_df[["folder"]],
    model = list(id = workflow_df[["model_id"]]),
    run = list(
      site = list(id = workflow_df[["site_id"]],
                  met.start = workflow_df[["start_date"]],
                  met.end = workflow_df[["end_date"]]),
      start.date = workflow_df[["start_date"]],
      end.date = workflow_df[["end_date"]]
    ),
    info = list(notes = workflow_df[["notes"]])
  )
  new_settings <- modifyList(settings, workflow_settings)
  if (!overwrite) {
    new_settings <- modifyList(new_settings, settings)
  }
  new_settings
}
