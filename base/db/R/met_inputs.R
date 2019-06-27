#' Retrieve available met inputs for the given site, model, and hostname
#'
#' This is identical to the query performed by `web/03-inputs.php` to
#' populate the list of available sites. It may be useful for
#' debugging, or otherwise replicating PEcAn web interface behavior.
#'
#' @param dbcon Database connection object
#' @param site_id Site ID (from `sites` table)
#' @param model_id Model ID (from `models`) table
#' @param hostname Hostname of machine
#' @return `data.frame` of available met inputs
#' @author Alexey Shiklomanov
#' @export
met_inputs <- function(dbcon, site_id, model_id, hostname) {
  query_text <- paste0(
    "SELECT tag, inputs.name AS input_name, inputs.id, dbfiles.file_name,",
    " sites.sitename, inputs.start_date, inputs.end_date",
    " FROM sites, inputs, dbfiles, machines,",
    " modeltypes_formats, models, formats",
    # NOTE: 1118 is the site_id for the "earth" site
    " WHERE (inputs.site_id = 1118 OR inputs.site_id = $1)",
    " AND inputs.id = dbfiles.container_id",
    " AND dbfiles.container_type = 'Input'",
    " AND dbfiles.machine_id = machines.id",
    " AND machines.hostname = $2",
    " AND inputs.format_id = modeltypes_formats.format_id",
    " AND inputs.site_id = sites.id",
    " AND modeltypes_formats.modeltype_id = models.modeltype_id",
    " AND models.id = $3",
    " AND modeltypes_formats.input",
    " AND formats.id = inputs.format_id"
  )
  result <- db.query(query_text, con = dbcon, values = list(site_id, "docker", model_id))
  result
}
