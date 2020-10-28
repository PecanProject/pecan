library(magrittr, include.only = "%>%")

#' List models available on a specific machine
#'
#' @param machine_name Target machine hostname. Default = `"docker"`
#' @param machine_id Target machine ID. If `NULL` (default), deduced from hostname.
#' @param dbcon Database connection object. Default is global database pool.
#' @return `data.frame` of information on available models
#' @author Alexey Shiklomanov
#* @get /
availableModels <- function(machine_name = "docker", machine_id = NULL,
                            dbcon = global_db_pool) {
  if (is.null(machine_id)) {
    machines <- dplyr::tbl(dbcon, "machines")
    machineid <- machines %>%
      dplyr::filter(hostname == !!machine_name) %>%
      dplyr::pull(id)
    if (length(machineid) > 1) {
      stop("Found ", length(machineid), " machines with name ", machine_name)
    }
    if (length(machineid) < 1) {
      stop("Found no machines with name ", machine_name)
    }
  }
  dbfiles <- dplyr::tbl(dbcon, "dbfiles") %>%
    dplyr::filter(machine_id == !!machineid)
  modelfiles <- dbfiles %>%
    dplyr::filter(container_type == "Model")
  models <- dplyr::tbl(dbcon, "models")
  modeltypes <- dplyr::tbl(dbcon, "modeltypes") %>%
    dplyr::select(modeltype_id = id, modeltype = name)

  modelfiles %>%
    dplyr::select(dbfile_id = id, file_name, file_path,
                  model_id = container_id) %>%
    dplyr::inner_join(models, c("model_id" = "id")) %>%
    dplyr::inner_join(modeltypes, "modeltype_id") %>%
    dplyr::collect()
}
