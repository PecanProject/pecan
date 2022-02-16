library(magrittr, include.only = "%>%")

#' List models available on a specific machine
#'
#' @param machine_name Target machine hostname. Default = `"docker"`
#' @param machine_id Target machine ID. If `NA` (default), deduced from hostname.
#' @return `data.frame` of information on available models
#' @author Alexey Shiklomanov
#* @get /
availableModels <- function(machine_name = "docker", machine_id = NA) {
  if (is.na(machine_id)) {
    machines <- dplyr::tbl(global_db_pool, "machines")
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
  dbfiles <- dplyr::tbl(global_db_pool, "dbfiles") %>%
    dplyr::filter(machine_id == !!machineid)
  modelfiles <- dbfiles %>%
    dplyr::filter(container_type == "Model")
  models <- dplyr::tbl(global_db_pool, "models")
  modeltypes <- dplyr::tbl(global_db_pool, "modeltypes") %>%
    dplyr::select(modeltype_id = id, modeltype = name)

  modelfiles %>%
    dplyr::select(dbfile_id = id, file_name, file_path,
                  model_id = container_id) %>%
    dplyr::inner_join(models, c("model_id" = "id")) %>%
    dplyr::inner_join(modeltypes, "modeltype_id") %>%
    dplyr::collect()
}
