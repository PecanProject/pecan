#' Retrieve PFT ID, name, and type from BETY
#'
#' @param dbcon Database connection object
#' @param pft_names character vector of PFT names
#' @param modeltype character.
#'   If specified, only returns PFTs matching this modeltype.
#'   If NULL, considers all modeltypes.
#' @return `data.frame` containing PFT ID (`id`), type (`pft_type`),
#'   and name (`name`).
#' @author Alexey Shiklomanov, Chris Black
#' @export
query_pfts <- function(dbcon, pft_names, modeltype = NULL, strict = FALSE) {
  pftres <- (dplyr::tbl(dbcon, "pfts")
    %>% dplyr::filter(name %in% !!pft_names))
  if (!is.null(modeltype)) {
    pftres <- (pftres %>% dplyr::semi_join(
    (dplyr::tbl(dbcon, "modeltypes") %>% dplyr::filter(name == !!modeltype)),
    by = c("modeltype_id" = "id")))
  }
  result <- (pftres
    %>% dplyr::select(.data$id, .data$pft_type, .data$name)
    %>% dplyr::collect()
    # Arrange in order of inputs
    %>% dplyr::slice(match(.data$name, pft_names)))

  if (!setequal(pft_names, result[["name"]])) {
    missing_pfts <- setdiff(pft_names, result[["name"]])
    msg <- paste(
      "The following PFTs were not found in the database:",
      paste0("'", missing_pfts, "'", collapse = ", ")
    )
    if (strict) {
      PEcAn.logger::logger.severe(
        "Strict matching requested, but failed with message:\n",
        msg, wrap = FALSE
      )
    } else {
      PEcAn.logger::logger.warn(msg, wrap = FALSE)
    }
  }
  result
}
