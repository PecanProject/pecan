#' Retrieve the current CF variables table from `cfconventions.org`
#' and convert it into a `data.frame`
#'
#' @param cf_url URL of CF variables table XML
#' @return CF variables table, as a `tibble`
#' @author Alexey Shiklomanov
#' @export
get_cf_variables_table <- function(cf_url = "http://cfconventions.org/Data/cf-standard-names/57/src/cf-standard-name-table.xml") {
  raw_xml <- XML::xmlParse(cf_url)
  list_xml <- XML::xmlToList(raw_xml)
  entries <- list_xml[names(list_xml) == "entry"]
  entries_flat <- purrr::map(entries, unlist)
  entries_df <- purrr::map(entries, as.list) %>%
    purrr::transpose() %>%
    purrr::map(~ifelse(purrr::map_lgl(.x, is.null), NA, .x)) %>%
    purrr::map_dfc(unlist, recursive = TRUE)
  entries_df %>%
    dplyr::select(
      cf_standard_name = .attrs,
      unit = canonical_units,
      description,
      dplyr::everything()
    )
}
