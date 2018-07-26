#' Retrieve the current CF variables table from `cfconventions.org`
#' and convert it into a `data.frame`
#'
#' @param cf_url URL of CF variables table XML. See also [build_cf_variables_table_url].
#' @return CF variables table, as a `tibble`
#' @author Alexey Shiklomanov
#' @export
get_cf_variables_table <- function(cf_url = build_cf_variables_table_url(57)) {
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
#' Construct a URL to a specific version of the CF variables table XML
#'
#' This uses [sprintf] to construct the URL with the version number as
#' the first argument.
#'
#' @param version CF variables table version number (integer/numeric)
#' @param url_format_string A format string passed to [sprintf]. This
#'   should contain the entire target URL with the version number
#'   replaced by `"%d"`, and _no other string substitutions_.
#' @return Complete URL, as a string
#' @author Alexey Shiklomanov
#' @export
build_cf_variables_table_url <- function(version,
                                         url_format_string = "http://cfconventions.org/Data/cf-standard-names/%d/src/src-cf-standard-name-table.xml") {
  sprintf(url_format_string, version)
}
