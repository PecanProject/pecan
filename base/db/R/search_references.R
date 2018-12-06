#' Perform crossref search for a list of references
#'
#' @param queries Character vector of queries
#' @inheritDotParams search_reference_single
#' @inherit search_reference_single description return
#' @export
search_references <- function(queries, ...) {
  search_fun <- search_reference_single %>%
    purrr::partial(...) %>%
    purrr::possibly(otherwise = data.frame(title = "Not found"))
  encodeString(queries) %>%
    purrr::map_dfr(search_fun)
}

#' Perform crossref search for a single reference
#'
#' Requires the `rcrossref` package.
#'
#' @param query Citation string (length 1) to search for DOI
#' @param min_score Minimum match score. Default (85) is fairly strict.
#' @param limit Number of results to return
#' @return `data.frame` containing crossref information converted to match bety citations table.
search_reference_single <- function(query, limit = 1, min_score = 85) {
  stopifnot(length(query) == 1)
  PEcAn.logger::logger.debug("Processing query:\n", query)
  crsearch <- rcrossref::cr_works(query = query, limit = limit)
  if (is.null(crsearch[["data"]])) {
    PEcAn.logger::logger.warn(
      "Error in crossref query. ",
      "Setting title to search string and leaving other fields blank."
    )
    return(tibble::tibble(query = query))
  }
  crdata <- crsearch[["data"]] %>%
    dplyr::mutate(score = as.numeric(score)) %>%
    dplyr::filter(score > min_score)
  if (nrow(crdata) < 1) {
    PEcAn.logger::logger.info(
      "No matches found. ",
      "Setting title to search string and leaving other fields blank.")
    return(tibble::tibble(query = query))
  }
  keep_cols <- c(
    "author",
    "year",
    "title",
    journal = "container.title",
    vol = "volume",
    pg = "page",
    doi = "DOI",
    "score",
    "query"
  )
  proc_search <- crdata %>%
    dplyr::mutate(
      # Get the first author only -- this is the BETY format
      author_family = purrr::map(author, list("family", 1)),
      author_given = purrr::map(author, list("given", 1)),
      author = paste(author_family, author_given, sep = ", "),
      year = gsub("([[:digit:]]{4}).*", "\\1", issued) %>% as.numeric(),
      query = query,
      score = as.numeric(score)
    )
  use_cols <- keep_cols[keep_cols %in% colnames(proc_search)]
  dplyr::select(proc_search, !!!use_cols)
}

