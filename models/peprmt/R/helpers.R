# setEnsemblePaths leaves all path components other than siteid
# identical across sites. This is an issue for our dataset, because
# each site has a different location and date range.
# To use site-specific grid id, we need to string-replace each siteid

#' Set grid cell names (not exported)
#'
#' @param s settings object
#'
#' @returns updated settings with met paths including grid cell
#' @export
#'
#' @examples
id2grid <- function(s) {
  # replacing in place to preserve names
  for (p in seq_along(s$run$inputs$met$path)) {
    s$run$inputs$met$path[[p]] <- gsub(
      pattern = s$run$site$id,
      replacement = s$run$site$ERA5_grid_cell,
      x = s$run$inputs$met$path[[p]]
    )
  }
  s
}

#' Set start and end dates (not exported)
#'
#' @param s settings object
#'
#' @returns updated settings object with met path including correct years
#'
#' @examples
dates2grid <- function(s) {
  for (p in seq_along(s$run$inputs$met$path)) {
    s$run$inputs$met$path[[p]] <- gsub(
      pattern = "DATES-HERE",
      replacement = paste0(s$run$site$met.start, ".",
                           s$run$site$met.end),
      x = s$run$inputs$met$path[[p]]
    )
  }
  s
}