#' Report installed PEcAn version
#'
#' Reports the currently installed version of each PEcAn package, as well the
#'  versions expected by the specified release(s) of the PEcAn system.
#'
#' When `exact` = FALSE (the default), `pecan_version` will show all releases
#'  whose names contain `version` as a substring.
#' This is mostly so that e.g. "1.4.2" finds the release tag "v1.4.2",
#'  but since all matches are returned it can also be handy for seeing all
#'  patches sharing a minor version number (see examples).
#'
#' @param version PEcAn release number to use for expected package versions
#' @param exact Show only tags that exactly match `version`,
#'  or all tags that have it as a substring?
#' @return data frame with columns for package name, expected version(s),
#'  and installed version
#'
#' @examples
#' pecan_version()
#' pecan_version(c("1.4.0", "1.5.0", "1.6.0"))
#'
#' # multiple matches are possible when exact = FALSE
#' pecan_version("v1.3", exact = TRUE)
#' pecan_version("v1.3", exact = FALSE)
#'
#' @export
pecan_version <- function(version = max(PEcAn.all::pecan_releases$version),
                          exact = FALSE) {
  all_pkgs <- as.data.frame(utils::installed.packages())
  our_pkgs <- all_pkgs[
    grepl("PEcAn", all_pkgs$Package),
    c("Package", "Version")
  ]
  colnames(our_pkgs) <- c("package", "installed")
  our_pkgs$installed <- package_version(our_pkgs$installed)

  if (!exact) {
    version <- sapply(
      X = version,
      FUN = function(x, ref) ref[grepl(x, ref)],
      ref = colnames(PEcAn.all::pecan_version_history)
    )
    version <- unique(unlist(version))
  }

  res <- merge(our_pkgs, PEcAn.all::pecan_version_history, all = TRUE)
  res <- res[, c("package", version, "installed")]

  drop_na_version_rows(res)
}

# Remove rows where all versions are missing
# i.e. packages neither installed now nor present in any `version`
drop_na_version_rows <- function(df) {
  stopifnot(colnames(df)[[1]] == "package")
  df[rowSums(is.na(df[, -1])) < ncol(df[, -1]), ]
}
