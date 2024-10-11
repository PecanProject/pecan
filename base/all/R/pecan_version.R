#' Report installed PEcAn version
#'
#' Reports the currently installed or loaded version(s) of each PEcAn package,
#'  as well as the versions expected by the specified release(s) of the PEcAn
#'  system.
#'
#' When `exact` = FALSE (the default), `pecan_version` will show all releases
#'  whose names contain `version` as a substring.
#' This is mostly so that e.g. "1.4.2" finds the release tag "v1.4.2",
#'  but since all matches are returned it can also be handy for seeing all
#'  patches sharing a minor version number (see examples).
#'
#' If more than one version of a package is installed, the output from
#'  `pecan_version()` will contain multiple rows for that package.
#' This can occur if you have installed different versions to different
#'  locations in `.libPaths()`, or if you've loaded a new version into your
#'  current session by loading it from its source directory without installing
#'  it to the R library.
#' If you see multiple rows unexpectedly, try
#'  `find.package(<pkgname>, verbose = TRUE)` to see where each version was found.
#'
#' @param version PEcAn release number to use for expected package versions
#' @param exact Show only tags that exactly match `version`,
#'  or all tags that have it as a substring?
#' @return data frame with columns for package name, expected version(s),
#'  installed version, and Git hash (if known).
#'  If the `sessioninfo` package is installed, an additional column reports
#'  where each package was installed from: local, github, CRAN, etc.
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
  if (!exact) {
    version <- sapply(
      X = version,
      FUN = function(x, ref) ref[grepl(x, ref)],
      ref = colnames(PEcAn.all::pecan_version_history)
    )
    version <- unique(unlist(version))
  }
  cols_to_return <- c("package", version, "installed", "build_hash")


  if (requireNamespace("sessioninfo", quietly = TRUE)) {
    cols_to_return <- c(cols_to_return, "source")

    all_pkgs <- sessioninfo::package_info(pkgs = "installed", dependencies = FALSE)
    our_pkgs <- all_pkgs[grepl("PEcAn", all_pkgs$package),]

    # Why do we need this when `pkgs = "installed"` usually shows loaded too?
    # Because there are times a package is loaded but not installed
    # (e.g. notably during R CMD check)
    all_loaded <- sessioninfo::package_info(pkgs = "loaded", dependencies = FALSE)
    our_loaded <- all_loaded[grepl("PEcAn", all_loaded$package),]

    # TODO: consider using package_info's callouts of packages where loaded and
    #   installed versions mismatch -- it's a more elegant version of what we
    #   were trying for with the "multiple rows for packages with multiple
    #   versions found" behavior.
    our_pkgs <- merge(
      x = our_pkgs[, c("package", "ondiskversion", "source")],
      y = our_loaded[, c("package", "loadedversion", "source")],
      by.x = c("package", "ondiskversion", "source"),
      by.y = c("package", "loadedversion", "source"),
      all = TRUE,
      sort = TRUE)
    colnames(our_pkgs) <- c("package", "installed", "source")
    our_pkgs$installed <- package_version(our_pkgs$installed)

  } else {
    all_pkgs <- as.data.frame(utils::installed.packages())
    our_pkgs <- all_pkgs[
      grepl("PEcAn", all_pkgs$Package),
      c("Package", "Version")
    ]
    colnames(our_pkgs) <- c("package", "installed")
    our_pkgs$installed <- package_version(our_pkgs$installed)
    sess <- utils::sessionInfo()
    sess <- c(sess$otherPkgs, sess$loadedOnly)
    our_loaded <- sess[grepl("PEcAn", names(sess))]
    our_loaded <- data.frame(
      package = names(our_loaded),
      installed = sapply(our_loaded, `[[`, "Version"))
    our_loaded$installed <- package_version(our_loaded$installed)
    our_pkgs <- merge(our_pkgs, our_loaded, all = TRUE, sort = TRUE)
    our_pkgs <- our_pkgs[!duplicated(our_pkgs),]
  }

  want_hash <- !is.na(our_pkgs$installed)
  our_pkgs$build_hash[want_hash] <- sapply(
    our_pkgs$package[want_hash],
    get_buildhash)

  res <- merge(
    x = our_pkgs,
    y = PEcAn.all::pecan_version_history,
    all = TRUE)
  res <- drop_na_version_rows(res[, cols_to_return])
  rownames(res) <- res$package
  class(res) <- c("pecan_version_report", class(res))

  res
}

# Remove rows where all versions are missing
# i.e. packages neither installed now nor present in any `version`
drop_na_version_rows <- function(df) {
  stopifnot(colnames(df)[[1]] == "package")
  df[rowSums(is.na(df[, -1])) < ncol(df[, -1]), ]
}


# Look up git revision, if recorded, from an installed PEcAn package
get_buildhash <- function(pkg) {
  # Set if pkg was installed from r-universe or via install_github()
  desc_sha <- utils::packageDescription(pkg, fields = "RemoteSha")
  if (!is.na(desc_sha)) {
    return(substr(desc_sha, 1, 10))
  }
  # Set if PECAN_GIT_REV was set during install (includes `make install`)
  get0(".build_hash", envir = asNamespace(pkg), ifnotfound = NA_character_)
}


# print method for version
# (Just to help it display more compactly)
#' @export
print.pecan_version_report <- function(x, ...) {

  dots <- list(...)
  if (is.null(dots$row.names)) { dots$row.names <- FALSE }
  if (is.null(dots$right)) { dots$right <- FALSE }

  xx <- as.data.frame(x)
  # only print hash for dev versions
  # (typically x.y.z.9000, but we'll use anything with a 4th version component)
  skip_hash <- is.na(xx$installed[,4]) | is.na(xx$build_hash)
  xx$build_hash[skip_hash] <- ""
  xx$build_hash <- sub(".{4}\\+mod$", "+mod", xx$build_hash)
  xx$installed <- paste0(
    xx$installed,
    sub("(.+)", " (\\1)", xx$build_hash))
  xx$build_hash <- NULL
  if (!is.null(xx$source)) {
    xx$source <- paste0(
      strtrim(xx$source, 17),
      ifelse(nchar(xx$source, type="width") <= 17, "", "..."))
  }
  dots$x <- xx
  do.call("print", dots)

  invisible(x)
}
