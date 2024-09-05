#!/usr/bin/env Rscript

# Usage as a script:
# ./confirm_deps path/to/package [dependencies]
# Usage as a function: source in and use as documented below



#' Check whether a local package's dependencies are already satisfied
#'
#' This is a wrapper around `remotes::install_deps` that checks first, before
#' consulting any remote repositories,  whether the package versions already
#' installed are sufficient to meet all declared dependencies.
#' If they are, it returns nothing. If any dependencies are missing or need
#' upgrading, it either installs them by calling `remotes::install_deps`
#' (if `install = TRUE`) or returns a dataframe of dependency versions
#' (if `install = FALSE`).
#'
#' Motivation: Because `install_deps` always eagerly checks for new versions of
#' all dependencies, including those in non-CRAN remotes, using it to check
#' dependencies for a large number of packages can exceed GitHub's limit on
#' unauthenticated API queries.
#' Setting a personal access token increases the rate allowance, but it still seemed silly to check
#' every time for new versions when the real question was "Do I already have the
#' dependencies in place to install this package right now?"
#'
#' Note that if any packages do need installation, the call to
#' `install_deps` will as usual check and update all dependencies, not just the
#' ones that were unsatisfied. Control this behavior by passing an `upgrade`
#' argument in `...`.
#'
#' @inheritParams remotes::install_deps
#' @param install Install dependencies if they are missing, or just report them?
#' @return If `install = FALSE`, a data frame of available and needed package
#'   versions. If `install = TRUE`, the output from `install_deps`.
confirm_deps <- function(pkg,
                         install = TRUE,
                         dependencies = NA,
                         ...) {

  # Q: "Why a separate variable instead of overwriting `dependencies`?"
  # A: As a quick workaround for https://github.com/r-lib/remotes/issues/809:
  # remotes::install_deps(pkgdir, `dependencies = TRUE`) correctly installs
  # optional deps of `pkgdir` but only hard deps of its dependencies,
  # whereas `dependencies = c(..., "Suggests")` installs the whole
  # recursive chain of Suggests of Suggests of Suggests.
  if (all(is.na(dependencies)) || all(dependencies == "hard")) {
    dependency_types <- c("Depends", "Imports", "LinkingTo")
  } else if (isTRUE(dependencies) || all(dependencies == "soft")) {
    dependency_types <- c("Depends", "Imports", "LinkingTo", "Suggests")
  } else if (isFALSE(dependencies)) {
    return() # for compatibility with remotes::install_deps
  }

  deps <- desc::desc_get_deps(pkg)
  deps <- deps[deps$type %in% dependency_types, ]

  pkgs <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)[, c("Package", "Version")]
  colnames(pkgs) <- c("package", "installed_version")
  colnames(deps)[colnames(deps) == "version"] <- "needed_version"
  deps <- merge(deps, pkgs, all.x = TRUE)

  # Check for incompatible versions
  # Surprised I can't find a base function that does this in one shot...
  # Regex note: `*` in pattern is a literal asterisk,
  # to match the "*" returned by desc_get_deps when no version is specified
  deps$later_ok <- grepl("[>*]", deps$needed_version) #
  deps$earlier_ok <- grepl("[<*]", deps$needed_version)
  deps$equal_ok <- grepl("[=*]", deps$needed_version)
  deps$needed_version <- gsub("[^[:digit:].-]", "", deps$needed_version)
  deps$compare <- mapply(
    FUN = compareVersion,
    deps$installed_version,
    deps$needed_version,
    SIMPLIFY = TRUE
  )
  deps$needs_upgrade <- (
    is.na(deps$installed_version) |
      (!deps$equal_ok) & (deps$compare == 0) |
      (!deps$later_ok) & (deps$compare == 1) |
      (!deps$earlier_ok) & (deps$compare == -1)
  )

  if (any(deps$needs_upgrade) && install) {
    return(
      remotes::install_deps(
        pkg = pkg,
        dependencies = dependencies,
        ...
      )
    )
  }

  if (!install) {
    cols <- c(
      "package",
      "needed_version",
      "installed_version",
      "needs_upgrade"
    )
    return(deps[, cols])
  }
}






args <- commandArgs(trailingOnly = TRUE)
pkg <- args[[1]]
if (length(args) > 1) {
  dep <- eval(parse(text = args[[2]]))
} else {
  dep <- NA
}
confirm_deps(pkg = pkg, dependencies = dep, install = TRUE, upgrade = FALSE)
