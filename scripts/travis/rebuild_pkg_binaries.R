#!/usr/bin/env Rscript

# ¡ugly hack!
#
# Travis setup uses many prebuilt R packages from c2d4u3.5, which despite its
# name now contains a mix of packages built for R 3.5 and R 3.6. When loaded in
# R 3.5.x, 3.6-built packages throw error
# `rbind(info, getNamespaceInfo(env, "S3methods")):
# number of columns of matrices must match`,
# and as of 2019-10-30 at least one package (data.table) refuses to load if its
# build version does not match the R version it is running on.
#
# We resolve this the slow brute-force way: By running this script before
# attempting to load any R packages, thereby reinstalling from source any
# package whose binary was built with a different R version.
#
# TODO: Remove this when c2d4u situation improves.

is_wrong_build <- function(pkgname) {

  # lockfile implies incomplete previous installation => delete and rebuild
  lock_path <- file.path(.libPaths(), paste0("00LOCK-", pkgname))
  if (any(file.exists(lock_path))) {
    unlink(lock_path, recursive = TRUE)
    return(TRUE)
  }

  built_str <- tryCatch(
    packageDescription(pkgname)$Built,
    error = function(e)e)
  if (inherits(built_str, "error")) {
    # In the rare case we can't read the description,
    # assume package is broken and needs rebuilding
    return(TRUE)
  }

  # Typical packageDescription(pkgname)$Built result: we only need chars 3-7
  # "R 3.4.4; x86_64-apple-darwin15.6.0; 2019-03-18 04:41:51 UTC; unix"
  built_ver <- R_system_version(substr(built_str, start = 3, stop = 7))

  # NB strict comparison: even patch level must agree
  built_ver != getRversion()
}

all_pkgs <- installed.packages()[, 1]
needs_rebuild <- vapply(all_pkgs, is_wrong_build, logical(1))

if (any(needs_rebuild)) {
  print(paste(
    "Found R packages that were built for a different R version.",
    "Reinstalling these from source."))
  install.packages(
    all_pkgs[needs_rebuild],
    repos = "cloud.r-project.org",
    dependencies = FALSE,
    Ncpus = 2)
}
