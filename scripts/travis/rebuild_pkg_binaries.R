#!/usr/bin/env Rscript

# ¡ugly hack!
#
# Travis setup uses many prebuilt R packages from c2d4u3.5, which despite its
# name now contains some packages that were built for R 3.6. When loaded in
# R 3.5.x, these throw error `rbind(info, getNamespaceInfo(env, "S3methods")):
# number of columns of matrices must match`.
#
# We resolve this the slow brute-force way: By running this script before
# attempting to load any R packages, thereby reinstalling from source any
# package whose binary was built with R >= 3.6.
#
# In principle it's *always* unsafe to use a package built for a different R
# version, so it might be safest to rebuild any binary that fails the
# wrong_build test. But rebuilding is slooow and *probably* only needed for
# this special case, so we only do this check if run with R < 3.6.
#
# TODO: Remove this when c2d4u situation improves or we drop support for R 3.5.

if (getRversion() < "3.6") {
  wrong_build <- function(pkgname) {
    # Typical packageDescription(pkgname)$Built result:
    # "R 3.4.4; x86_64-apple-darwin15.6.0; 2019-03-18 04:41:51 UTC; unix"
    build_ver <- substr(packageDescription(pkgname)$Built, start = 3, stop = 7)
    build_ver > getRversion()
  }
  all_pkgs <- installed.packages()[,1]
  needs_rebuild <- vapply(all_pkgs, wrong_build, logical(1))

  if (any(needs_rebuild)) {
    print("Found R packages that were built for a later R version. Reinstalling these from source.")
    install.packages(all_pkgs[needs_rebuild], repos = "cloud.r-project.org", dependencies = FALSE)
  }
}
