#!/usr/bin/env Rscript

# must be run from home directory

require(roxygen2)
require(methods)
packages <- c("utils", "db", "visualization",
              "modules/meta.analysis", "modules/uncertainty",
              "modules/data.land", "modules/data.atmosphere","modules/allometry",
              "modules/assim.batch", "modules/assim.sequential", "modules/priors",
              "modules/benchmark","models/ed", "models/sipnet", "models/biocro")

pkgdocfiles <- function(x) {
  docfiles <- dir(path = file.path(x, "man"), pattern = "*.Rd", full.names = TRUE)
}
all.docfiles <- unlist(sapply(packages, pkgdocfiles, simplify = TRUE))
sapply(all.docfiles, file.remove)
null <- sapply(packages, function(x) roxygenize(x, roclets = 'rd')
