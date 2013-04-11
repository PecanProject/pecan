#!/usr/bin/Rscript

require(roxygen2)
require(PEcAn.all)
packages <- c("utils", "db", "visualization",
              "modules/meta.analysis", "modules/uncertainty",
              "modules/data.land", "modules/data.atmosphere",
              "modules/assim.batch", "modules/assim.sequential", "modules/priors",
              "models/ed", "models/sipnet", "models/biocro",
              "all")

sapply(packages, roxygenize)
