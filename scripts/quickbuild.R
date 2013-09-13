#!/usr/bin/env Rscript

library(devtools)
dev_mode(on = TRUE)

lapply(list("utils", "db", "settings", "visualization", "modules/priors", "modules/meta.analysis",
            "modules/uncertainty", "modules/data.land", "modules/data.atmosphere", "modules/assim.batch",
            "modules/assim.sequential", "models/ed", "models/sipnet", "models/biocro", "all"),
       function(x) install(x, quick = TRUE, local = TRUE, quiet = TRUE))

