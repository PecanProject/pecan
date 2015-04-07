#!/usr/bin/env Rscript

library(devtools)
library(staticdocs)

pkgs <- list("utils", "db", "settings", "visualization",
    "modules/priors",
    "modules/meta.analysis",
            "modules/uncertainty", "modules/data.land", "modules/data.atmosphere", "modules/assim.batch",
            "modules/assim.sequential",
             "models/ed", "models/sipnet", "models/biocro", "all")
 
lapply(pkgs, function(x) dir.create(file.path(x, "inst/staticdocs"), recursive = TRUE))
lapply(pkgs, function(x){
    setwd(file.path("~/tmp/pecan/", x))
    y <- basename(x)
    build_site(file.path("../", y))
})

links <- unlist(lapply(pkgs, function(x) paste0("* [", x, "](", file.path("https://pecanproject.github.io/pecan/", x, "inst/web/index.html"))))
writeLines("## Documentation:\n \n", paste(links, collapse = "\n"), con = "index.md")
system2("pandoc index.md -o index.html")
