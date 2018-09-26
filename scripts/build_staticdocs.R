#!/usr/bin/env Rscript

library(devtools)
library(pkgdown)

pkg_list<- list.dirs(path = ".", full.names = FALSE, recursive = TRUE) %>%
           grep("R$",., value = TRUE) %>%
           dirname(.)
        
        

pkgs <- list( "db", "settings", "utils","visualization",
              "modules/allometry","modules/assim.batch",
              "modules/assim.sequential",
              "modules/data.atmosphere","modules/data.land",
              "modules/data.remote","modules/emulator",
              "modules/meta.analysis","modules/photosynthesis",
              "modules/priors","modules/rtm",
              "modules/uncertainty", 
              "models/biocro", "models/clm45",
              "models/dalec",
              "models/ed", "models/gday",
              "models/linkages","models/lpjguess",
              "models/maat","models/maespa",
              "models/preles",
              "models/sipnet",
              "all")
 
lapply(pkgs, function(x) dir.create(file.path(x, "inst/staticdocs"), recursive = TRUE))

build.doc <- function(x){
  y <- basename(x)
  pkgdown::build_site(file.path("../", y))
}
start = 1

for(i in seq_along(pkgs)){
  print(i)
  build.doc(pkgs[[i]])
}

links <- unlist(lapply(pkgs, function(x) paste0("* [", x, "](", file.path("https://pecanproject.github.io/pecan/", x, "inst/web/index.htmlg)"))))
writeLines("## Documentation:\n \n", paste(links, collapse = "\n"), con = "index.md")
system2("pandoc index.md -o index.html")
