#!/usr/bin/env Rscript

library(devtools)
library(pkgdown)

pkg_list<- list.dirs(path = ".", full.names = FALSE, recursive = TRUE) %>%
           grep("man$",., value = TRUE) %>%
           dirname(.)
pkg_list <- pkg_list[-1]
#as.data.frame(.) 
#%>%
#  t <- filter(pkg_list,grepl(paste(pkg_patterns, collapse="$"),.))
#%>%
#dirname(as.vector(t$.))
#pat <- "PEcAn"
#packages <- paste0(installed.packages()[,2],"/",installed.packages()[,1])
#pecan_pkg <- grep(pat,packages,value = TRUE)


lapply(pkg_list, function(x){
  print(x)
  tryCatch(
    {
      pkgdown::build_site(x)
    },
    error = function(e){print(conditionMessage(e))
    }
  )
}
)


links <- unlist(lapply(pkgs, function(x) paste0("* [", x, "](", file.path("https://pecanproject.github.io/pecan/", x, "inst/web/index.htmlg)"))))
writeLines("## Documentation:\n \n", paste(links, collapse = "\n"), con = "index.md")
system2("pandoc index.md -o index.html")
