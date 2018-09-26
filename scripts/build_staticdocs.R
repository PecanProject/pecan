#!/usr/bin/env Rscript

library(devtools)
library(pkgdown)
library(magrittr)

pkg_list<- list.dirs(path = ".", full.names = FALSE, recursive = TRUE) %>%
           grep("man$",., value = TRUE) %>%
           dirname(.)
pkg_list <- pkg_list[-1]

lapply(pkg_list, function(x){
  print(x)
  tryCatch(
    {
      pkgdown::build_site(x,examples = FALSE)
    },
    error = function(e){print(conditionMessage(e))
    }
  )
}
)

links <- unlist(lapply(pkg_list, function(x) paste0("* [", x, "](", file.path("https://pecanproject.github.io/pecan/", x, "docs/index.html)"))))
writeLines("## Documentation:\n \n", paste(links, collapse = "\n"), con = "index.md")
system("pandoc index.md -o index.html")
