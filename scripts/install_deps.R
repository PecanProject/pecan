#!/usr/bin/env RScript

SUGGESTS <- TRUE
INSTALL <- FALSE

files <-
  c(
    list.files(
      path = "base",
      full.names = TRUE,
      pattern = "^DESCRIPTION$",
      recursive = TRUE
    ),
    list.files(
      path = "modules",
      full.names = TRUE,
      pattern = "^DESCRIPTION$",
      recursive = TRUE
    ),
    list.files(
      path = "models",
      full.names = TRUE,
      pattern = "^DESCRIPTION$",
      recursive = TRUE
    )
  )

## Required dependencies
d <- purrr::map(files,
                function(x) {
                  y <- desc::desc_get_deps(x)
                  y[y$type %in% c('Depends', 'Imports', 'Remotes'), 'package']
                })
d <- sort(unique(unlist(d)))
d <- d[!grepl('^PEcAn.', d)]

if (INSTALL) {
  purrr::walk(d,
              function(p) {
                print("# ----------------------------------------------------------------------")
                print(paste0("# INSTALLING ", p))
                print("# ----------------------------------------------------------------------")
                install.packages(p, repos = 'http://cran.rstudio.com/')
                if (system.file(package = p) == "") {
                  stop("Don't know how to install dependency ", p)
                }
              })
} else {
  print(paste(d, collapse = ' '))
}

## Suggested dependencies
if (SUGGESTS) {
  s <- purrr::map(files,
                  function(x) {
                    y <- desc::desc_get_deps(x)
                    y[y$type %in% c('Suggests'), 'package']
                  })
  s <- sort(unique(unlist(s)))
  s <- s[!grepl('^PEcAn.', s)]
  s <- s[!s %in% c('BioCro', 'linkages', 'Maeswrap', 'Rpreles', 'RMySQL')]
  s <- s[!s %in% d]

  if (INSTALL) {
    purrr::walk(s,
                function(p) {
                  print("# ----------------------------------------------------------------------")
                  print(paste0("# INSTALLING ", p))
                  print("# ----------------------------------------------------------------------")
                  install.packages(p, repos = 'http://cran.rstudio.com/')
                  if (system.file(package = p) == "") {
                    stop("Don't know how to install dependency ", p)
                  }
                })
  } else {
    print(paste(d, collapse = ' '))
  }
}

