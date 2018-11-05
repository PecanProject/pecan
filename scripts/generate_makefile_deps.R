#!/usr/bin/env RScript

# generate the dependencies of the PEcAn packages on each other.

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
pecan <- c()
depends <- c()
d <- purrr::walk(files,
                 function(x) {
                   y <- desc::desc_get_deps(x)
                   y <- y[y$type %in% c('Depends', 'Imports', 'Remotes'), 'package']
                   y <- y[grepl('^PEcAn.', y)]
                   p <- desc::desc_get_field('Package', file=x)
                   f <- gsub('/DESCRIPTION$', '', x)
                   pecan[[p]] <<- f
                   depends[[f]] <<- y
                 } )

for (name in names(depends)) {
  x <- paste0('$(call depends,', name, '): |')
  for (p in depends[[name]]) {
    x <- paste0(x, ' .install/', pecan[[p]])
  }
  print(x)
}
