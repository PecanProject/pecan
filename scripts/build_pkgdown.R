#!/usr/bin/env Rscript

# Build pkgdown documentation for PEcAn packages
library(pkgdown)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No package names provided. Please pass package names as arguments.")
}

packages <- args

if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
  logger <- PEcAn.logger::logger.info
} else {
  logger <- function(...) {
    message(paste(...)) 
  }
}

logger(paste("Building pkgdown docs for:", paste(packages, collapse = ", ")))

for (pkg in packages) {
  message(paste("Building pkgdown site for:", pkg)) 
  current_wd <- getwd()  
  tryCatch({
    if (!dir.exists(pkg)) {
      stop(paste("Package directory does not exist:", pkg))
    }
    setwd(pkg) 
    pkgdown::build_site() 
    setwd(current_wd) 
    source_docs <- file.path(pkg, "docs")
    if (!dir.exists(source_docs)) {
      warning(paste("No docs folder created for:", pkg))
      next 
    }
    dest <- file.path("book_source/pkgdocs", pkg)
    if (!dir.exists(dest)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    if (getRversion() >= "4.2.0") {
      dir.copy(from = source_docs, to = dest, recursive = TRUE, overwrite = TRUE)
    } else { 
      temp_dest <- paste0(dest, "_temp")
      dir.create(temp_dest, recursive = TRUE, showWarnings = FALSE)
      file.copy(from = source_docs, to = temp_dest, recursive = TRUE, overwrite = TRUE)
      file.rename(temp_dest, dest)
    }
    message(paste("✅ Successfully copied docs from", pkg, "to", dest))
  }, error = function(e) {
    warning(paste("❌ Error building pkgdown site for", pkg, ":", e$message))
  }, finally = {
    setwd(current_wd) 
  })
}

logger("✅ All packages processed.")
