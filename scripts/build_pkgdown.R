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

logger("Building pkgdown docs for:", paste(packages, collapse = ", "))

for (pkg in packages) {
  logger("Building pkgdown site for:", pkg)
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
    dest <- file.path("package_documentation/pkgdocs", pkg)
    if (!dir.exists(dest)) {  
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    file.copy(from = source_docs, to = dest, recursive = TRUE, overwrite = TRUE)
    logger("✅ Successfully copied docs from", pkg, "to", dest)
  }, error = function(e) {
    warning(paste("❌ Error building pkgdown site for", pkg, ":", e$message))
  },warning = function(w) {
    warning(paste("⚠️ Warning building pkgdown site for", pkg, ":", w$message))
  }, finally = {
    setwd(current_wd) 
  })
}

logger("✅ All packages processed.")
