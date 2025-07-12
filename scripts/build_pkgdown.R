#!/usr/bin/env Rscript

# Build pkgdown documentation for PEcAn packages
library(pkgdown)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No package names provided. Please pass package names as arguments.")
}

packages <- args
output_dir <- "_pkgdown_docs"

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
    pkgname <- desc::desc_get("Package", pkg)
    dest <- file.path(output_dir, pkgname)
    if (!dir.exists(dest)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    }
    file.copy(
      from = list.files(source_docs, full.names = TRUE),
      to = dest,
      recursive = TRUE,
      overwrite = TRUE
    )
    logger("✅ Successfully copied docs from", pkg, "to", dest)
  }, error = function(e) {
    warning(paste("❌ Error building pkgdown site for", pkg, ":", e$message))
  },warning = function(w) {
    warning(paste("⚠️ Warning building pkgdown site for", pkg, ":", w$message))
  }, finally = {
    setwd(current_wd) 
  })
}

logger("Creating index page")
built_pkg_dirs <- list.dirs(output_dir, recursive=FALSE, full.names = FALSE)
before_text <- c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <title>Package-specific documentation for the PEcAn R packages</title>',
  '</head>',
  '<body>',
  '<h1>PEcAn package documentation</h1>',
  '<p>Function documentation and articles for each PEcAn package,',
  '   generated from the package source using <code>{pkgdown}</code>.</p>',
  '',
  '<ul>'
)
listing_text <- paste0(
  '  <li><a href="', built_pkg_dirs, '/index.html">',
  built_pkg_dirs,
  '</a></li>'
)
after_text <- c(
  '  </ul>',
  '',
  '</body>',
  '</html>'
)
writeLines(
  text = c(before_text, listing_text, after_text),
  con = file.path(output_dir, "index.html")
)

logger("✅ All packages processed.")
