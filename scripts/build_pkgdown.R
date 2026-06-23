#!/usr/bin/env Rscript
# Build pkgdown documentation for PEcAn packages
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("No package names provided. Please pass package names as arguments.")
}
packages <- args
output_dir <- "_pkgdown_docs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
  logger <- PEcAn.logger::logger.info
} else {
  logger <- function(...) {
    message(paste(...))
  }
}

build_and_copy <- function(pkg, branch, outdir) {
  logger("Building pkgdown site for:", pkg)
  if (!dir.exists(pkg)) {
    stop(paste("Package directory does not exist:", pkg))
  }
  outdir <- normalizePath(outdir)
  oldwd <- setwd(pkg)
  on.exit(setwd(oldwd), add = TRUE)

  url_str <- paste0("https://github.com/PecanProject/pecan/blob/",
                    branch, "/", pkg, "/")
  up_nav_str <- r"(<a href="../index.html" style="padding: 0em 1em">← Up</a>)"

  pkgdown::build_site(
    pkg = ".",
    override = list(
      "llm-docs" = FALSE,
      repo = list(url = list(source = url_str)),
      template = list(
        bootstrap = 5,
        includes = list(before_navbar = up_nav_str)
      )
    )
  )

  if (!dir.exists("docs")) {
    warning(paste("No docs folder created for:", pkg))
    return()
  }
  pkgname <- desc::desc_get("Package", ".")
  dest <- file.path(outdir, pkgname)
  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(
    from = list.files("docs", full.names = TRUE),
    to = dest,
    recursive = TRUE,
    overwrite = TRUE
  )
  logger("✅ Successfully copied docs from", pkg, "to", dest)
}

# wrapper that catches errors and warnings as part of the returned list
build_quietly <- function(pkg, ...) {
  err <- NULL
  warns <- list()

  tryCatch(
    build_and_copy(pkg = pkg, ...),
    error = function(e) {
      e$message <- paste(
        "❌ Error building pkgdown site for", pkg, ":", e$message
      )
      err <<- e
    },
    # TODO this branch doesn't ever seem to run.
    # Seems like pkgdown reports warnings as they happen, before returning
    warning = function(w) {
      w$message <- paste(
        "⚠️ Warning building pkgdown site for", pkg, ":", w$message
      )
      warns <<- append(warns, w)
      invokeRestart("muffleWarning")
    }
  )

  list(error = err, warnings = warns)
}




# Define branch variable once for all packages
branch <- Sys.getenv("PECAN_GIT_BRANCH", unset = "develop")

logger("Building pkgdown docs for:", paste(packages, collapse = ", "))
build_results <- packages |>
  purrr::map(build_quietly, branch = branch, outdir = output_dir)

logger("Creating index page")
built_pkg_dirs <- list.dirs(output_dir, recursive = FALSE, full.names = FALSE)
before_text <- c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '  <title>Package-specific documentation for the PEcAn R packages</title>',
  '</head>',
  '<body>',
  '<h1>PEcAn package documentation</h1>',
  '<p>Function documentation and articles for each PEcAn package,',
  '   generated from the package source using <a href="https://pkgdown.r-lib.org/">pkgdown</a>.</p>',
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

build_warns <- purrr::map_lgl(build_results, \(x) length(x$warnings) > 0)
if (any(build_warns)) {
  logger("⚠️ Warnings found in package(s)", packages[build_warns], ":")
  purrr::map(build_results[build_warns], "warnings") |>
    purrr::walk(rlang::warn)
}

build_err <- purrr::map_lgl(build_results, \(x) !is.null(x$error))
if (any(build_err)) {
  logger("❌ Error building package(s)", packages[build_err], ":")
  purrr::map(build_results[build_err], "error") |>
    purrr::walk(\(x) rlang::inform(conditionMessage(x)))
  stop("Please fix these and rerun.")
} else {
  logger("✅ All packages processed.")
}
