#!/usr/bin/env Rscript

# SUPERSEDED, DO NOT USE -- instead just edit the CSVs in `data/`.

# Adds the current versions of all PEcAn packages to `pecan_version_history`,
# and adds the specified tag and version number to `pecan_releases`.

# Usage for testing (prints to console, does not save to package files):
#   ./base/all/data-raw/record_versions.R tag date version
# Usage for release (appends result to package data files):
#   ./base/all/data-raw/record_versions.R tag date version save

# Expected typical example: Prepare to release PEcAn 10.8.3 on Dec 1, 2045
# (Please use same tag and date that will be applied to the actual release!)
#   ./base/all/data-raw/record_versions.R v10.8.3 2045-12-01 10.8.3 save

# Needs R >= 4.1 for `|>` and `\()`,


######## Function defs

#' Sort columns by version number
#'
#' Puts "v1.9" left of "v1.10" and "v1.3" left of "1.4".
#'
#' Column "package" is mandatory and is sorted leftmost.
#' Other columns that don't contain recognizable numeric versions are sorted
#'   rightmost
#'
#' @param df dataframe
#' @return `df` with columns sorted such that colnames containing version
#'   strings are sorted in increasing semantic order from left to right
#' @noRd
semantic_sort_columns <- function(df) {
  sorted_tags <- colnames(df)[order(parse_tag_versions(colnames(df)))]
  sorted_tags <- c("package", sorted_tags[sorted_tags != "package"])

  df[, sorted_tags]
}

#' Convert string containing version number into a numeric version
#'
#' R's numeric versions are integers separated by single dots or dashes
#' We "convert" by taking the first substring that contains only these
#' "v1.4.3" -> 1.4.3, "1.8foo2.9" -> 1.8, etc
#' But beware: "foo2bar_1.3.5" -> 2
#'
#' @param x character vector
#' @noRd
parse_tag_versions <- function(x) {
  package_version(
    gsub(".*?(([[:digit:]]+[.-]?)+).*", "\\1", x),
    strict = FALSE
  )
}

######## End function defs


args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3 || length(args) > 4) {
  stop("Usage: record_versions.R tag version date [save]")
}

if (!all(c("./base", "./models", "./modules") %in% list.dirs())) {
  stop("Run this script in the PEcAn root directory")
}

tag <- args[[1]]
date = as.Date(args[[2]])
version = package_version(args[[3]])
 
save_result <- FALSE
if (length(args) == 4 && args[[4]] == "save") {
  save_result <- TRUE
}


pkgs <- list.files(
  path = ".",
  pattern = "DESCRIPTION",
  recursive = TRUE,
  full.names = TRUE
)
pkg_list <- lapply(
  pkgs[!grepl("shiny|\\.Rcheck", pkgs)],
  read.dcf,
  fields = c("Package", "Version")
)
pkg_versions <- do.call("rbind", pkg_list) |>
  as.data.frame() |>
  transform(Version = package_version(Version)) |>
  setNames(c("package", tag))
stopifnot(!anyDuplicated(pkg_versions$package))

version_file <- "base/all/data/pecan_version_history.rda"
stopifnot(file.exists(version_file))
old_ver <- new.env()
load(version_file, envir = old_ver)
pecan_version_history <- pkg_versions |>
  merge(old_ver$pecan_version_history, all = TRUE) |>
  semantic_sort_columns()
if (save_result) {
  save(pecan_version_history, file = version_file, version = 2)
}

release_file <- "base/all/data/pecan_releases.rda"
stopifnot(file.exists(release_file))
old_rel <- new.env()
load(release_file, envir = old_rel)
pecan_releases <- data.frame(tag = tag, date = date, version = version) |>
  merge(old_rel$pecan_releases, all = TRUE) |>
  (\(.) .[order(.$date), ])()
if(save_result) {
  save(pecan_releases, file = release_file, version = 2)
}

if(!save_result) {
  print("PEcAn release info:")
  print(pecan_releases)
  print("PEcAn package versions:")
  print(pecan_version_history)
  print("If this looks good and you're ready to overwrite data files,")
  print("rerun adding `save` as the last argument")
}
