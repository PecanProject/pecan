#!/usr/bin/env Rscript

# Parses existing Git tags to create 2 tables:
# `pecan_releases` lists versions and dates of tagged PEcAn releases,
# `pecan_version_history` lists package versions at each release.
#
# This script is provided for historical interest.
# It was used to generate the initial versions of these tables,
# but is not useful for adding new entries to them at release time,
# because those updates will happen before the new tag is created.
# See `record_versions.R` for the release-friendly version.
#
# This script writes full tag dates including timestamps,
# but `record_versions.R` only uses dates.
# Rather than rerun to fix this for all previous releases,
# I resaved the existing file:
# ```
# release_file <- "base/all/data/pecan_releases.rda"
# load(release_file)
# pecan_releases$date <- as.Date(pecan_releases$date)
# save(pecan_releases, file = release_file, version = 2)
# ```

# Usage:
# ./base/all/data-raw/record_previous_releases.R [tag ...] [save]
#
# START WITH A CLEAN GIT TREE. This script checks out tags to read their
# versions. It complains before clobbering anything and restores state
# after run, but no warranties etc
#
# No "save" => just write to console
# ./base/all/data-raw/record_previous_releases.R v1.2.3
#
# With "save" => add results to package data files
# base/all/data/pecan_version_history.rda & base/all/data/pecan_releases.rda
# ./base/all/data-raw/record_previous_releases.R v4.5.6 v4.5.7 save
#
# No tags specified => read all (slow!)
# ./base/all/data-raw/record_previous_releases.R

# Needs R >= 4.1 for `|>` and `\()`,
#  and `git2r` package for repository operations


# ------------ Function definitions -----------

#' Get version numbers of all packages in a (set of) PEcAn release(s)
#'
#' Used internally at release time to construct the lookup table used by
#'   `pecan_version()`.
#'
#' If `tags` is not specified, will report everything that "looks like a
#'   release": any tag that starts with a digit or a "v".
#'   Yes, this is a very crude test, but it Works In This Codebase(TM).
#'
#' @note This function checks out each tag into `repo_path`'s working tree
#'   for processing, so I strongly recommend starting from a clean repository
#'   state. The underlying `git2r::checkout()` function will *probably*
#'   complain before checking out a state that would conflict with any
#'   uncommitted changes, but please do not count on this.
#'
#' @param tags Character vector of Git tags for which to look up package
#'   versions, or NULL for all release tags in repo
#' @param repo_path Path to directory containing PEcAn
#' @return Data frame with one row per package,
#'   with columns `Package` (Package name as character)
#'   plus one column of package versions (as character) per tag
#' @noRd
get_tagged_package_versions <- function(tags = NULL, repo_path = ".") {
  oldwd <- setwd(repo_path)
  on.exit(setwd(oldwd), add = TRUE)

  current_head <- git2r::repository_head()
  on.exit(git2r::checkout(current_head), add = TRUE)

  version_list <- read_tags(tags) |>
    sapply(scan_at_tag, simplify = FALSE) |>
    tag_to_colname()

  versions <- Reduce(
    \(x, y) merge(x, y, by = "package", all = TRUE),
    version_list
  )

  semantic_sort_columns(versions)
}

#' Report tag names, numeric versions, and release dates of PEcAn versions
#' @inheritParams get_tagged_package_versions
#' @return Data frame with one row per release
#'  and columns `tag`, `date`, `version`
#' @noRd
get_releases <- function(tags = NULL, repo_path = ".") {
  oldwd <- setwd(repo_path)
  on.exit(setwd(oldwd), add = TRUE)

  tag_list <- read_tags(tags)
  versions <- parse_tag_versions(names(tag_list))
  dates <- sapply(
    X = tag_list,
    FUN = \(.) {
      if (!is.null(.[["author"]])) {
        as.character(.[[c("author", "when")]])
      } else {
        as.character(.[[c("tagger", "when")]])
      }
    }
  )

  res <- data.frame(
    tag = names(tag_list),
    date = dates,
    version = versions
  )

  res[order(res$date), ]
}

#' Read Git tags from repo in current working directory
#' @param @param tags Character vector of Git tags for which to look up package
#'   versions, or NULL for all release tags in repo
#' @return Named list of `git_commit` objects
#' @noRd
read_tags <- function(tags = NULL) {
  tag_list <- git2r::tags()
  if (is.null(tags) || length(tags) == 0) {
    releases <- grepl("^([[:digit:]]|v)", names(tag_list))
    tag_list <- tag_list[releases]
  } else {
    found_tags <- tags[tags %in% names(tag_list)]
    if (length(found_tags) == 0) {
      PEcAn.logger::logger.severe("Couldn't find any requested tags in repo")
    }
    if (length(found_tags) < length(tags)) {
      PEcAn.logger::logger.warn(
        "Tags not found in repo:",
        tags[!(tags %in% found_tags)],
        "Tags matched:",
        found_tags,
        "Known tags:",
        names(tag_list)
      )
    }
    tag_list <- tag_list[found_tags]
  }

  tag_list
}

#' Check out a Git tag and report the versions of all its packages
#' @param tag a `git_tag` object from `git2r` (not just a string)
#' @noRd
scan_at_tag <- function(tag) {
  git2r::checkout(tag)
  pkgs <- list.files(
    path = ".",
    pattern = "DESCRIPTION",
    recursive = TRUE,
    full.names = TRUE
  )

  pkg_list <- lapply(
    pkgs[!grepl("shiny", pkgs)],
    read.dcf,
    fields = c("Package", "Version")
  )

  do.call("rbind", pkg_list) |>
    as.data.frame() |>
    unique() |> # Yes, some early tags have duplicate package names!
    (\(.) setNames(., tolower(colnames(.))))() |>
    transform(version = package_version(version))
}

#' Move tag from list names to column names inside list entries
#' @param list_of_dfs Named list of dataframes,
#'  each containing a `version` column to be renamed to the list names
#' @noRd
tag_to_colname <- function(list_of_dfs) {
  mapply(
    FUN = \(x, name) {
      colnames(x) <- gsub("version", name, colnames(x))
      x
    },
    list_of_dfs,
    names(list_of_dfs),
    SIMPLIFY = FALSE
  )
}

#' Sort columns by version number
#'
#' Puts "v1.9" left of "v1.10" and "v1.3" left of "1.4".
#'
#' Column "Package" is mandatory and is sorted leftmost.
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
#' "v1.4.3" -> 1.4.3, "1.8foo2.9" -> 1.8, etc
#'
#' @param x character vector
#' @noRd
parse_tag_versions <- function(x) {
  package_version(
    gsub(".*?(([[:digit:]]+\\.?)+).*", "\\1", x),
    strict = FALSE
  )
}



# ---------- End function defs, now for the script! ------------

tags <- commandArgs(trailingOnly = TRUE)
save_result <- FALSE
if (length(tags) > 0 && tags[length(tags)] == "save") {
  save_result <- TRUE
  tags <- tags[-length(tags)]
}

pecan_version_history <- get_tagged_package_versions(tags)
pecan_releases <- get_releases(tags)

if (save_result) {
  version_file <- "base/all/data/pecan_version_history.rda"
  if (file.exists(version_file)) {
    old_ver <- new.env()
    load(version_file, envir = old_ver)
    pecan_version_history <- pecan_version_history |>
      merge(old_ver$pecan_version_history, all = TRUE) |>
      semantic_sort_columns()
  }
  save(pecan_version_history, file = version_file, version = 2)

  release_file <- "base/all/data/pecan_releases.rda"
  if (file.exists(release_file)) {
    old_rel <- new.env()
    load(release_file, envir = old_rel)
    pecan_releases <- pecan_releases |>
      merge(old_rel$pecan_releases, all = TRUE) |>
      (\(.) .[order(.$date), ])()
  }
  save(pecan_releases, file = release_file, version = 2)
} else {
  print("PEcAn release dates:")
  print(pecan_releases)
  print("PEcAn package versions:")
  print(pecan_version_history)
}
