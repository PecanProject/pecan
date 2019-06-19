#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##--------------------------------------------------------------------------------------------------#
##' Check two lists. Identical does not work since one can be loaded
##' from the database and the other from a CSV file.
##'
##' @name check.lists
##' @title Compares two lists
##' @param x first list
##' @param y second list
##' @param filename one of "species.csv" or "cultivars.csv"
##' @return true if two list are the same
##' @author Rob Kooper
##'
check.lists <- function(x, y, filename = "species.csv") {
  if (nrow(x) != nrow(y)) {
    return(FALSE)
  }
  if(filename == "species.csv"){
    cols <- c('id', 'genus', 'species', 'scientificname')
  } else if (filename == "cultivars.csv") {
    cols <- c('id', 'specie_id', 'species_name', 'cultivar_name')
  } else {
    return(FALSE)
  }
  xy_match <- vapply(cols, function(i) identical(as.character(x[[i]]), as.character(y[[i]])), logical(1))
  return(all(unlist(xy_match)))
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database for a single PFT
##'
##' @details `pft` should be a list containing at least `name` and `outdir`, and optionally `posteriorid` and `constants`. BEWARE: All existing files in `outir` will be deleted!
##' @param pft list of settings for the pft whose traits to retrieve. See details
##' @param modeltype type of model that is used, this is used to distinguish between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @param trait.names list of trait names to retrieve
##' @return updated pft with posteriorid
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
get.trait.data.pft <- function(pft, modeltype, dbfiles, dbcon, trait.names,
                               forceupdate = FALSE) {

  # Create directory if necessary
  if (!file.exists(pft$outdir) && !dir.create(pft$outdir, recursive = TRUE)) {
    PEcAn.logger::logger.error(paste0("Couldn't create PFT output directory: ", pft$outdir))
  }

  ## Remove old files.  Clean up.
  old.files <- list.files(path = pft$outdir, full.names = TRUE, include.dirs = FALSE)
  file.remove(old.files)

  # find appropriate pft
  pftres <- query_pfts(dbcon, pft[["name"]], modeltype)
  pfttype <- pftres[["pft_type"]]
  pftid <- pftres[["id"]]

  if (nrow(pftres) > 1) {
    PEcAn.logger::logger.severe(
      "Multiple PFTs named", pft[["name"]], "found,",
      "with ids", PEcAn.utils::vecpaste(pftres[["id"]]), ".",
      "Specify modeltype to fix this.")
  }

  if (nrow(pftres) == 0) {
    PEcAn.logger::logger.severe("Could not find pft", pft[["name"]])
    return(NA)
 }

 # get the member species/cultivars, we need to check if anything changed
  if (pfttype == "plant") {
    pft_member_filename = "species.csv"
    pft_members <- PEcAn.DB::query.pft_species(pft$name, modeltype, dbcon)
  } else if (pfttype == "cultivar") {
    pft_member_filename = "cultivars.csv"
    pft_members <- PEcAn.DB::query.pft_cultivars(pft$name, modeltype, dbcon)
  } else {
    PEcAn.logger::logger.severe("Unknown pft type! Expected 'plant' or 'cultivar', got", pfttype)
  }

  # ANS: Need to do this conversion for the check against existing
  # membership later on. Otherwise, `NA` from the CSV is interpreted
  # as different from `""` returned here, even though they are really
  # the same thing.
  pft_members <- pft_members %>%
    dplyr::mutate_if(is.character, ~dplyr::na_if(., ""))

  # get the priors
  prior.distns <- PEcAn.DB::query.priors(pft = pftid, trstr = PEcAn.utils::vecpaste(trait.names), con = dbcon)
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in% names(pft$constants)),]
  traits <- rownames(prior.distns)

  # get the trait data (don't bother sampling derived traits until after update check)
  trait.data.check <- PEcAn.DB::query.traits(ids = pft_members$id, priors = traits, con = dbcon, update.check.only = TRUE, ids_are_cultivars = (pfttype=="cultivar"))
  traits <- names(trait.data.check)

  # Set forceupdate FALSE if it's a string (backwards compatible with 'AUTO' flag used in the past)
  if (!is.logical(forceupdate)) {
    forceupdate <- FALSE
  }

  # check to see if we need to update
  if (!forceupdate) {
    if (is.null(pft$posteriorid)) {
      pft$posteriorid <- dplyr::tbl(dbcon, "posteriors") %>%
        dplyr::filter(pft_id == !!pftid) %>%
        dplyr::arrange(dplyr::desc(created_at)) %>%
        head(1) %>%
        dplyr::pull(id)
    }
    if (!is.null(pft$posteriorid)) {
      files <- dbfile.check(type = "Posterior", container.id = pft$posteriorid, con = dbcon,
                            return.all = TRUE)
      need_files <- c(
        trait_data = "trait.data.Rdata",
        priors = "prior.distns.Rdata",
        pft_membership = pft_member_filename
      )
      ids <- match(need_files, files$file_name)
      names(ids) <- names(need_files)
      if (any(is.na(ids))) {
        missing_files <- need_files[is.na(ids)]
        PEcAn.logger::logger.info(paste0(
          "Forcing meta-analysis update because ",
          "the following files are missing from the posterior: ",
          paste0(shQuote(missing_files), collapse = ", ")
        ))
        PEcAn.logger::logger.debug(
          "\n `dbfile.check` returned the following output:\n",
          PEcAn.logger::print2string(files),
          wrap = FALSE
        )
      } else {
        PEcAn.logger::logger.debug(
          "All posterior files are present. Performing additional checks ",
          "to determine if meta-analysis needs to be updated."
        )
        # check if all files exist
        need_paths <- file.path(files$file_path[ids], need_files)
        names(need_paths) <- names(need_files)
        files_exist <- file.exists(need_paths)
        foundallfiles <- all(files_exist)
        if (!foundallfiles) {
          PEcAn.logger::logger.warn(
            "The following files are in database but not found on disk: ",
            paste(shQuote(need_files[!files_exist]), collapse = ", "), ". ",
            "Re-running meta-analysis."
          )
        } else {
          # Check if PFT membership has changed
          PEcAn.logger::logger.debug("Checking if PFT membership has changed.")
          existing_membership <- utils::read.csv(
            need_paths[["pft_membership"]],
            # Columns are: id, genus, species, scientificname
            # Need this so NA values are
            colClasses = c("double", "character", "character", "character"),
            stringsAsFactors = FALSE,
            na.strings = ""
          )
          diff_membership <- symmetric_setdiff(
            existing_membership,
            pft_members,
            xname = "existing",
            yname = "current"
          )
          if (nrow(diff_membership) > 0) {
            PEcAn.logger::logger.error(
              "\n PFT membership has changed. \n",
              "Difference is:\n",
              PEcAn.logger::print2string(diff_membership),
              wrap = FALSE
            )
            foundallfiles <- FALSE
          }

          # Check if priors have changed
          PEcAn.logger::logger.debug("Checking if priors have changed")
          existing_prior <- PEcAn.utils::load_local(need_paths[["priors"]])[["prior.distns"]]
          diff_prior <- symmetric_setdiff(
            dplyr::as_tibble(prior.distns, rownames = "trait"),
            dplyr::as_tibble(existing_prior, rownames = "trait")
          )
          if (nrow(diff_prior) > 0) {
            PEcAn.logger::logger.error(
              "\n Prior has changed. \n",
              "Difference is:\n",
              PEcAn.logger::print2string(diff_prior),
              wrap = FALSE
            )
            foundallfiles <- FALSE
          }

          # Check if trait data have changed
          PEcAn.logger::logger.debug("Checking if trait data have changed")
          existing_trait_data <- PEcAn.utils::load_local(
            need_paths[["trait_data"]]
          )[["trait.data"]]
          if (length(trait.data.check) != length(existing_trait_data)) {
            PEcAn.logger::logger.warn(
              "Lengths of new and existing `trait.data` differ. ",
              "Re-running meta-analysis."
            )
            foundallfiles <- FALSE
          } else if (length(trait.data.check) == 0) {
            PEcAn.logger::logger.warn("New and existing trait data are both empty. Skipping this check.")
          } else {
            current_traits <- dplyr::bind_rows(trait.data.check, .id = "trait") %>%
              dplyr::select(-mean, -stat)
            existing_traits <- dplyr::bind_rows(existing_trait_data, .id = "trait") %>%
              dplyr::select(-mean, -stat)
            diff_traits <- symmetric_setdiff(current_traits, existing_traits)
            if (nrow(diff_traits) > 0) {
              diff_summary <- diff_traits %>%
                dplyr::count(source, trait)
              PEcAn.logger::logger.error(
                "\n Prior has changed. \n",
                "Here are the number of differing trait records by trait:\n",
                PEcAn.logger::print2string(diff_summary),
                wrap = FALSE
              )
              foundallfiles <- FALSE
            }
          }
        }
        

        if (foundallfiles) {
          PEcAn.logger::logger.info(
            "Reusing existing files from posterior", pft$posteriorid,
            "for PFT", shQuote(pft$name)
          )
          for (id in seq_len(nrow(files))) {
            file.copy(from = file.path(files[[id, "file_path"]], files[[id, "file_name"]]),
                      to = file.path(pft$outdir, files[[id, "file_name"]]))
          }

          done <- TRUE

          # May need to symlink the generic post.distns.Rdata to a specific post.distns.*.Rdata file.
          if (length(list.files(pft$outdir, "post.distns.Rdata")) == 0) {
            all.files <- list.files(pft$outdir)
            post.distn.file <- all.files[grep("post\\.distns\\..*\\.Rdata", all.files)]
            if (length(post.distn.file) > 1)
              PEcAn.logger::logger.severe(
                "get.trait.data.pft() doesn't know how to ",
                "handle multiple `post.distns.*.Rdata` files.",
                "Found the following files: ",
                paste(shQuote(post.distn.file), collapse = ", ")
              )
            else if (length(post.distn.file) == 1) {
              # Found exactly one post.distns.*.Rdata file. Use it.
              link_input <- file.path(pft[["outdir"]], post.distn.file)
              link_target <- file.path(pft[["outdir"]], "post.distns.Rdata")
              PEcAn.logger::logger.debug(
                "Found exactly one posterior distribution file: ",
                shQuote(link_input),
                ". Symlinking it to PFT output directory: ",
                shQuote(link_target)
              )
              file.symlink(from = link_input, to = link_target)
            } else {
              PEcAn.logger::logger.error(
                "No previous posterior distribution file found. ",
                "Most likely, trait data were retrieved, but meta-analysis ",
                "was not run. Meta-analysis will be run."
              )
              done <- FALSE
            }
          }
          if (done) return(pft)
        }
      }
    }
  }

  # get the trait data (including sampling of derived traits, if any)
  trait.data <- query.traits(pft_members$id, traits, con = dbcon,
                             update.check.only = FALSE,
                             ids_are_cultivars = (pfttype == "cultivar"))
  traits <- names(trait.data)

  if (length(trait.data) > 0) {
    trait_counts <- trait.data %>%
      dplyr::bind_rows(.id = "trait") %>%
      dplyr::count(trait)

    PEcAn.logger::logger.info(
      "\n Number of observations per trait for PFT ", shQuote(pft[["name"]]), ":\n",
      PEcAn.logger::print2string(trait_counts, n = Inf),
      wrap = FALSE
    )
  } else {
    PEcAn.logger::logger.warn(
      "None of the requested traits were found for PFT ",
      format(pft_members[["id"]], scientific = FALSE)
    )
  }

  # get list of existing files so they get ignored saving
  old.files <- list.files(path = pft$outdir)

  # create a new posterior
  now <- format(x = Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  db.query(paste0("INSERT INTO posteriors (pft_id, created_at, updated_at) ",
                  "VALUES (", pftid, ", '", now, "', '", now, "')"),
           con = dbcon)
  pft$posteriorid <- dplyr::tbl(dbcon, "posteriors") %>%
    dplyr::filter(pft_id == !!pftid, created_at == !!now) %>%
    dplyr::pull(id)

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## 1. get species/cultivar list based on pft
  utils::write.csv(pft_members, file.path(pft$outdir, pft_member_filename),
                   row.names = FALSE)

  ## save priors
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  utils::write.csv(prior.distns, file.path(pft$outdir, "prior.distns.csv"),
                   row.names = TRUE)

  ## 3. display info to the console
  PEcAn.logger::logger.info(
    "\n Summary of prior distributions for PFT ", shQuote(pft$name), ":\n",
    PEcAn.logger::print2string(prior.distns),
    wrap = FALSE
  )

  ## traits = variables with prior distributions for this pft
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  utils::write.csv(
    dplyr::bind_rows(trait.data),
    file.path(pft$outdir, "trait.data.csv"),
    row.names = FALSE
  )

  ### save and store in database all results except those that were there already
  store_files_all <- list.files(path = pft[["outdir"]])
  store_files <- setdiff(store_files_all, old.files)
  PEcAn.logger::logger.debug(
    "The following posterior files found in PFT outdir ",
    "(", shQuote(pft[["outdir"]]), ") will be registered in BETY ",
    "under posterior ID ", format(pft[["posteriorid"]], scientific = FALSE), ": ",
    paste(shQuote(store_files), collapse = ", "), ". ",
    "The following files (if any) will not be registered because they already existed: ",
    paste(shQuote(intersect(store_files, old.files)), collapse = ", "),
    wrap = FALSE
  )
  for (file in store_files) {
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    dbfile.insert(in.path = pathname, in.prefix = file,
                  type = "Posterior", id = pft[["posteriorid"]],
                  con = dbcon)
  }

  return(pft)
}

##--------------------------------------------------------------------------------------------------#
##' Get trait data from the database.
##'
##' This will use the following items from settings:
##' - `settings$pfts`
##' - `settings$model$type`
##' - `settings$database$bety`
##' - `settings$database$dbfiles`
##' - `settings$meta.analysis$update`
##' @param pfts the list of pfts to get traits for
##' @param modeltype type of model that is used, this is is used to distinguis between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param database database connection parameters (see `params`
##'   argument to [db.query()])
##' @param forceupdate (Logical) If `TRUE`, force a database update
##'   whether or not it is needed. If `FALSE`, only update if an
##'   update is needed.
##' @param trait.names Character vector of trait names to search. If
##'   `NULL` (default), use all traits that have a prior for at least
##'   one of the `pfts`.
##' @return list of PFTs with update posteriorids
##' @author David LeBauer, Shawn Serbin, Alexey Shiklomanov
##' @export
get.trait.data <- function(pfts, modeltype, dbfiles, database, forceupdate,
                           trait.names = NULL) {
  if (!is.list(pfts)) {
    PEcAn.logger::logger.severe('pfts must be a list')
  }
  # Check that all PFTs have associated outdir entries
  pft_outdirs <- lapply(pfts, '[[', 'outdir')
  if (any(sapply(pft_outdirs, is.null))) {
    PEcAn.logger::logger.severe('At least one pft in settings is missing its "outdir"')
  }

  dbcon <- db.open(database)
  on.exit(db.close(dbcon))

  if (is.null(trait.names)) {
    PEcAn.logger::logger.debug(paste0(
      "`trait.names` is NULL, so retrieving all traits ",
      "that have at least one prior for these PFTs."
    ))
    pft_names <- vapply(pfts, "[[", character(1), "name")
    pft_ids <- query_pfts(dbcon, pft_names, modeltype, strict = TRUE)[["id"]]
    # NOTE: Use `format` here to avoid implicit (incorrect) coercion
    # to double by `lapply`. This works fine if we switch to
    # `query_priors`, but haven't done so yet because that requires
    # prepared statements and therefore requires the Postgres driver. 
    all_priors_list <- lapply(format(pft_ids, scientific = FALSE), query.priors,
                              con = dbcon, trstr = trait.names)
    trait.names <- unique(unlist(lapply(all_priors_list, rownames)))
    # Eventually, can replace with this:
    # all_priors <- query_priors(pfts, params = database)
    # trait.names <- unique(all_priors[["name"]])
  }

  # process all pfts
  result <- lapply(pfts, get.trait.data.pft,
                   modeltype = modeltype,
                   dbfiles = dbfiles,
                   dbcon = dbcon,
                   forceupdate = forceupdate,
                   trait.names = trait.names)

  invisible(result)
}

#' Symmetric set difference of two data frames
#'
#' @param x,y `data.frame`s to compare
#' @param xname Label for data in x but not y. Default = "x"
#' @param yname Label for data in y but not x. Default = "y"
#' @param namecol Name of label column. Default = "source".
#' @param simplify_types (Logical) If `TRUE`, coerce anything that
#'   isn't numeric to character, to facilitate comparison.
#' @return `data.frame` of data not common to x and y, with additional
#'   column (`namecol`) indicating whether data are only in x
#'   (`xname`) or y (`yname`)
#' @export
#' @examples
#' xdf <- data.frame(a = c("a", "b", "c"),
#'                   b = c(1, 2, 3),
#'                   stringsAsFactors = FALSE)
#' ydf <- data.frame(a = c("a", "b", "d"),
#'                   b = c(1, 2.5, 3),
#'                   stringsAsFactors = FALSE)
#' symmetric_setdiff(xdf, ydf)
symmetric_setdiff <- function(x, y, xname = "x", yname = "y",
                              namecol = "source", simplify_types = TRUE) {
  stopifnot(is.data.frame(x), is.data.frame(y),
            is.character(xname), is.character(yname),
            length(xname) == 1, length(yname) == 1)
  is_i64 <- c(
    vapply(x, inherits, logical(1), what = "integer64"),
    vapply(y, inherits, logical(1), what = "integer64")
  )
  if (any(is_i64)) {
    PEcAn.logger::logger.debug(
      "Detected at least one `integer64` column. ",
      "Converting to `numeric` for comparison."
    )
    if (requireNamespace("bit64", quietly = TRUE)) {
      x <- dplyr::mutate_if(x, bit64::is.integer64, as.numeric)
      y <- dplyr::mutate_if(y, bit64::is.integer64, as.numeric)
    } else {
      PEcAn.logger::logger.warn(
        '"bit64" package required for `integer64` conversion, but not installed. ',
        "Skipping conversion, which may produce weird results!"
      )
    }
  }
  if (simplify_types) {
    x <- dplyr::mutate_if(x, ~!is.numeric(.), as.character)
    y <- dplyr::mutate_if(x, ~!is.numeric(.), as.character)
  }
  namecol <- dplyr::sym(namecol)
  xy <- dplyr::setdiff(x, y) %>%
    dplyr::mutate(!!namecol := xname)
  yx <- dplyr::setdiff(y, x) %>%
    dplyr::mutate(!!namecol := yname)
  dplyr::bind_rows(xy, yx) %>%
    dplyr::select(!!namecol, dplyr::everything())
}

####################################################################################################
### EOF.  End of R script file.
####################################################################################################
