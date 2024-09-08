##' Get trait data from the database for a single PFT
##'
##' @details `pft` should be a list containing at least `name` and `outdir`, and optionally `posteriorid` and `constants`. BEWARE: All existing files in `outir` will be deleted!
##' @param pft list of settings for the pft whose traits to retrieve. See details
##' @param modeltype type of model that is used, this is used to distinguish between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param forceupdate set this to true to force an update, auto will check to see if an update is needed.
##' @param write (Logical) If `TRUE` updated posteriors will be written to
##'   BETYdb.  Defaults to FALSE.
##' @param trait.names list of trait names to retrieve
##' @return updated pft with posteriorid
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
get.trait.data.pft <-
  function(pft,
           modeltype,
           dbfiles,
           dbcon,
           trait.names,
           forceupdate = FALSE,
           write = FALSE) {
    

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
  prior.distns <- PEcAn.DB::query.priors(
    pft = pftid,
    trstr = PEcAn.utils::vecpaste(trait.names),
    con = dbcon)
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in% names(pft$constants)),]
  traits <- rownames(prior.distns)

  # get the trait data (don't bother sampling derived traits until after update check)
  trait.data.check <- PEcAn.DB::query.traits(ids = pft_members$id, priors = traits, con = dbcon, update.check.only = TRUE, ids_are_cultivars = (pfttype=="cultivar"))
  traits <- names(trait.data.check)

  # Set forceupdate FALSE if it's a string (backwards compatible with 'AUTO' flag used in the past)
  forceupdate <- isTRUE(as.logical(forceupdate))

  # check to see if we need to update
  if (!forceupdate) {
    if (is.null(pft$posteriorid)) {
      recent_posterior <- dplyr::tbl(dbcon, "posteriors") %>%
        dplyr::filter(.data$pft_id == !!pftid) %>%
        dplyr::collect()
      if (length(recent_posterior) > 0) {
        pft$posteriorid <- dplyr::tbl(dbcon, "posteriors") %>%
          dplyr::filter(.data$pft_id == !!pftid) %>%
          dplyr::arrange(dplyr::desc(.data$created_at)) %>%
          utils::head(1) %>%
          dplyr::pull("id")
      } else {
        PEcAn.logger::logger.info("No previous posterior found. Forcing update")
      }
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
          if (pfttype == "plant") {
            # Columns are: id, genus, species, scientificname
            colClass = c("double", "character", "character", "character")
          } else if (pfttype == "cultivar") {
            # Columns are: id, specie_id, genus, species, scientificname, cultivar
            colClass = c("double", "double", "character", "character", "character", "character")
            }
          existing_membership <- utils::read.csv(
            need_paths[["pft_membership"]],
            # Need this so NA values are formatted consistently
            colClasses = colClass,
            stringsAsFactors = FALSE,
            na.strings = c("", "NA")
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
              dplyr::select(-mean, -"stat")
            existing_traits <- dplyr::bind_rows(existing_trait_data, .id = "trait") %>%
              dplyr::select(-mean, -"stat")
            diff_traits <- symmetric_setdiff(current_traits, existing_traits)
            if (nrow(diff_traits) > 0) {
              diff_summary <- diff_traits %>%
                dplyr::count(source, .data$trait)
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
      dplyr::count(.data$trait)

    PEcAn.logger::logger.info(
      "\n Number of observations per trait for PFT ", shQuote(pft[["name"]]), ":\n",
      PEcAn.logger::print2string(trait_counts, n = Inf, na.print = ""),
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
  insert_result <- db.query(
    paste0("INSERT INTO posteriors (pft_id) VALUES (", pftid, ") RETURNING id"),
    con = dbcon)
  pft$posteriorid <- insert_result[["id"]]

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
  if(isTRUE(write)) {
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
  }

  return(pft)
}
