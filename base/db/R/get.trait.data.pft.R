##' Get trait data from the database for a single PFT
##'
##' Queries BETYdb for trait observations and prior distributions for a single
##' plant functional type (PFT). Results are saved to files in the PFT output
##' directory (`pft$outdir`), and also registered in the database as posterior
##' records when `write = TRUE`.
##'
##' @details
##' `pft` should be a list containing at least `name` and `outdir`, and
##' optionally `posteriorid` and `constants`.
##'
##' @md
##' @param pft list of settings for the pft whose traits to retrieve. See details.
##' @param modeltype type of model that is used, this is used to distinguish
##'   between different pfts with the same name.
##' @param dbfiles location where previous results are found
##' @param dbcon database connection
##' @param trait.names list of trait names to retrieve
##' @param forceupdate set this to true to force an update, auto will check to
##'   see if an update is needed.
##' @param write (Logical) If `TRUE` updated posteriors will be written to
##'   BETYdb.  Defaults to `FALSE`.
##' @param return_data (Logical) If `TRUE`, the returned `pft` list also
##'   includes `trait_data` and `prior_distns` as in-memory objects.
##'   Defaults to `FALSE` to preserve legacy behavior — when `pft` is
##'   embedded inside a `settings` object, attaching these data frames by
##'   default would inflate the settings and break serialization.
##' @return The updated \code{pft} list with \code{posteriorid} set to the
##'   ID of the (possibly new) posterior record in BETYdb. The posterior ID
##'   can be used to locate the output files (\code{trait.data.Rdata},
##'   \code{prior.distns.Rdata}, etc.) via BETYdb's \code{dbfiles} table.
##'
##'   When \code{return_data = TRUE}, the returned list also includes
##'   \code{trait_data} and \code{prior_distns} as in-memory objects, so
##'   downstream callers can use them without reloading from \code{pft$outdir}.
##'
##' @section File-based side effects:
##' The following files are written to \code{pft$outdir}:
##' \describe{
##'   \item{\code{trait.data.Rdata}}{Named list of trait data frames, one per
##'     trait that has observations. Read by \code{run.meta.analysis.pft()}.}
##'   \item{\code{prior.distns.Rdata}}{Data frame of prior distributions, rows
##'     named by trait. Read by \code{run.meta.analysis.pft()}.}
##'   \item{\code{species.csv} or \code{cultivars.csv}}{Data frame of PFT
##'     member species or cultivar IDs, depending on PFT type.}
##'   \item{\code{trait.data.csv}}{Flattened CSV of all trait observations,
##'     one row per observation, for human inspection.}
##' }
##' In addition, each output file is registered in BETYdb via
##' \code{dbfile.insert()} and associated with the posterior record whose ID
##' is returned as \code{pft$posteriorid}.
##'
##' @section Downstream contract:
##' \code{run.meta.analysis.pft()} reads \code{trait.data.Rdata} and
##' \code{prior.distns.Rdata} from \code{pft$outdir} at the start of the
##' meta-analysis step. These two files are therefore a required output of
##' this wrapper.
##'
##' Note: this file-based contract will be removed once
##' \code{run.meta.analysis.pft()} is refactored to accept in-memory inputs
##' (GSoC Week 2).
##'
##' @author David LeBauer, Shawn Serbin, Rob Kooper
##' @export
get.trait.data.pft <- function(pft, modeltype, dbfiles, dbcon,
                               trait.names = NULL,
                               forceupdate = FALSE,
                               write = TRUE,
                               return_data = FALSE) {

  # Create directory if necessary
  if (!file.exists(pft$outdir) && !dir.create(pft$outdir, recursive = TRUE)) {
    PEcAn.logger::logger.error(
      paste0("Couldn't create PFT output directory: ", pft$outdir)
    )
  }

  # Backwards-compatible: forceupdate may arrive as "AUTO" or other strings
  forceupdate <- isTRUE(as.logical(forceupdate))

  # All database queries are delegated to get_trait_data_pft() exactly once.
  # The returned objects feed both the cache-staleness check and the save
  # step, so the database is never queried more than once per call.
  # Input validation (pft_name, trait_names, dbcon) and the pft_type guard
  # also live inside get_trait_data_pft(); errors surface from there.
  computed <- get_trait_data_pft(
    pft_name    = pft[["name"]],
    modeltype   = modeltype,
    dbcon       = dbcon,
    trait_names = trait.names,
    constants   = if (!is.null(pft$constants)) pft$constants else list()
  )
  trait.data          <- computed$trait_data
  prior.distns        <- computed$prior_distns
  pft_members         <- computed$pft_info$pft_members
  pftid               <- computed$pft_info$pft_id
  pfttype             <- computed$pft_info$pft_type
  pft_member_filename <- computed$pft_info$pft_member_filename

  # ---- Cache staleness check ----
  if (!forceupdate) {
    if (is.null(pft$posteriorid)) {
      recent_posterior <- dplyr::tbl(dbcon, "posteriors") |>
        dplyr::filter(.data$pft_id == !!pftid) |>
        dplyr::collect()
      if (length(recent_posterior) > 0) {
        pft$posteriorid <- dplyr::tbl(dbcon, "posteriors") |>
          dplyr::filter(.data$pft_id == !!pftid) |>
          dplyr::arrange(dplyr::desc(.data$created_at)) |>
          utils::head(1) |>
          dplyr::pull("id")
      } else {
        PEcAn.logger::logger.info("No previous posterior found. Forcing update")
      }
    }

    if (!is.null(pft$posteriorid)) {
      files <- dbfile.check(type = "Posterior", container.id = pft$posteriorid,
                            con = dbcon, return.all = TRUE)
      need_files <- c(
        trait_data     = "trait.data.Rdata",
        priors         = "prior.distns.Rdata",
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

        # Check all required files exist on disk
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
            colClass <- c("double", "character", "character", "character")
          } else if (pfttype == "cultivar") {
            # Columns are: id, specie_id, genus, species, scientificname, cultivar
            colClass <- c("double", "double", "character", "character",
                          "character", "character")
          }
          existing_membership <- utils::read.csv(
            need_paths[["pft_membership"]],
            colClasses       = colClass,
            stringsAsFactors = FALSE,
            na.strings       = c("", "NA")
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
          existing_prior <- PEcAn.utils::load_local(
            need_paths[["priors"]]
          )[["prior.distns"]]
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
          if (length(trait.data) != length(existing_trait_data)) {
            PEcAn.logger::logger.warn(
              "Lengths of new and existing `trait.data` differ. ",
              "Re-running meta-analysis."
            )
            foundallfiles <- FALSE
          } else if (length(trait.data) == 0) {
            PEcAn.logger::logger.warn(
              "New and existing trait data are both empty. Skipping this check."
            )
          } else {
            current_traits <- dplyr::bind_rows(trait.data, .id = "trait") |>
              dplyr::select(-mean, -"stat")
            existing_traits <- dplyr::bind_rows(existing_trait_data,
                                                .id = "trait") |>
              dplyr::select(-mean, -"stat")
            diff_traits <- symmetric_setdiff(current_traits, existing_traits)
            if (nrow(diff_traits) > 0) {
              diff_summary <- diff_traits |>
                dplyr::count(source, .data$trait)
              PEcAn.logger::logger.error(
                "\n Trait data has changed. \n",
                "Here are the number of differing trait records by trait:\n",
                PEcAn.logger::print2string(diff_summary),
                wrap = FALSE
              )
              foundallfiles <- FALSE
            }
          }
        } # end else (all files on disk)

        if (foundallfiles) {
          PEcAn.logger::logger.info(
            "Reusing existing files from posterior", pft$posteriorid,
            "for PFT", shQuote(pft$name)
          )
          for (id in seq_len(nrow(files))) {
            file.copy(
              from = file.path(files[[id, "file_path"]], files[[id, "file_name"]]),
              to   = file.path(pft$outdir, files[[id, "file_name"]])
            )
          }

          done <- TRUE

          # May need to symlink the generic post.distns.Rdata to the
          # model-specific post.distns.*.Rdata file.
          if (length(list.files(pft$outdir, "post.distns.Rdata")) == 0) {
            all.files <- list.files(pft$outdir)
            post.distn.file <- all.files[grep("post\\.distns\\..*\\.Rdata",
                                              all.files)]
            if (length(post.distn.file) > 1) {
              PEcAn.logger::logger.severe(
                "get.trait.data.pft() doesn't know how to ",
                "handle multiple `post.distns.*.Rdata` files.",
                "Found the following files: ",
                paste(shQuote(post.distn.file), collapse = ", ")
              )
            } else if (length(post.distn.file) == 1) {
              link_input  <- file.path(pft[["outdir"]], post.distn.file)
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

          if (done) {
            if (return_data) {
              pft$trait_data   <- trait.data
              pft$prior_distns <- prior.distns
            }
            return(pft)
          }
        }
      } # end else (all files in DB)
    } # end if (!is.null(pft$posteriorid))
  } # end if (!forceupdate)

  # ---- Cache miss: log counts, save to disk, register in DB ----
  if (length(trait.data) > 0) {
    trait_counts <- trait.data |>
      dplyr::bind_rows(.id = "trait") |>
      dplyr::count(.data$trait)
    PEcAn.logger::logger.info(
      "\n Number of observations per trait for PFT ", shQuote(pft[["name"]]),
      ":\n",
      PEcAn.logger::print2string(trait_counts, n = Inf, na.print = ""),
      wrap = FALSE
    )
  } else {
    PEcAn.logger::logger.warn(
      "None of the requested traits were found for PFT ",
      format(pft_members[["id"]], scientific = FALSE)
    )
  }

  # Snapshot existing files so we know which ones are new after saving
  old.files <- list.files(path = pft$outdir)

  # Create a new posterior record in BETYdb
  insert_result <- db.query(
    paste0("INSERT INTO posteriors (pft_id) VALUES (", pftid,
           ") RETURNING id"),
    con = dbcon
  )
  pft$posteriorid <- insert_result[["id"]]

  # Create the storage path for this posterior
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ## Write species/cultivar membership list
  utils::write.csv(pft_members,
                   file.path(pft$outdir, pft_member_filename),
                   row.names = FALSE)

  ## Save prior distributions
  save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
  utils::write.csv(prior.distns,
                   file.path(pft$outdir, "prior.distns.csv"),
                   row.names = TRUE)

  PEcAn.logger::logger.info(
    "\n Summary of prior distributions for PFT ", shQuote(pft$name), ":\n",
    PEcAn.logger::print2string(prior.distns),
    wrap = FALSE
  )

  ## Save trait data
  trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
  save(trait.data, file = trait.data.file)
  utils::write.csv(
    dplyr::bind_rows(trait.data),
    file.path(pft$outdir, "trait.data.csv"),
    row.names = FALSE
  )

  ## Register new files in BETYdb
  if (isTRUE(write)) {
    store_files_all <- list.files(path = pft[["outdir"]])
    store_files <- setdiff(store_files_all, old.files)
    PEcAn.logger::logger.debug(
      "The following posterior files found in PFT outdir ",
      "(", shQuote(pft[["outdir"]]), ") will be registered in BETY ",
      "under posterior ID ",
      format(pft[["posteriorid"]], scientific = FALSE), ": ",
      paste(shQuote(store_files), collapse = ", "), ". ",
      "The following files (if any) will not be registered because they ",
      "already existed: ",
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

  if (return_data) {
    pft$trait_data   <- trait.data
    pft$prior_distns <- prior.distns
  }
  return(pft)
}