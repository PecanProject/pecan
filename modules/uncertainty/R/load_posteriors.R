#' Load posterior distributions and/or Monte Carlo samples for a PFT
#'
#' Detects posterior type by file contents, not filenames.
#' Monte Carlo samples (\code{trait.mcmc}) take precedence over
#' distribution summaries (\code{post.distns} / \code{prior.distns}).
#' Falls back to legacy \code{outdir} + DB lookup when
#' \code{posterior.file} is \code{NA}, with a deprecation warning.
#'
#' @param posterior.file path to a posterior file or directory, or \code{NA}.
#' @param outdir \strong{Deprecated.} Legacy PFT output directory used only
#'   when \code{posterior.file} is \code{NA}. Will be removed in a future
#'   version; pass an explicit \code{posterior.file} instead.
#' @param posteriorid \strong{Deprecated.} Posterior ID for DB lookup, used
#'   only in the legacy fallback path.
#' @param con \strong{Deprecated.} Database connection, used only in the
#'   legacy fallback path.
#' @param hostname \strong{Deprecated.} Hostname for DB file lookup, used
#'   only in the legacy fallback path.
#' @return list with \code{prior.distns}, \code{trait.mcmc}, \code{is.joint}.
#'   \code{is.joint} is \code{TRUE} when MCMC samples represent a joint
#'   posterior (e.g. from PDA) and parameter correlations should be preserved.
#' @keywords internal
#' @author Siddhey Patil
load.posteriors <- function(posterior.file,
                            outdir = NULL,
                            posteriorid = NULL,
                            con = NULL,
                            hostname = NULL) {
  if (!is.na(posterior.file)) {
    result <- load.posteriors.from.path(posterior.file)
  } else {
    PEcAn.logger::logger.warn(
      "Reading posterior from pft$outdir is deprecated. ",
      "Please specify posterior.files explicitly."
    )
    result <- load.posteriors.legacy(
      outdir = outdir,
      posteriorid = posteriorid,
      con = con,
      hostname = hostname
    )
  }

  return(result)
}


#' Load posteriors from an explicit path (file or directory)
#'
#' Scans \code{.Rdata} files at \code{path} and detects MCMC samples
#' by object class (\code{coda::is.mcmc.list}) rather than by variable
#' name.  Distribution summaries are detected by the names
#' \code{post.distns} and \code{prior.distns}.
#'
#' @param path file path or directory path
#' @return List with \code{prior.distns}, \code{trait.mcmc}, \code{is.joint}
#' @keywords internal
load.posteriors.from.path <- function(path) {
  result <- list(
    prior.distns = NULL,
    trait.mcmc = NULL,
    is.joint = FALSE
  )

  if (!file.exists(path)) {
    PEcAn.logger::logger.error(
      "Posterior path does not exist: ", path
    )
    return(result)
  }

  # Collect list of .Rdata files to scan
  if (dir.exists(path)) {
    rdata_files <- list.files(
      path,
      pattern = "\\.[Rr][Dd]ata$",
      full.names = TRUE
    )
    if (length(rdata_files) == 0) {
      PEcAn.logger::logger.warn(
        "No .Rdata files found in posterior directory: ", path
      )
      return(result)
    }
  } else {
    rdata_files <- path
  }

  # Load each file and detect posterior type by contents
  for (f in rdata_files) {
    env <- new.env(parent = emptyenv())
    tryCatch(
      load(f, envir = env),
      error = function(e) {
        PEcAn.logger::logger.warn(
          "Failed to load posterior file: ", f, " -- ", conditionMessage(e)
        )
      }
    )

    # Detect Monte Carlo samples by object class (coda::is.mcmc.list)
    # rather than assuming the variable is named "trait.mcmc"
    for (obj_name in ls(env)) {
      obj <- get(obj_name, envir = env)
      # trait.mcmc is a named list of mcmc.list objects (one per trait)
      if (is.list(obj) && !is.data.frame(obj) &&
          length(obj) > 0 &&
          all(vapply(obj, coda::is.mcmc.list, logical(1)))) {
        PEcAn.logger::logger.info(
          "Found Monte Carlo samples (", obj_name, ") in: ", f
        )
        result$trait.mcmc <- obj
        break
      }
      # Single mcmc.list (not wrapped in a named list)
      if (coda::is.mcmc.list(obj)) {
        PEcAn.logger::logger.info(
          "Found mcmc.list object (", obj_name, ") in: ", f
        )
        result$trait.mcmc <- stats::setNames(list(obj), obj_name)
        break
      }
    }

    # Detect distribution summaries (post.distns takes priority, prior.distns also accepted)
    if (exists("post.distns", envir = env, inherits = FALSE)) {
      PEcAn.logger::logger.info(
        "Found distribution summaries (post.distns) in: ", f
      )
      # Only set if not already populated by an earlier file
      if (is.null(result$prior.distns)) {
        result$prior.distns <- env$post.distns
      }
    }
    if (exists("prior.distns", envir = env, inherits = FALSE)) {
      PEcAn.logger::logger.info(
        "Found distribution summaries (prior.distns) in: ", f
      )
      if (is.null(result$prior.distns)) {
        result$prior.distns <- env$prior.distns
      }
    }
  }

  # Log precedence outcome
  if (!is.null(result$trait.mcmc) && !is.null(result$prior.distns)) {
    PEcAn.logger::logger.info(
      "Both Monte Carlo samples and distribution summaries found. ",
      "Monte Carlo samples will take precedence."
    )
  }

  # Detect joint posterior: check for a companion mcmc.pda.* file in the
  # same directory, which PDA always creates alongside trait.mcmc.pda.*
  if (!is.null(result$trait.mcmc)) {
    scan_dir <- if (dir.exists(path)) path else dirname(path)
    pda_companions <- list.files(
      scan_dir, pattern = "^mcmc\\.pda\\.", ignore.case = TRUE
    )
    if (length(pda_companions) > 0) {
      result$is.joint <- TRUE
      PEcAn.logger::logger.info(
        "Detected joint posterior (PDA companion file found in ",
        scan_dir, ")"
      )
    }
  }

  return(result)
}


#' Legacy fallback: load posteriors from outdir and/or database
#'
#' \strong{Deprecated.} This path is used only when
#' \code{posterior.file} is \code{NA} and will be removed in a
#' future release.
#'
#' @param outdir PFT output directory
#' @param posteriorid Posterior ID for database lookup
#' @param con db connection
#' @param hostname host name for dbfile lookup
#' @return List with \code{prior.distns}, \code{trait.mcmc}, \code{is.joint}
#' @keywords internal
load.posteriors.legacy <- function(outdir = NULL,
                                   posteriorid = NULL,
                                   con = NULL,
                                   hostname = NULL) {
  result <- list(
    prior.distns = NULL,
    trait.mcmc = NULL,
    is.joint = FALSE
  )

  ## Step 1: Load distribution summaries from outdir
  if (!is.null(outdir)) {
    fname <- file.path(outdir, "post.distns.Rdata")
    if (file.exists(fname)) {
      env <- new.env(parent = emptyenv())
      load(fname, envir = env)
      if (exists("post.distns", envir = env, inherits = FALSE)) {
        result$prior.distns <- env$post.distns
      }
    } else {
      prior_fname <- file.path(outdir, "prior.distns.Rdata")
      if (file.exists(prior_fname)) {
        env <- new.env(parent = emptyenv())
        load(prior_fname, envir = env)
        if (exists("prior.distns", envir = env, inherits = FALSE)) {
          result$prior.distns <- env$prior.distns
        }
      }
    }
  }

  ## Step 2: Try database lookup for MCMC files
  if (!is.null(posteriorid) && !is.null(con)) {
    files <- tryCatch(
      PEcAn.DB::dbfile.check(
        "Posterior", posteriorid,
        con, hostname,
        return.all = TRUE
      ),
      error = function(e) NULL
    )

    if (!is.null(files) && nrow(files) > 0) {
      # Scan all associated .Rdata files for trait.mcmc by content
      rdata_idx <- grep("\\.[Rr][Dd]ata$", files$file_name)
      for (idx in rdata_idx) {
        fpath <- file.path(files$file_path[idx], files$file_name[idx])
        if (file.exists(fpath)) {
          env <- new.env(parent = emptyenv())
          tryCatch(
            load(fpath, envir = env),
            error = function(e) {
              PEcAn.logger::logger.warn(
                "Failed to load DB posterior file: ", fpath,
                " -- ", conditionMessage(e)
              )
            }
          )
          if (exists("trait.mcmc", envir = env, inherits = FALSE)) {
            PEcAn.logger::logger.info(
              "Found Monte Carlo samples in DB-referenced file: ", fpath
            )
            result$trait.mcmc <- env$trait.mcmc
            # Detect PDA samples to preserve correlations
            if (exists("post.distns", envir = env, inherits = FALSE)) {
              result$prior.distns <- result$prior.distns %||% env$post.distns
            }
            # In the legacy DB path, detect joint posterior via filename
            # heuristic (PDA convention: trait.mcmc.pda.*)
            fname_lower <- tolower(files$file_name[idx])
            if (grepl("pda", fname_lower)) {
              result$is.joint <- TRUE
            }
            break # Use first MCMC found
          }
        }
      }
    }
  }

  ## Step 3: If no MCMC from DB, scan outdir for .Rdata files with trait.mcmc
  if (is.null(result$trait.mcmc) && !is.null(outdir) && dir.exists(outdir)) {
    rdata_files <- list.files(
      outdir,
      pattern = "\\.[Rr][Dd]ata$",
      full.names = TRUE
    )
    for (f in rdata_files) {
      env <- new.env(parent = emptyenv())
      tryCatch(
        load(f, envir = env),
        error = function(e) NULL
      )
      if (exists("trait.mcmc", envir = env, inherits = FALSE)) {
        PEcAn.logger::logger.info(
          "Found Monte Carlo samples in outdir file: ", f
        )
        result$trait.mcmc <- env$trait.mcmc
        # In the legacy outdir-scan path, detect joint posterior via
        # filename heuristic (PDA convention: trait.mcmc.pda.*)
        fname_lower <- tolower(basename(f))
        if (grepl("pda", fname_lower)) {
          result$is.joint <- TRUE
        }
        break
      }
    }
  }

  return(result)
}
