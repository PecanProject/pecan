#' Load PFT priors and posteriors from disk/database
#'
#' Resolves each PFT's output directory and reads its prior distributions and
#' (where available) MCMC posterior chains into memory. This is the disk/BETY
#' I/O step that used to live inside \code{\link{get.parameter.samples}}; it is
#' kept separate so that the sampling functions (\code{get_parameter_samples}
#' and the design generators) can share one loader rather than each carrying
#' their own copy.
#'
#' Posterior type is detected by content, not filename, via
#' \code{\link{load.posteriors}}: MCMC chains take precedence over distribution
#' summaries. When a PFT's posterior is a joint fit (e.g. from PDA), the
#' returned \code{independent} flag is set to \code{FALSE} so downstream
#' sampling preserves the parameter correlations.
#'
#' @param settings PEcAn settings object. Uses \code{settings$pfts} (name,
#'   outdir, posteriorid per PFT), \code{settings$database$bety} for the
#'   optional BETY lookup, and \code{settings$host$name}.
#' @param posterior.files character vector of posterior filenames, one per PFT,
#'   \code{NA} where none is specified. Length must match \code{settings$pfts}.
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{pft_names}{character vector of PFT names ("NULL" where unnamed).}
#'     \item{prior_distns_list}{list of prior distribution data frames, one per
#'       PFT (\code{NULL} entry where none was found).}
#'     \item{trait_mcmc_list}{list of trait MCMC results, one per PFT
#'       (\code{NULL} entry where the PFT has no MCMC chains).}
#'     \item{independent}{logical, \code{FALSE} if any PFT carries a joint
#'       posterior, \code{TRUE} otherwise.}
#'   }
#'
#' @seealso \code{\link{get_parameter_samples}} for the sampling step that
#'   consumes this output.
#'
#' @author David LeBauer, Shawn Serbin, Istem Fer, Om Kapale
#' @importFrom rlang %||%
#' @export
load_pft_posteriors <- function(settings,
                                posterior.files = rep(NA, length(settings$pfts))) {
  pfts <- settings$pfts

  if (length(pfts) != length(posterior.files)) {
    PEcAn.logger::logger.error(
      "settings$pfts and posterior.files should be the same length"
    )
  }

  ## Open database connection
  con <- NULL
  if (!is.null(settings$database$bety)) {
    con <- try(PEcAn.DB::db.open(settings$database$bety))
    on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
      PEcAn.logger::logger.warn(
        "We were not able to successfully establish a connection with Bety"
      )
    }
  } else {
    PEcAn.logger::logger.info(
      "No database connection parameters provided.",
      "Will not use Bety for parameter lookup."
    )
  }

  ## Get output directory info
  pft.names <- list()
  outdirs   <- list()
  for (i.pft in seq_along(pfts)) {
    pft.names[i.pft] <- pfts[[i.pft]]$name %||% "NULL"

    if (!is.null(pfts[[i.pft]]$outdir)) {
      outdirs[i.pft] <- pfts[[i.pft]]$outdir
    } else if (!is.null(con)) {
      outdirs[i.pft] <- unique(
        PEcAn.DB::dbfile.check(
          type = "Posterior",
          container.id = pfts[[i.pft]]$posteriorid,
          con = con
        )$file_path
      )
    } # else outdirs[i.pft] stays NULL and load.posteriors handles it
  }

  PEcAn.logger::logger.info("Selected PFT(s): ", pft.names)

  ## Load priors and posteriors for each PFT
  prior_distns_list <- vector("list", length(pft.names))
  trait_mcmc_list   <- vector("list", length(pft.names))
  independent       <- TRUE

  for (i in seq_along(pft.names)) {
    posterior <- load.posteriors(
      posterior.file = posterior.files[i],
      outdir         = unlist(outdirs[i]),
      posteriorid    = settings$pfts[[i]]$posteriorid,
      con            = con,
      hostname       = settings$host$name
    )

    if (!is.null(posterior$prior.distns)) {
      prior_distns_list[[i]] <- posterior$prior.distns
    }

    if (!is.null(posterior$trait.mcmc)) {
      trait_mcmc_list[[i]] <- posterior$trait.mcmc
      # Joint posteriors (e.g. from PDA) should preserve correlations
      if (posterior$is.joint) {
        independent <- FALSE
      }
    } # else trait_mcmc_list[[i]] stays NULL
  }

  list(
    pft_names         = pft.names,
    prior_distns_list = prior_distns_list,
    trait_mcmc_list   = trait_mcmc_list,
    independent       = independent
  )
}

