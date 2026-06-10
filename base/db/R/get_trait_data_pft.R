#' Retrieve trait data and priors for one PFT from BETYdb
#'
#' Core computation extracted from \code{\link{get.trait.data.pft}}.
#' Queries the database for trait observations and prior distributions,
#' returning them as R objects with no file I/O of any kind.
#'
#' The wrapper \code{\link{get.trait.data.pft}} handles directory creation,
#' caching, CSV output, and BETYdb registration via \code{dbfile.insert}.
#' This function handles only the query. Choosing between them is the
#' provenance opt-in: calling the wrapper saves artifacts to disk; calling
#' this function never does.
#'
#' This follows the pattern established by \code{meta_analysis_standalone}
#' for the meta-analysis step and \code{get_parameter_samples} for parameter
#' sampling — each is a computation core that can be tested in isolation
#' without a filesystem or a \code{settings} object.
#'
#' @param pft_name character. PFT name as stored in BETYdb.
#' @param modeltype character or NULL. Disambiguates PFTs that share a name
#'   across model types (e.g. \code{"SIPNET"}, \code{"ED2"}).
#' @param dbcon database connection from \code{\link[PEcAn.DB]{db.open}}.
#' @param trait_names character vector of trait names to retrieve.
#' @param constants named list from \code{pft$constants} in the settings.
#'   Traits named here are excluded from the returned priors because their
#'   values are fixed rather than sampled by the meta-analysis.
#'
#' @return Named list with three elements:
#' \describe{
#'   \item{\code{trait_data}}{Named list of data frames, one per trait that
#'     has observations. Column structure matches what
#'     \code{meta_analysis_standalone} expects. Traits with no observations
#'     are omitted from the list.}
#'   \item{\code{prior_distns}}{Data frame with columns \code{distn},
#'     \code{parama}, \code{paramb}, \code{n}; rows named by trait. Traits
#'     listed in \code{constants} are excluded.}
#'   \item{\code{pft_info}}{List with \code{name}, \code{pft_id},
#'     \code{pft_type}, \code{pft_members}, \code{pft_member_filename}, and
#'     \code{posteriorid}. \code{pft_members} is the data frame of species or
#'     cultivar IDs used during the query. \code{pft_member_filename} is
#'     \code{"species.csv"} or \code{"cultivars.csv"} depending on PFT type.
#'     \code{posteriorid} is always \code{NULL} — the wrapper sets it after
#'     registering outputs in BETYdb.}
#' }
#'
#' @seealso \code{\link{get.trait.data.pft}} for the backward-compatible
#'   wrapper that handles provenance and caching.
#'   \code{meta_analysis_standalone} (in PEcAn.MA) for the analogous
#'   function in the meta-analysis step.
#'   \code{get_parameter_samples} for the analogous function in the
#'   parameter sampling step.
#'
#' @examples
#' \dontrun{
#' dbcon <- PEcAn.DB::db.open(list(
#'   host = "localhost", user = "bety",
#'   password = "bety",  dbname = "bety"
#' ))
#' result <- get_trait_data_pft(
#'   pft_name    = "temperate.deciduous",
#'   modeltype   = "SIPNET",
#'   dbcon       = dbcon,
#'   trait_names = c("SLA", "Vcmax", "leaf_respiration_rate_m2")
#' )
#' str(result$trait_data)
#' str(result$prior_distns)
#' PEcAn.DB::db.close(dbcon)
#' }
#'
#' @author David LeBauer, Shawn Serbin, Alexey Shiklomanov, Om Kapale
#' @export
get_trait_data_pft <- function(pft_name,
                               modeltype,
                               dbcon,
                               trait_names,
                               constants = list()) {

  # ---- Input validation (cheap checks before any DB call) ----
  if (!is.character(pft_name) || length(pft_name) != 1L) {
    PEcAn.logger::logger.severe("'pft_name' must be a single character string")
  }
  if (!is.character(trait_names) || length(trait_names) == 0L) {
    PEcAn.logger::logger.severe(
      "'trait_names' must be a non-empty character vector"
    )
  }
  if (!inherits(dbcon, "DBIConnection")) {
    PEcAn.logger::logger.severe("'dbcon' must be a database connection")
  }

 # ---- Resolve PFT to a single database record ----
 # Note: query_pfts(strict = TRUE) only catches missing PFTs, not multi-PFT
 # results (setequal() ignores duplicates). So we do both checks explicitly
 # here and match the wrapper's canonical error wording for test consistency.
 pft_record <- query_pfts(dbcon, pft_name, modeltype)

 if (nrow(pft_record) == 0L) {
   PEcAn.logger::logger.severe("PFTs were not found:", pft_name)
 }

 if (nrow(pft_record) > 1L) {
   PEcAn.logger::logger.severe(
     "Multiple PFTs named", pft_name, "found,",
     "with ids", PEcAn.utils::vecpaste(pft_record[["id"]]),
     ". Specify modeltype to fix this."
   )
 }

  pft_id   <- pft_record[["id"]]
  pft_type <- pft_record[["pft_type"]]

  PEcAn.logger::logger.info(
    "Querying trait data for PFT '", pft_name, "' (id = ", pft_id, ")"
  )

  # ---- Fetch PFT member species or cultivars ----
  # The join table depends on pft_type.  An unknown type is a hard error —
  # silently falling through to the species path would produce wrong results.
  if (identical(pft_type, "cultivar")) {
    pft_member_filename <- "cultivars.csv"
    members <- query.pft_cultivars(pft = pft_name, modeltype = modeltype,
                                   con = dbcon)
  } else if (identical(pft_type, "plant")) {
    pft_member_filename <- "species.csv"
    members <- query.pft_species(pft = pft_name, modeltype = modeltype,
                                 con = dbcon)
  } else {
    PEcAn.logger::logger.severe(
      "Unknown pft_type '", pft_type, "' for PFT '", pft_name,
      "'; expected 'plant' or 'cultivar'."
    )
  }

  # Normalise empty strings to NA so membership comparisons are consistent
  members <- members |>
    dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        \(x) dplyr::na_if(x, "")
    ))

  member_ids <- members[["id"]]

  if (length(member_ids) == 0L) {
    PEcAn.logger::logger.info(
      "PFT '", pft_name, "' has no associated ",
      if (identical(pft_type, "cultivar")) "cultivars" else "species",
      "; trait_data will be an empty list."
    )
  }

  # ---- Query prior distributions ----
  # format() prevents integer64 from being silently coerced to double in SQL.
  prior_distns <- query.priors(
    pft   = format(pft_id, scientific = FALSE),
    trstr = PEcAn.utils::vecpaste(trait_names),
    con   = dbcon
  )

  # Exclude traits listed in pft$constants — their values are fixed and must
  # not be sampled by the meta-analysis.
  if (length(constants) > 0L && !is.null(names(constants))) {
    constant_traits <- names(constants)
    in_constants    <- rownames(prior_distns) %in% constant_traits
    if (any(in_constants)) {
      PEcAn.logger::logger.info(
        "Excluding ", sum(in_constants), " constant trait(s) from priors: ",
        PEcAn.utils::vecpaste(rownames(prior_distns)[in_constants])
      )
      prior_distns <- prior_distns[!in_constants, , drop = FALSE]
    }
  }

  # ---- Query trait observations ----
  # Only query traits that actually have a prior — querying without a prior
  # is meaningless for meta-analysis.
  traits_with_priors <- rownames(prior_distns)
  if (length(member_ids) > 0L && length(traits_with_priors) > 0L) {
    trait_data <- query.traits(
      ids               = member_ids,
      priors            = traits_with_priors,
      con               = dbcon,
      ids_are_cultivars = identical(pft_type, "cultivar")
    )
  } else {
    trait_data <- list()
  }

  PEcAn.logger::logger.info(
    "PFT '", pft_name, "': ",
    length(trait_data), " trait(s) with observations, ",
    nrow(prior_distns), " trait(s) with priors"
  )

  # posteriorid is always NULL here — the wrapper assigns it after registering
  # the output files in BETYdb via dbfile.insert().
  # pft_members and pft_member_filename are included so the wrapper can use
  # them for cache comparison and CSV output without re-querying the database.
  pft_info <- list(
    name                = pft_name,
    pft_id              = pft_id,
    pft_type            = pft_type,
    pft_members         = members,
    pft_member_filename = pft_member_filename,
    posteriorid         = NULL
  )

  return(list(
    trait_data   = trait_data,
    prior_distns = prior_distns,
    pft_info     = pft_info
  ))
}