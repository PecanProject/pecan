#' @title Create Sitegroup MultiSettings
#'
#' @description Helps to create a MultiSettings object to run some or all
#'   sites in a Sitegroup.
#'
#' @param templateSettings A \code{\link{Settings}} object that will be the
#'   template for the resulting MultiSettings.
#' @param sitegroupId The Bety ID of the sitegroup to draw from
#' @param nSite The number of sites to randomly select (without replacement)
#'   from the siteGroup. Omit to use all sites in the group.
#' @param con,params Bety DB connection or parameters.
#'   Passed directly to \code{\link[PEcAn.DB]{db.query}}
#'
#' @details
#' Starts with a template settings object, and fills in the \code{run} block
#'  with site info sampled from the sitegroup.
#'  The template could be fully set up except for the site info,
#'  or more or less empty if you plan to fill in the other settings later.
#'  A \code{\link{MultiSettings}} is created from \code{templateSettings},
#'  \code{nSite} sites (or all of them, if \code{nSite} is unset) are selected
#'  from Bety, and their info is dropped into the \code{MultiSettings}.
#'
#' @return A \code{MultiSettings} object with the same settings as
#'  \code{templateSettings} but site information for the selected sites
#'
#' @author Ryan Kelly
#' @export
#'
#' @example examples/examples.MultiSite.MultiSettings.r
createSitegroupMultiSettings <- function(
    templateSettings,
    sitegroupId,
    nSite,
    con = NULL,
    params = templateSettings$database$bety) {
  query <- paste(
    "SELECT site_id FROM sitegroups_sites WHERE sitegroup_id =", sitegroupId)
  allSites <- PEcAn.DB::db.query(query, con = con, params = params)

  if (missing(nSite)) {
    siteIds <- allSites$site_id
  } else {
    siteIds <- sample(allSites$site_id, nSite, replace = FALSE)
  }

  createMultiSiteSettings(templateSettings, siteIds)
}



#' @title Transform Settings into multi-site MultiSettings
#'
#' @description Create a MultiSettings object containing (identical) run blocks
#'   for multiple different sites
#'
#' @param templateSettings A \code{\link{Settings}} object that will be
#'   the template for the resulting MultiSettings.
#' @param siteIds The site IDs to be used in the resulting MultiSettings.
#'   May be a vector or a data frame with a mandatory \code{id} column and
#'   optionally other columns to be copied into \code{run$site}. See details.
#'
#' @details
#' Starts with a template settings object, and duplicates the \code{run$site}
#'   block once for each specified site ID. The resulting MultiSettings is thus
#'   identical to the input, except ready to run for each site in the vector
#'   of site IDs.
#' If \code{siteIds} is a data frame with a column named \code{id},
#'   each resulting \code{run$site} block will contain all the site parameters
#'   (lat, lon, site name, etc) that are specified in its other columns.
#'
#' @return A \code{MultiSettings} object with the same settings as
#'   \code{templateSettings} but replicated \code{run$site} blocks,
#'   one for each specified site ID.
#'
#' @author Ryan Kelly
#' @export
#'
#' @example examples/examples.MultiSite.MultiSettings.r
createMultiSiteSettings <- function(templateSettings, siteIds) {
  templateSettings <- as.MultiSettings(templateSettings)

  if (is.data.frame(siteIds)) {
    ids <- siteIds$id
    # get rows as lists: (id=(1, 2), x=(a,b)) -> ((id=1, x=a), (id=2, x=b))
    siteIds <- .mapply(FUN = list, dots = siteIds, MoreArgs = NULL)
  } else {
    ids <- siteIds
  }

  runSettings <- lapply(
    siteIds,
    getRunSettings,
    templateSettings = templateSettings)
  templateSettings[["run", global = FALSE]] <- runSettings
  templateSettings <- settingNames(templateSettings, paste0("site.", ids))
  return(templateSettings)
}


#' Build run MultiSettings for a single site id
#'
#' Processes one site from the `siteIds` argument of `createMultiSiteSettings`.
#' You probably don't need to call it directly.
#'
#' @inheritParams createMultiSiteSettings
#' @param siteId site to process. See `createMultiSiteSettings`
getRunSettings <- function(templateSettings, siteId) {
  startDate <- templateSettings$run$start.date
  endDate <- templateSettings$run$end.date
  inputs <- templateSettings$run$inputs

  if (is.list(siteId)) {
    if (is.null(siteId$id)) {
      PEcAn.logger::logger.error("Need a site ID for every site")
    }
    site_info <- siteId
  } else {
    site_info <- list(id = siteId)
  }
  site_info$met.start <- startDate
  site_info$met.end <- endDate

  return(list(
    site = site_info,
    start.date = startDate,
    end.date = endDate,
    inputs = inputs
  ))
}


#' @title Set the Output Directories of PEcAn Settings
#'
#' @description  Sets the main output directory and nulls out the others
#'
#' @param settings A \code{\link{Settings}} object
#' @param outDir The desired output directory
#'
#' @details
#' Sets the main output directory (\code{settings$outdir}) to \code{outDir},
#'   and sets numerous others (\code{settings$modeloutdir},
#'   \code{settings$host$rundir}, \code{settings$host$outdir},
#'   \code{settings$host$modeloutdir}) to NULL so they will revert to defaults
#'   when \code{\link{check.settings}} is run.
#'
#' @return The original \code{Settings} object with updated output directories
#'
#' @author Ryan Kelly
#' @export
setOutDir <- function(settings, outDir) {
  settings$outdir <- outDir
  settings$rundir <- NULL
  settings$modeloutdir <- NULL
  settings$host$rundir <- NULL
  settings$host$outdir <- NULL
  settings$host$modeloutdir <- NULL

  for (j in seq_along(settings$pfts)) {
    settings$pfts[[j]]$outdir <- NULL
  }

  return(settings)
}


#' @title Set the Dates of PEcAn Settings
#'
#' @description Sets the run, ensemble, and sensitivity analysis dates
#'   of PEcAn Settings
#'
#' @param settings A \code{\link{Settings}} object
#' @param startDate,endDate The desired start and end dates
#'
#' @details
#' Sets the start/end dates in \code{settings$run} to the specified dates,
#'  and sets the corresponding years for \code{settings$ensemble} and
#'  \code{settings$sensitivity.analysis}.
#' Either date can be omitted to leave it unchanged.
#'
#' @return The original \code{Settings} object with updated dates
#'
#' @author Ryan Kelly
#' @export
setDates <- function(settings, startDate, endDate) {
  if (!missing(startDate)) {
    settings$run$start.date <- startDate

    if (!is.null(settings$ensemble)) {
      settings$ensemble$start.year <- lubridate::year(startDate)
    }

    if (!is.null(settings$sensitivity.analysis)) {
      settings$sensitivity.analysis$start.year <- lubridate::year(startDate)
    }
  }

  if (!missing(endDate)) {
    settings$run$end.date <- endDate

    if (!is.null(settings$ensemble)) {
      settings$ensemble$end.year <- lubridate::year(endDate)
    }

    if (!is.null(settings$sensitivity.analysis)) {
      settings$sensitivity.analysis$end.year <- lubridate::year(endDate)
    }
  }

  return(settings)
}
