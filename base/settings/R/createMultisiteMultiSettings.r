#' @title Create Sitegroup MultiSettings
#' 
#' @description  Helps to create a MultiSettings object to run some or all sites in a Sitegroup. 
#' 
#' @param templateSettings A \code{\link{Settings}} object that will be the template for the resulting
#' MultiSettings. 
#' @param sitegroupId The Bety ID of the sitegroup to draw from
#' @param nSite The number of sites to randomly select (without replacement) from the siteGroup. Omit
#' to use all sites in the group.
#' @param con,params Bety DB connection or parameters. passed directly to \code{\link[PEcAn.DB]{db.query}}
#' 
#' @details 
#' Starts with a template settings object, and fills in the \code{run} block with site info sampled from
#' the sitegroup. The template could be fully set up except for the site info, or more or less empty if you 
#' plan to fill in the other settings later. A \code{\link{MultiSettings}} is created from \code{templateSettings},
#' \code{nSite} sites (or all of them, if \code{nSite} is unset) are selected from Bety, and their info
#' is dropped into the \code{MultiSettings}.
#'   
#' @return A \code{MultiSettings} object with the same settings as \code{templateSettings} but site
#' information for the selected sites
#' 
#' @author Ryan Kelly
#' @export
#' 
#' @example examples/examples.MultiSite.MultiSettings.r
createSitegroupMultiSettings = function(templateSettings, sitegroupId, nSite, con=NULL, params=templateSettings$database$bety) {
  query <- paste("SELECT site_id FROM sitegroups_sites WHERE sitegroup_id =", sitegroupId)
  allSites <- PEcAn.DB::db.query(query, con=con, params=params)   
  
  if(missing(nSite))
    siteIds <- allSites$site_id
  else 
    siteIds <- sample(allSites$site_id, nSite, replace=FALSE)

  settings <- createMultiSiteSettings(templateSettings, siteIds)
}



#' @title Transform Settings into multi-site MultiSettings
#' 
#' @description Create a MultiSettings object containing (identical) run blocks for multiple different
#' sites
#' 
#' @param templateSettings A \code{\link{Settings}} object that will be the template for the resulting
#' MultiSettings. 
#' @param siteIds The site IDs to be used in the resulting MultiSettings

#' @details 
#' Starts with a template settings object, and duplicates the \code{run$site} block once for each
#' specified site ID. The resulting MultiSettings is thus identical to the input, except ready to run
#' for each site in the vector of site IDs.
#'   
#' @return A \code{MultiSettings} object with the same settings as \code{templateSettings} but replicated
#' \code{run$site} blocks, one for each specified site ID.
#' 
#' @author Ryan Kelly
#' @export
#' 
#' @example examples/examples.MultiSite.MultiSettings.r
createMultiSiteSettings <- function(templateSettings, siteIds) {
  templateSettings <- as.MultiSettings(templateSettings)
  runSettings <- lapply(siteIds, getRunSettings, templateSettings=templateSettings)
  
  templateSettings[["run", global=FALSE]] <- runSettings
  return(templateSettings)
}


#' Build run MultiSettings for a list of site id's
#'
#' @param templateSettings 
#' @param siteId 
#'
#' @return
#' @export
#'
#' @examples
getRunSettings <- function(templateSettings, siteId) {
  startDate = templateSettings$run$start.date
  endDate = templateSettings$run$end.date
  inputs = templateSettings$run$inputs
  return(list(
    site = list(
      id = siteId,
      met.start = startDate,
      met.end = endDate),
    start.date = startDate,
    end.date = endDate,
    inputs = inputs
  ))
}


#' @title Set the Ouptu Directories of PEcAn Settings
#' 
#' @description  Sets the main output directory and nulls out the others
#' 
#' @param settings A \code{\link{Settings}} object
#' @param outDir The desired output directory
#' 
#' @details 
#' Sets the main output directory (\code{settings$outdir}) to \code{outDir}, and sets numerous others
#' (\code{settings$modeloutdir}, \code{settings$host$rundir}, \code{settings$host$outdir}, 
#' \code{settings$host$modeloutdir}) to NULL so they will revert to defaults when 
#' \code{\link{check.settings}} is run.
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
    
  for(j in 1:length(settings$pfts)) {
    settings$pfts[[j]]$outdir <- NULL
  }
  
  return(settings)
}


#' @title Set the Dates of PEcAn Settings
#' 
#' @description  Sets the run, ensemble, and sensitivity analysis dates of PEcAn Settings
#' 
#' @param settings A \code{\link{Settings}} object
#' @param startDate,endDate The desired start and end dates
#' 
#' @details 
#' Sets the start/end dates in \code{settings$run} to the specified dates, and sets the corresponding 
#' years for \code{settings$ensemble} and \code{settings$sensitivity.analysis}. Either date can be 
#' omitted to leave it unchanged.
#'   
#' @return The original \code{Settings} object with updated dates
#' 
#' @author Ryan Kelly
#' @export
setDates <- function(settings, startDate, endDate) {
  if(!missing(startDate)) {
    settings$run$start.date <- startDate
   
    if(!is.null(settings$ensemble))
      settings$ensemble$start.year <- lubridate::year(startDate)

    if(!is.null(settings$sensitivity.analysis)) 
      settings$sensitivity.analysis$start.year <- lubridate::year(startDate)
  }
  
  if(!missing(endDate)) {
    settings$run$end.date <- endDate
   
    if(!is.null(settings$ensemble))
      settings$ensemble$end.year <- lubridate::year(endDate)

    if(!is.null(settings$sensitivity.analysis)) 
      settings$sensitivity.analysis$end.year <- lubridate::year(endDate)
  }
  
  return(settings)
}
