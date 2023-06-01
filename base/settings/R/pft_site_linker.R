#' @title site.pft.linkage
#'
#' @param settings pecan settings list.
#' @param site.pft.links dataframe. Your look up table should have two columns
#'   of site and pft with site ids under site column
#'   and pft names under pft column.
#'
#' @description This function creates the required tags inside pecan.xml
#'   to link sites with pfts given a look up table.
#'   If the required tags are already defined in the pecan xml
#'   then they will be updated.
#'  If there are multiple pfts that they need to be used for a site,
#'   each pft needs to have a separate row in the lookup table,
#'   resulting multiple rows for a site.
#' @return pecan setting list
#' @export site.pft.linkage
#'
#' @examples
#'\dontrun{
#' #setting up the Look up tables
#' site.pft.links <-tribble(
#'  ~site, ~pft,
#'  "1000025731", "temperate.broadleaf.deciduous1",
#'  "1000025731", "temperate.needleleaf.evergreen",
#'  "1000000048", "temperate.broadleaf.deciduous2",
#'  "772", "temperate.broadleaf.deciduous3",
#'  "763", "temperate.broadleaf.deciduous4"
#' )
#'
#' # sending a multi- setting xml file to the function
#' site.pft.linkage(settings,site.pft.links)
#'}
site.pft.linkage <- function(settings, site.pft.links) {

  # checking the LUT
  if (is.null(site.pft.links) || ncol(site.pft.links) != 2) {
   PEcAn.logger::logger.severe(
    "Your look up table should have two columns of site and pft",
    "with site ids under site column and pft names under pft column.")
  }

  # if it's not a multisetting put it still in a list
  if (!is.MultiSettings(settings)) {
    settings <- list(settings)
  }

  #for each site in this setting
  new.mset <- purrr::map(
    settings,
    function(site.setting) {
      site.pft <- NULL
      site.id <- (site.setting[["run"]])$site$id
      #if no site id was found
      if (is.null(site.id)) {
        PEcAn.logger::logger.warn(
          "Since your site xml tag does NOT have a site id,",
          "we can not assign a PFT to it. The site of this site is",
          (site.setting[["run"]])$site$name)
      } else {
        # see if we can find this site id in the LUT
        if (site.id %in% site.pft.links$site) {
          site.pft <- site.pft.links$pft[which(site.pft.links$site %in% site.id)]
        }
        # if there was a pft associated with that
        if (!is.null(site.pft)) {
          site.setting[["run"]]$site$site.pft <- stats::setNames(
            as.list(site.pft),
            rep("pft.name", length(site.pft))
          )
        }
      }
      return(site.setting)
    })

  #putting it in the right format depending if it's multisetting or not
  if (is.MultiSettings(settings)) {
    new.mset <- MultiSettings(new.mset)
  } else{
    new.mset <- new.mset[[1]]
  }

  return(new.mset)
}
