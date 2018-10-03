#'
#' @title site.pft.linkage
#' 
#' @param pecan.xml.address character path to the pecan xml file.
#' @param site.pft.links dataframe. Your look up table should have two columns of site and pft with site ids under site column and pft names under pft column.
#'
#' 
#' @description This function creates the required tags inside pecan.xml to link sites with pfts given a look up table. If the required tags are already defined in the pecan xml then they will be updated.
#' 
#' @return NONE
#' @export
#' 
#' @examples
#'\dontrun{
#' #setting up the Look up tables
#' site.pft.links <-tribble(
#'  ~site, ~pft,
#'  "1000025731", "temperate.broadleaf.deciduous1",
#'  "1000025731", "temperate.broadleaf.deciduous11",
#'  "1000000048", "temperate.broadleaf.deciduous2",
#'  "772", "temperate.broadleaf.deciduous3",
#'  "763", "temperate.broadleaf.deciduous4"
#' )
#'  
#' # sending a single setting xml to the function
#' site.pft.linkage('pecan.xml',site.pft.links)
#' # sending a multi- setting xml file to the function
#' site.pft.linkage('pecan.SDA.4site.xml',site.pft.links)
#'}
site.pft.linkage <- function(pecan.xml.address='', site.pft.links){
  
  # checking the LUT 
  if (is.null(site.pft.links) | ncol(site.pft.links) !=2) PEcAn.logger::logger.severe('Your look up table should have two columns of site and pft with site ids under site column and pft names under pft column.')
  #finding the file name
  filename<-strsplit(pecan.xml.address,'.xml$')[[1]]
  #reading in the xml file
  msetting<-read.settings(pecan.xml.address)
  # if it's not a multisetting put it still in a list
  if(!is.MultiSettings(msetting)) msetting<- list(msetting) 
  
  #for each site in this setting
  new.mset <- msetting%>%
    purrr::map(function(site.setting){
      
      site.pft <- NULL
      site.id <- (site.setting[['run']])$site$id
      #if no site id was found
      if (is.null(site.id)) PEcAn.logger::logger.severe('Your site needs to have a site id.')
      # see if we can find this site id in the LUT
      if (site.id %in% site.pft.links$site) site.pft <-site.pft.links$pft[which(site.pft.links$site %in% site.id)]
      # if there was a pft associated with that
      if (!is.null(site.pft)) site.setting[['run']]$site$site.pft <- as.list(site.pft) %>% setNames(rep('pft.name', length(site.pft)))
      site.setting
    })
  
  #putting it in the right format depending if it's multisetting or not 
  if (is.MultiSettings(msetting)) {
    new.mset <- MultiSettings(new.mset)
    outdir <-new.mset[[1]]$outdir
  } else{
    new.mset <- new.mset[[1]]
    outdir <-new.mset$outdir
  }
  
  write.settings(new.mset, paste0(filename,".site_pft.xml"), outdir)
}

