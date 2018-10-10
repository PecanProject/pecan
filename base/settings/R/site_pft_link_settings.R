#' Title site.pft.link.settings
#'
#' @param settings settings list
#'
#' @return pecan xml setting file
#' @export site.pft.link.settings
#'
#' @description This function reads in a pecan setting and check for the pft.site xml tag under run>inputs . If a path or a ID for the input is defined then, it will be used for linking sites with the pfts.
site.pft.link.settings <-function(settings){
  #lets see if there is the pft.site tag under run>inputs
  pft.site.info <- settings$run$inputs$pft.site
  # if it's not specified just let it go !
  if (is.null(pft.site.info)) return(settings)
  
  #if there is no input file/id defined or if both are defined at the same time. At the moment I'm gonna make sure that there is just one input.
  if (length(pft.site.info) !=1) {
    PEcAn.logger::logger.warn("In your xml tag for linking site with pfts, you either have no input specified or you have more than one input defined. No change was made !")
    return(settings)
  }
  
  if (!is.null(pft.site.info$path)) {
    #lets read in the Look Up Table
    LUT <- loadPath.sitePFT(settings,pft.site.info$path)
    # doing the real linkage and writing the setting down
    settings <- site.pft.linkage(settings, LUT)
  }
  

  return(settings)
}