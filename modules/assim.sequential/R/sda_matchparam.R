#' sda_matchparam
#'
#' @name sda_matchparam
#' @author Alexis Helgeson
#' 
#' @param settings settings object passed from sda.enkf_MultiSite
#' @param ensemble.samples taken from sample.Rdata object
#' @param site.ids character object passed from sda.enkf_MultiSite
#' @param nens number of ensemble members in model runs, taken from restart$runids
#'
#' @return new.params object used to 
sda_matchparam <- function(settings, ensemble.samples, site.ids, nens){
  #reformatting params
  new.params <- list()
  all.pft.names <- names(ensemble.samples)
  
  #loop over each site.
  for (i in seq_along(site.ids)) {
    #match pft name
    site.pft.name <- settings[[i]]$run$site$site.pft$pft.name
    if(is.null(site.pft.name)){
      site_pft = utils::read.csv(settings[[i]]$run$inputs$pft.site$path)
      site.pft.name = site_pft$pft[site_pft$site == settings[[i]]$run$site$id]
    }
    which.pft <- which(all.pft.names==site.pft.name)
    
    site.param <- list()
    site.samples <- ensemble.samples[which.pft]
    for (j in seq_len(nens)) {
      site.param[[j]] <- lapply(site.samples, function(x, n) {
        x[j, ] }, n = j)
    } 
    new.params[[i]] <- site.param
  }
  names(new.params) <- site.ids
  
  return(new.params)
}