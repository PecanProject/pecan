#' build_X
#' 
#' @name build_X
#' @author Alexis Helgeson
#' 
#' @description builds X matrix for SDA
#'
#' @param runid vector contain runids from model runs to assimilate, taken from restart object
#' @param new.params object created from sda_matchparam, passed from sda.enkf_MultiSite
#' @param nens number of ensemble members i.e. runs
#' @param read_restart_times passed from sda.enkf_MultiSite
#' @param settings settings object, passed from sda.enkf_MultiSite
#' @param sim.time passed from sda.enkf_MultiSite
#' @param outdir location of previous run output folder containing .nc files
#'
#' @return X ready to be passed to SDA Analysis code
#' 
#'
#' @examples
build_X <- function(runid, settings, new.params, nens, read_restart_times, sim.time, outdir){
  
  for(t in sim.time){
    X_tmp <- vector("list", 2)
    
    for (i in seq_len(nens)) {
      X_tmp[[i]] <- do.call(my.read_restart,
                             args = list(
                               outdir = old.dir,
                               runid = runid[i] %>% as.character(),
                               stop.time = read_restart_times[t+1],
                               settings = settings,
                               var.names = var.names,
                               params = new.params[[i]]
                             )
      )
      
    }
    return(X_tmp)
    #let's read the parameters of each site/ens
    params.list <- reads %>% map(~.x %>% map("params"))
    # Now let's read the state variables of site/ens
    #don't need to build X when t=1
    X <- reads %>% map(~.x %>% map_df(~.x[["X"]] %>% t %>% as.data.frame))
    
    
    #replacing crazy outliers before it's too late
    if (control$OutlierDetection) X <- outlier.detector.boxplot(X)
    
    # Now we have a matrix that columns are state variables and rows are ensembles.
    # this matrix looks like this
    #         GWBI    AbvGrndWood   GWBI    AbvGrndWood
    #[1,]  3.872521     37.2581  3.872521     37.2581
    # But therer is an attribute called `Site` which tells yout what column is for what site id - check out attr (X,"Site")
    if (multi.site.flag)
      X <- X %>%
      map_dfc(~.x) %>% 
      as.matrix() %>%
      `colnames<-`(c(rep(var.names, length(X)))) %>%
      `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
  }
  
      return(X)
}
