#' build_X
#' 
#' @name build_X
#' @author Alexis Helgeson
#' 
#' @description builds X matrix for SDA
#'
#' @param new.params object created from sda_matchparam, passed from sda.enkf_MultiSite
#' @param nens number of ensemble members i.e. runs
#' @param read_restart_times passed from sda.enkf_MultiSite
#' @param settings settings object, passed from sda.enkf_MultiSite
#' @param outdir location of previous run output folder containing .nc files
#' @param out.configs object created for build_X passed from sda.enkf_MultiSite
#' @param t Default t=1, for function to work within time loop
#' @param var.names list of state variables taken from settings object
#' @param my.read_restart object that points to the model restart function i.e. read_restart.SIPNET
#'
#' @return X ready to be passed to SDA Analysis code
#' @export
#'
#' @examples
build_X <- function(out.configs, settings, new.params, nens, read_restart_times, outdir, t = 1, var.names, my.read_restart){

  if(t == 1){
    reads <-
      furrr::future_pmap(list(out.configs %>% `class<-`(c("list")), settings, new.params),function(configs,settings,siteparams) {
        # Loading the model package - this is required bc of the furrr
        #library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
        #source("~/pecan/models/sipnet/R/read_restart.SIPNET.R")
        
        X_tmp <- vector("list", 2)
        
        for (i in seq_len(nens)) {
          X_tmp[[i]] <- do.call( my.read_restart,
                                 args = list(
                                   outdir = outdir,
                                   runid = settings$runs$id[i] %>% as.character(),
                                   stop.time = read_restart_times[t+1],
                                   settings = settings,
                                   var.names = var.names,
                                   params = siteparams[[i]]
                                 )
          )
          
        }
        return(X_tmp)
      })
    
  }else{
    reads <-
      furrr::future_pmap(list(out.configs %>% `class<-`(c("list")), settings, new.params),function(configs,settings,siteparams) {
        # Loading the model package - this is required bc of the furrr
        #library(paste0("PEcAn.",settings$model$type), character.only = TRUE)
        #source("~/pecan/models/sipnet/R/read_restart.SIPNET.R")
        
        X_tmp <- vector("list", 2)
        
        for (i in seq_len(nens)) {
          X_tmp[[i]] <- do.call( my.read_restart,
                                 args = list(
                                   outdir = outdir,
                                   runid = configs$runs$id[i] %>% as.character(),
                                   stop.time = read_restart_times[t+1],
                                   settings = settings,
                                   var.names = var.names,
                                   params = siteparams[[i]]
                                 )
          )
          
        }
        return(X_tmp)
      })
  }
  
  #commented out text below describes future work - Alexis
  # #let's read the parameters of each site/ens
  # params.list <- reads %>% map(~.x %>% map("params"))
  # # Now let's read the state variables of site/ens
  # X <- reads %>% map(~.x %>% map_df(~.x[["X"]] %>% t %>% as.data.frame))
  # 
  # # Now we have a matrix that columns are state variables and rows are ensembles.
  # # this matrix looks like this
  # #         GWBI    AbvGrndWood   GWBI    AbvGrndWood
  # #[1,]  3.872521     37.2581  3.872521     37.2581
  # # But therer is an attribute called `Site` which tells yout what column is for what site id - check out attr (X,"Site")
  # if (multi.site.flag){
  #   X <- X %>%
  #     map_dfc(~.x) %>% 
  #     as.matrix() %>%
  #     `colnames<-`(c(rep(var.names, length(X)))) %>%
  #     `attr<-`('Site',c(rep(site.ids, each=length(var.names))))
  # }
    
  return(reads)
}