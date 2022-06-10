#' build_X
#' 
#' @name build_X
#' @author Alexis Helgeson
#' 
#' @description builds X matrix for SDA
#'
#' @param configs 
#' @param settings 
#' @param siteparams 
#'
#' @return
#' @export
#'
#' @examples
build_X <- function(configs, settings, siteparams){
  
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
      
      
      
      
      FORECAST[[obs.t]] <- X
}
