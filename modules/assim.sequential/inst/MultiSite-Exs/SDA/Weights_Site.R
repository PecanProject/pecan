   library(tidyverse)
   library(mvtnorm)
   library(nimble)
   library(scales)
   library(furrr)
   rm(list = ls())
   setwd("/projectnb/dietzelab/hamzed/SDA/ProductionRun/500Sites/Weights")
   load('FORECAST.RData')
   load('ANALYSIS.RData')
   plan(multisession)
   #------------------------------------------------------
   #Loading SDA outputs-----------------------------------
   #------------------------------------------------------
   args <- commandArgs(trailingOnly = TRUE)
   ind <- args[1] %>% as.numeric()
   if(is.na(ind))ind  <- 1
   print(ind)
   ind <- 1:33
   
   site.ids <- attr(FORECAST[[1]],'Site') %>% unique() 
   
   Weights.new <- pmap(list(ANALYSIS[ind],
                            FORECAST[ind],
                            names(FORECAST)[ind]),
                       function(ANALYSIS.r, FORECAST.r, Year.applid.weight) {
        
      
      site.ids %>%
         future_map_dfr(function(one.site){
           # browser() 
            site.ind <- which( attr(FORECAST[[1]],'Site') %in% one.site)
            #Read tobit outputs                   
            ind <- which( names(FORECAST) %in% Year.applid.weight)
            wr <- readRDS(paste0("RDS/W_",ind,".RDS"))
            # Mu.a, Pa
            mu.a <- apply(ANALYSIS.r[,site.ind],2 ,mean)
            Pa <- cov(ANALYSIS.r[,site.ind])
            #   browser()
            w <- emdbook::dmvnorm(FORECAST.r[,site.ind], mu.a, Pa, TRUE)
            
            data.frame(
               ens = 1:20,
               raw_weight=w,
               Site= one.site,
               Relative_weight=abs(w)/sum(abs(w)),
               Year=lubridate::year(Year.applid.weight)
            )
         }, .progress = TRUE)
 
   })
  
   #saveRDS(Weights.new, 'Weights.new.RDS')
   site.ids %>%
      walk(function(site.id.one){
         Weights.new %>%
            map_dfr(~.x) %>%
            filter(Site %in% site.id.one)%>%
            ggplot(aes(Year,Relative_weight))+
            geom_area(stat="identity", aes(fill=ens %>% as.factor()))+
            scale_fill_viridis_d(name="Ensemble", option = "A")+
            scale_x_continuous(breaks = seq(1986,2018,2))+
            theme_minimal(base_size = 15)
         
         ggsave(paste0("SiteL_Plots/Weights_",site.id.one,".png"), width = 10, height = 6)   
      })

   Weights.new %>%
      map_dfr(~.x) %>%
      saveRDS(file="site_level_shit.RDS")
   
   
   site_level_shit[ which(is.nan(site_level_shit$Relative_weight)), 4] <- 0.05
   
   saveRDS(site_level_shit, file="site_level_shit.RDS")
   
   
   
   site.ids %>%
      future_map(function(site.id.one){
         site_level_shit %>%
            filter(Site %in% site.id.one)%>%
            ggplot(aes(Year,Relative_weight))+
            geom_area(stat="identity", aes(fill=ens %>% as.factor()))+
            scale_fill_viridis_d(name="Ensemble", option = "A")+
            scale_x_continuous(breaks = seq(1986,2018,2))+
            labs(title=site.id.one)+
            theme_minimal(base_size = 15)
         
         ggsave(paste0("SiteL_Plots/Weights_",site.id.one,".png"), width = 10, height = 6)   
      })
   