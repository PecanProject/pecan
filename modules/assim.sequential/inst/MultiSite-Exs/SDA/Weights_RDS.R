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
   
   Weights.new <- pmap(list(ANALYSIS[ind],
                            FORECAST[ind],
                            names(FORECAST)[ind]),
                       function(ANALYSIS.r, FORECAST.r, Year.applid.weight) {
      #Read tobit outputs                   
      ind <- which( names(FORECAST) %in% Year.applid.weight)
      wr <- readRDS(paste0("RDS/W_",ind,".RDS"))
      # Mu.a, Pa
      mu.a <- apply(ANALYSIS.r,2 ,mean)
      Pa <- cov(ANALYSIS.r)
      #   browser()
      w <- emdbook::dmvnorm(wr$x.original.new, mu.a, Pa, TRUE)
      
     data.frame(
       ens = 1:20,
       raw_weight=w,
       Relative_weight=abs(w)/sum(abs(w)),
       Year=lubridate::year(Year.applid.weight)
       ) 
   })
  
   #saveRDS(Weights.new, 'Weights.new.RDS')

   Weights.new %>%
     map_dfr(~.x) %>%
   #  filter(!(Year%in%c(2013,2014)))%>%
     ggplot(aes(Year,Relative_weight))+
     geom_area(stat="identity", aes(fill=ens %>% as.factor()))+
     scale_fill_viridis_d(name="Ensemble", option = "A")+
     scale_x_continuous(breaks = seq(1986,2018,2))+
     theme_minimal(base_size = 15)
   
ggsave("Weights2.png", width = 10, height = 6)   
