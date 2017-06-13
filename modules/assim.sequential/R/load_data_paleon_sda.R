library(plyr)
library(corrplot)
library(magic)

#LINKAGES
setwd('/fs/data2/output//PEcAn_1000003314/')
#SIPNET
#setwd('/fs/data2/output//PEcAn_1000003356')

#---------------- Load PEcAn settings file. -------------------------------------------------------#
# Open and read in settings file for PEcAn run.
settings <- read.settings("pecan.SDA.xml") 


d <- settings$database$bety[c("dbname", "password", "host", "user")]
bety <- src_postgres(host = d$host, user = d$user, password = d$password, dbname = d$dbname)
settings$host$name <- "localhost"

site <- PEcAn.DB::query.site(settings$run$site$id, bety$con)
format_id <- settings$state.data.assimilation$data$format_id

input.list <- list()
input.id <- list()
obvs <- list()

var.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                           function(x) {
                             x$variable.name
                           }, 
                           USE.NAMES = FALSE), 
                    use.names = FALSE)

start_date <- settings$state.data.assimilation$start.date
end_date   <- settings$state.data.assimilation$end.date

obs.mean <- list()
obs.cov <- list()

start.time <- format(ymd(start_date),settings$state.data.assimilation$forecast.time.step)
end.time <- format(ymd(end_date),settings$state.data.assimilation$forecast.time.step)
obs.times <- start.time:end.time


for(i in seq_along(format_id)){
  input.list[[i]] <- db.query(paste("SELECT * FROM inputs WHERE site_id =",site$id ,"  AND format_id = ",format_id[[i]]), bety$con)
  input.id[[i]] <- input.list[[i]]$id
  
  data.path <- PEcAn.DB::query.file.path(input.id[[i]], settings$host$name, bety$con)
  format_full <- format <- PEcAn.DB::query.format.vars(input.id = input.id[[i]], bety, format.id = NA, var.ids=NA)
  
  format$na.strings <- 'NA'
  
  # ---- LOAD INPUT DATA ---- #
  time.row <- format$time.row
  vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
  
  obvs[[i]] <- load_data(data.path, format, start_year, end_year, site, vars.used.index, time.row)
  
  ### Only use RW + Census model for tree ring data product
  if(format_id[[i]] == '1000000040'){
    obvs[[i]] <- obvs[[i]][obvs[[i]]$model_type=='Model RW + Census',]
    obvs[[i]]$AbvGrndWood <- obvs[[i]]$AbvGrndWood * .48
    obvs[[i]]$NPP_1 <- obvs[[i]]$NPP_1 * .48
  }
  
  dataset <- obvs[[i]]
  variable <- var.names
  
  # Map species to model specific PFTs
  if(any(var.names == 'AGB.pft')){
    variable <- sub('AGB.pft','species_id',variable)
    spp_id <- match_species_id(unique(dataset$species_id),format_name = 'usda',bety)
    pft_mat <- match_pft(spp_id$bety_species_id,settings$pfts,con = bety$con)
    
    mutate(dataset, pft.cat = )
  } 
  
  arguments <- list(.(year, iter, site_id), .(variable))
  arguments2 <- list(.(year), .(variable))
  
  melt_id <- colnames(dataset)[-which(colnames(dataset) %in% variable)]
  melt.test <- melt(dataset, id = melt_id, na.rm = TRUE)
  cast.test <- dcast(melt.test, arguments, sum, margins = variable)
  
  melt_id <- colnames(cast.test)[-which(colnames(cast.test) %in% variable)]
  melt.next <- melt(cast.test, id = melt_id)
  mean_mat <- dcast(melt.next, arguments2, mean)
  
  iter_mat <- acast(melt.next, iter ~ variable ~ year, mean)
  cov.test <- apply(iter_mat,3,function(x){cov(x)})
  
  
  summarize_data <- function(dataset, arguments, variable){
    
    agb_sum <- ddply(dataset, arguments, summarize, 
                     sum_agb = sum(variable, na.rm = TRUE)) #sum across trees
    
    group <- group_by(dataset, MCMC_iteration, year, site_id)
    test <- summarise(group,arguments, sum_test = sum(AbvGrndWood, na.rm = TRUE))
    
    agb_mean <- dlply(agb_sum,c('year'), summarize, 
                      AbvGrndWood = mean(sum_agb, na.rm = TRUE)) #mean across plots and interations
    names(agb_mean) <- sapply(agb_mean, names)
    agb_mean <- lapply(agb_mean, unname)
    
    agb_sd <- dlply(agb_sum,c('year'), summarize, 
                    SD.AbvGrndWood = sd(sum_agb, na.rm = TRUE)) #sd across interations
    names(agb_sd) <- sapply(agb_sd, names)
    agb_sd <- lapply(agb_sd, unname)
    
    
    agb_sum <- ddply(dataset, c('taxon','iter','year','site_id'), summarize, 
                     sum_agb = sum(ab, na.rm = TRUE)) #sum across trees
    ab_sum_2 <- ddply(agb_sum,c('iter','taxon','year'), summarize, 
                      mean_site_ab = mean(sum_agb, na.rm = TRUE)) #mean by plots
    ab_mean <- ddply(ab_sum_2,c('taxon','year'), summarize, 
                     mean_ab = mean(mean_site_ab, na.rm = TRUE)) #mean across iters
    ab_mat <- daply(ab_mean,c('taxon','year'),function(x) x$mean_ab)
    
    iter.sum.mat <- daply(ab_sum_2,c('taxon','year','iter'),function(x) x$mean_site_ab)
    
    cov_array<-array(NA,dim=c(15,15,56))
    for(i in 1:55){
      cov_array[,,i] <- cov(t(iter.sum.mat[,i,]))
    }
    
    row.keep <- list()
    spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params #this doesn't work unless linkages is in my home directory
    
    for(i in 1:15){
      row.keep[[i]]<-grep(rownames(ab_mat)[i],spp.params.default[,2])[1]
    }
    
    dist.mat<-spp.params.default[unlist(row.keep),]
    dist.mat<-dist.mat[-9,]
    rownames(dist.mat)<-dist.mat[,1]
    dist(dist.mat,method="canberra")
    
    new.names <- spp.params.default[unlist(row.keep),1]
    new.names[2] <- spp.params.default[18,1]
    rm.spp <- which(is.na(new.names))
    new.names<-new.names[-rm.spp]
    
    ab_mat<-ab_mat[-rm.spp,]
    
    rownames(ab_mat)<- paste0("AGB.pft.",new.names)
    obs.mean <- list()
    for(i in 1:ncol(ab_mat)){
      obs.mean[[i]] <- ab_mat[,i]
    }
    
    cov_array<-cov_array[-rm.spp,-rm.spp,]
    colnames(cov_array)<-new.names
    rownames(cov_array)<-new.names
    obs.cov <- list()
    for(i in 1:dim(cov_array)[3]){
      obs.cov[[i]] <- cov_array[,,i]
    }
    years<-1960:2014
    names(obs.mean) <- paste0(years,'/12/31')
    return(obs.mean = obs.mean, obs.cov = obs.cov)
  }
  
  if(any(var.names == 'AGB') & any(format$vars$bety_name == 'AbvGrndWood')){
    agb_sum <- ddply(obvs[[i]],c('MCMC_iteration','year','site_id'), summarize, 
                     sum_agb = sum(AbvGrndWood, na.rm = TRUE)) #sum across taxon
    agb_mean <- dlply(agb_sum,c('year'), summarize, 
                      AbvGrndWood = mean(sum_agb, na.rm = TRUE)) #mean across plots and interations
    names(agb_mean) <- sapply(agb_mean, names)
    agb_mean <- lapply(agb_mean, unname)
    
    agb_sd <- dlply(agb_sum,c('year'), summarize, 
                    SD.AbvGrndWood = sd(sum_agb, na.rm = TRUE)) #sd across interations
    names(agb_sd) <- sapply(agb_sd, names)
    agb_sd <- lapply(agb_sd, unname)
    
    if(!is.null(obs.mean)){
      obs.mean <- agb_mean
      obs.cov <- agb_sd
    }else{
      for(t in seq_len(length(obs.mean))){
        obs.mean[[t]] <- c(obs.mean[[t]],unlist(agb_mean[t]))
        obs.cov[[t]] <- adiag(obs.cov[[t]],unlist(agb_sd[t]))
      }
    }
  }
  
  if(any(var.names == 'AGB.pft') & any(format$vars$bety_name == 'AbvGrndWood')){
    
  }
}

#####
##### TOTAL AGB OUTPUT #####
#####

posts_ab$AbvGrndWood <- posts_ab$AbvGrndWood * .48 #convert from kg to kgC

agb_sum <- ddply(posts_ab,c('MCMC_iteration','year','site_id'), summarize, 
                 sum_agb = sum(AbvGrndWood, na.rm = TRUE)) #sum across taxon
agb_mean <- ddply(agb_sum,c('year'), summarize, 
                  mean_agb = mean(sum_agb, na.rm = TRUE)) #mean across plots and interations
agb_sd <- ddply(agb_sum,c('year'), summarize, 
                sd_agb = sd(sum_agb, na.rm = TRUE)) #sd across interations
total_biomass <- cbind(agb_mean,agb_sd$sd_agb)
colnames(total_biomass)[2] <- "mean_agb ('kgC/m^2')"


time_get <- function(time_point, x, x_time){
  mat_keep <- x[which(x_time == time_point),]
  return(mat_keep)
}

for(t in seq_along(obs.times)){
  
  for(i in seq_along(obvs)){
    ### Take out year t's data to make overall mean and cov for time t
    obvs_sep[[i]] <- time_get(time_point = obs.times[[t]],x = obvs[[i]], x_time = obvs[[i]]$year) # year is not general...
  }
  
  
  
  
  
  #need to apply over all data in obvs data list
  obvs[[1]][which(obvs[[1]]$year == obs.times[[t]]),]
  
  obs.mean[[t]] <- 
}


names(obs.mean) <- paste0(years,'/12/31')



### Now you have all the data and need to synthesize it.

### lookup table 



##' @title load_data_paleon_sda
##' @name  load_data_paleon_sda
##' @author Ann Raiho \email{araiho@nd.edu}
##' 
##' @param settings    PEcAn settings object
##' 
##' @description Load data function for paleon SDA data products
##' 
##' @return obs.mean and obs.cov for sda.enkf
##' @export
##' 
load_data_paleon_sda <- function(settings){
  
  #### Types of Data needed
  ## STEPPS Fcomp
  ## ReFAB biomass
  ## HF Tree Rings
  ## Tree Rings from tree ring module
  ## PLS and FIA Biomass Snapshot
  ## Wish list: eddy covariance
  
  ### TO DO
  #3. Make time variable
  #4. Insert them into obs.mean and obs.cov
  
  model      <- settings$model$type
  start_date <- settings$state.data.assimilation$start.date
  end_date   <- settings$state.data.assimilation$end.date
  site_id <- settings$run$site$id
  data_type  <- settings$state.data.assimilation$data$type
  data_variable  <- settings$state.data.assimilation$data$variable
  
  data.dir <- c('/fs/data2/output/paleon_data_products')
  site_path <- file.path(data.dir,site_id) #path to site folder
  variable_path <- file.path(site_path,list.files(site_path)[grep(data_type,list.files(site_path))])
  
  data_index <- sapply(seq_along(unlist(data_variable)), function(x) {
    grep(unlist(data_variable)[x],list.files(variable_path))})
  
  if(grep('AGB',settings$state.data.assimilation$data$variable)){
    gsub(".pft","" ,"AGB.pft")
  }
  
  data_path <- file.path(variable_path,list.files(variable_path)[data_index])
  
  data <- list()
  
  for(i in seq_along(data_path)){
    data[[i]] <- readRDS(data_path[i])
  }
  
  names(data) <- settings$state.data.assimilation$data$variable
  
  if(names(data)=='AGB.pft'){
    
  }
  
  if(names(data)=='NPP'){
    
  }
  
  load("~/Raiho_extras/linkages_lyford_summary_v7.Rdata")
  
  load("~/Raiho_extras/sipnet_lyford_summary_v7.Rdata")
  years<-1961:2015
  names(obs.mean) <- paste0(years,'/12/31')
  names(obs.cov) <- paste0(years,'/12/31')
  
  
}
