#test 39 NEON sites
#
settings <- read.settings("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
input_veg <- list(storedir='/projectnb/dietzelab/dongchen/test_IC/download')
# start_date <- as.Date("2016-01-01")
# end_date <- as.Date("2016-12-31")
start_date <- as.Date(settings$state.data.assimilation$start.date)
end_date <- Sys.Date()

source='NEON_veg'
machine_host <- "test-pecan.bu.edu"
n_ens <- 100
log_file <- c()
neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
site.IDs <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

for (i in 1:length(settings)) {
  temp_settings <- settings[[i]]
  print(temp_settings$run$site$id)
  print(i)
  temp_outdir <- paste0("/projectnb/dietzelab/dongchen/test_IC/HF/",as.character(temp_settings$run$site$id))
  if(dir.exists(temp_outdir)){
    next
  }else{
    dir.create(temp_outdir)
  }
  
  #before doing extract NEON function, we also need to identify when do we have data?
  
  
  #extract veg function
  veg_info <- try(extract_NEON_veg(lat = as.numeric(temp_settings$run$site$lat), 
                      lon = as.numeric(temp_settings$run$site$lon),
                      start_date = start_date,
                      end_date = end_date,
                      neonsites = neonsites,
                      store_dir = input_veg$storedir))
  #checks
  if(is.na(veg_info[[2]]) && !is.na(veg_info[[1]])){
    print(paste0("No veg stucture data found in NEON for site: ", temp_settings$run$site$id, ".  Herbacious downloaded!"))
    log_file <- c(log_file, paste0("Site: ", temp_settings$run$site$id, ". No veg stucture data found in NEON for this site! Herbacious downloaded!"))
    bin_var <- "dryMass"
    veg_ind <- 1
  }else if(sum(!is.na(veg_info[[2]]) && !is.na(veg_info[[1]]))){
    bin_var <- c("DBH", "dryMass")
    veg_ind <- c(1, 2)
  }else{
    print(paste0("No veg stucture nor herbacious for site: ", temp_settings$run$site$id))
    log_file <- c(log_file, paste0("No veg stucture nor herbacious for site: ", temp_settings$run$site$id))
    unlink(temp_outdir, recursive = T)
    next
  }
  
  #grab first measurements of each Plot.
  Grab_First_Measurements_of_Each_Plot <- function(temp_data){
    Plot_Year <- paste0(temp_data$plot, temp_data$year)
    unique_Year <- sort(unique(temp_data$year))
    unique_Plot <- sort(unique(temp_data$plot))
    Ind <- rep(NA, length(Plot_Year))
    
    for (j in 1:length(unique_Plot)) {
      for (k in 1:length(unique_Year)) {
        if(length(which(Plot_Year == paste0(unique_Plot[j], unique_Year[k])))>0){
          Ind[which(Plot_Year == paste0(unique_Plot[j], unique_Year[k]))] <- 1
          break
        }
      }
    }
    temp_data <- cbind(temp_data, Ind)
    if(sum(is.na(temp_data$Ind))==0){
      temp_data <- temp_data
    }else{
      temp_data <- temp_data[-which(is.na(temp_data$Ind)),]
    }
    temp_data
  }
  for (j in veg_ind) {
    temp_data <- veg_info[[j]]
    temp_data <- Grab_First_Measurements_of_Each_Plot(temp_data)
    veg_info[[j]] <- temp_data
  }
  
  
  sppfilename <- write_veg(temp_outdir, start_date, veg_info = veg_info, source)
  
  #sample_ic function
  sample_ic_results <- PEcAn.data.land::sample_ic(in.path = gsub(basename(sppfilename), "", sppfilename), 
                                                  in.name = basename(sppfilename), 
                                                  start_date = start_date, 
                                                  end_date = end_date, 
                                                  outfolder = temp_outdir, 
                                                  n.ensemble = n_ens, 
                                                  machine_host = machine_host, 
                                                  bin_var = bin_var,
                                                  source = source)
  # Calculate poolinfo and Write into NC file
  for (j in 1:n_ens) {
    poolinfo <- PEcAn.data.land::cohort2pool(dat = readRDS(sample_ic_results$file[j]))
    out <- PEcAn.SIPNET::veg2model.SIPNET(temp_outdir, poolinfo, as.numeric(temp_settings$run$site$id), ens = j)
  }
}
fileConn<-file(paste0("/projectnb/dietzelab/dongchen/test_IC/extract_veg/", "/log.txt"))
writeLines(log_file, fileConn)
close(fileConn)









#validation
fullpool <- list()
load("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/obs_2012_2021/obs_mean.Rdata")
for (i in 1:length(settings)) {
  ID <- settings[[i]]$run$site$id
  files <- list.files(paste0("/projectnb/dietzelab/dongchen/test_IC/HF/",ID), pattern = "*.nc", full.names = T)
  if(length(files)==0){
    next
  }
  obs <- obs.mean[[4]][as.character(ID)]
  ens_pool <- c()
  for (j in 1:length(files)) {
    pool <- PEcAn.data.land::prepare_pools(files[j])
    ens_pool <- c(ens_pool, pool$wood)
  }
  
  fullpool[[i]] <- list(pool_max=max(ens_pool)*10, pool_min=min(ens_pool)*10, obs=obs[[1]]$AbvGrndWood, ID=ID)
  
}
fullpool[sapply(fullpool, is.null)] <- NULL

obs <- c()
pool_max <- c()
pool_min <- c()
pool_mean <- c()
for (i in 1:39) {
  obs <- c(obs, fullpool[[i]]$obs)
  pool_max <- c(pool_max, fullpool[[i]]$pool_max)
  pool_min <- c(pool_min, fullpool[[i]]$pool_min)
  pool_mean <- c(pool_mean, (pool_max[i] + pool_min[i])/2)
}
library(scales)
plot(obs, pool_mean, pch=18, col="black", cex=1.5, xlab="AGB Obs (Mg/ha)", ylab="AGB IC (Mg/ha)")
abline(0,1, lwd=5, col = alpha("red", 0.4))
abline(LM, lwd=5, col = alpha("green", 0.4))
legend(0, 400, legend = c("1:1 Line", "Fitted Line"), lwd=rep(5,2), col=c(alpha("red", 0.4), alpha("green", 0.4)), lty = rep(1,2))

#do linear regression
LM_data <- as.data.frame(cbind(obs, pool_mean))
LM <- lm(obs ~ pool_mean, data = LM_data)
summary(LM)
# All_pre <- cbind(obs, pool_min, pool_max)
# lwd <- 1
# L <- 0.05
# i=1
# plot(rep(All_pre[i, 1], 2), c(All_pre[i, 3], All_pre[i, 2]), type="l", lwd = lwd, col="blue", ylim=c(0, 250), xlim=c(0,250), xlab="AGB Observation", ylab="AGB IC")
# lines(c(All_pre[i, 1]-L, All_pre[i, 1]+L), rep(All_pre[i, 2], 2), col="blue")
# lines(c(All_pre[1, 1]-L, All_pre[1, 1]+L), rep(All_pre[i, 4], 2), col="blue")
# points(All_pre[i, 1], All_pre[i, 3], pch=18, col="green")
# abline(a=0, b=1, col="red", lwd=2)
# for (i in 1:33) {
#   lines(rep(All_pre[i, 1], 2), c(All_pre[i, 4], All_pre[i, 2]), lwd = lwd, col="blue")
#   lines(c(All_pre[i, 1]-L, All_pre[i, 1]+L), rep(All_pre[i, 2], 2), col="blue")
#   lines(c(All_pre[i, 1]-L, All_pre[i, 1]+L), rep(All_pre[i, 4], 2), col="blue")
#   points(All_pre[i, 1], All_pre[i, 3], pch=18, col="green")
# }



#debug into ic_sample
in.path = gsub(basename(sppfilename), "", sppfilename) 
in.name = basename(sppfilename)
outfolder = temp_outdir 
n.ensemble = n_ens 
bin_var = c("DBH", "dryMass")
source = source













