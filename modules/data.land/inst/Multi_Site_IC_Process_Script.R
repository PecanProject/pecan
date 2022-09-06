#reading settings and setting up environments for later processes
settings <- read.settings("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
input_veg <- list(storedir='/projectnb/dietzelab/dongchen/test_IC/download')
outdir <- "/projectnb/dietzelab/dongchen/test_IC/HF/"
start_date <- as.Date(settings$state.data.assimilation$start.date)
end_date <- Sys.Date()
source='NEON_veg'
machine_host <- "test-pecan.bu.edu"
n_ens <- 100
log_file <- c()
neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
site.IDs <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

#loop over different sites
for (i in 1:length(settings)) {
  temp_settings <- settings[[i]]
  print(temp_settings$run$site$id)
  print(i)
  temp_outdir <- paste0(outdir,as.character(temp_settings$run$site$id))
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
fileConn<-file(paste0(outdir, "/log.txt"))
writeLines(log_file, fileConn)
close(fileConn)