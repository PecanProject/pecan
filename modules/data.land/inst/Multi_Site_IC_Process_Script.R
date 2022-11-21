#reading settings and setting up environments for later processes
settings <- read.settings("/projectnb/dietzelab/dongchen/All_NEON_SDA/NEON42/pecan.xml")
input_veg <- list(storedir='/projectnb/dietzelab/dongchen/test_IC/download')
outdir <- "/projectnb/dietzelab/dongchen/test_IC/test/"
start_date <- as.Date(settings$state.data.assimilation$start.date)
end_date <- Sys.Date()
source='NEON_veg'
machine_host <- "test-pecan.bu.edu"
n_ens <- settings$ensemble$size
log_file <- c()
neonsites <- neonstore::neon_sites(api = "https://data.neonscience.org/api/v0", .token = Sys.getenv("NEON_TOKEN"))
site.IDs <- settings %>% map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()
Drop <- TRUE
Write_into_settings <- TRUE

#loop over different sites
for (i in 1:length(settings)) {
  temp_settings <- settings[[i]]
  print(temp_settings$run$site$id)
  print(i)
  temp_outdir <- file.path(outdir,as.character(temp_settings$run$site$id))
  if(dir.exists(temp_outdir)){
    next
  }else{
    dir.create(temp_outdir)
  }
  
  #extract veg function
  veg_info <- try(extract_NEON_veg(lat = as.numeric(temp_settings$run$site$lat), 
                      lon = as.numeric(temp_settings$run$site$lon),
                      start_date = start_date,
                      end_date = end_date,
                      neonsites = neonsites,
                      store_dir = input_veg$storedir))
  #checks
  Var <- c("dryMass", "DBH", "SoilCarbon")
  Ind <- !is.na(veg_info)
  bin_var <- Var[Ind]
  
  #write into log file
  if(sum(Ind) != 3){
    log_file <- c(log_file, paste0("Site: ", temp_settings$run$site$id, ". No ", paste(Var[which(!(Var %in% bin_var))]), 
                                   " data found in NEON for this site! Herbacious downloaded!"))
  }else if(sum(Ind) == 0){
    log_file <- c(log_file, paste0("No data for site: ", temp_settings$run$site$id),". Jump to the next site.")
    unlink(temp_outdir, recursive = T)
    next
  }
  
  #check if we have the situation that only subplot ID 41 observed in one plot. if so, we might need to drop this record when Drop == TRUE.
  if(Ind[2]){
    for (PLOT in unique(veg_info[[2]]$plot)) {
      temp <- veg_info[[2]][which(veg_info[[2]]$plot==PLOT),]$Subplot
      if(length(unique(temp)) == 1){
        if(unique(temp) == 41){
          print(paste0("Only subplot 41 observed in plot: ", PLOT))
          log_file <- c(log_file, paste0("Only subplot 41 observed in plot: ", PLOT))
          if(Drop){
            veg_info[[2]] <- veg_info[[2]][-which(veg_info[[2]]$plot==PLOT),]
          }
        }
      }
    }
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
  paths <- c()
  for (j in 1:n_ens) {
    poolinfo <- PEcAn.data.land::cohort2pool(dat = readRDS(sample_ic_results$file[j]))
    out <- PEcAn.SIPNET::veg2model.SIPNET(temp_outdir, poolinfo, as.numeric(temp_settings$run$site$id), ens = j)
    paths <- c(out$file, paths)
  }
  
  if(Write_into_settings){
    #populated IC file paths into settings
    Create_mult_list <- function(list.names, paths){
      out <- as.list(paths)
      names(out) <- list.names
      out
    }
    settings[[i]]$run$inputs$poolinitcond$source <- "NEON_veg"
    settings[[i]]$run$inputs$poolinitcond$output <- "poolinitcond"
    settings[[i]]$run$inputs$poolinitcond$ensemble <- n_ens
    settings[[i]]$run$inputs$poolinitcond$path <- Create_mult_list(rep("path", n_ens), paths)
  }
}
if(Write_into_settings){
  write.settings(settings, outputdir = file.path(outdir), outputfile = "pecan.xml")
}

#write log file.
fileConn<-file(paste0(outdir, "/log.txt"))
writeLines(log_file, fileConn)
close(fileConn)