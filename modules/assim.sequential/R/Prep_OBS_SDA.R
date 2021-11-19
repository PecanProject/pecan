Prep_OBS_SDA <- function(multi.settings, start_year, end_year, out_dir, AGB_dir = "/projectnb/dietzelab/hamzed/LandTrendr/LandTrendr_AGB_data/"){
  ####working on downloading LAI and extraction AGB
  settings <- multi.settings #getting it simpler
  
  #getting site ID
  observations <- c()
  for (i in 1:length(settings)) {
    obs <- settings[[i]]$run$site$id
    observations <- c(observations,obs)
  }
  
  ##site info query site info
  obs <- observations
  dbparms = list()
  dbparms$dbname = "bety"
  dbparms$host = "128.197.168.114"
  dbparms$user = "bety"
  dbparms$password = "bety"
  
  #Connection code copied and pasted from met.process
  bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                              host     = dbparms$host, 
                              user     = dbparms$user, 
                              password = dbparms$password)
  con <- bety$con

  source("call_MODIS.R")
  
  #convert year to YEARDOY
  start_YEARDOY <- paste0(as.character(start_year), "001")
  end_YEARDOY <- paste0(as.character(end_year), "365")
  
  #assigning ncores
  if(length(obs) <= 10){
    ncores <- length(obs)
  }else{ncores <- 10}
  
  #parallel
  round.length <- ceiling(length(obs)/ncores)
  
  #initialize lai and lai.sd
  lai_data <- c()
  sd <- c()
  
  for (i in 1:round.length) {
    #prepare for parallel
    start = (1+((i-1)*ncores))
    end = start+ncores-1
    obs = observations[start:end]
    
    #grab site info
    site_ID <- obs
    suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                                ids = site_ID, .con = con))
    suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
    suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
    site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                      lon=qry_results$lon, time_zone=qry_results$time_zone)
    
    #download LAI
    lai = PEcAn.data.remote::call_MODIS(outdir = NULL, var = "LAI", site_info = site_info, product_dates = c(start_YEARDOY, end_YEARDOY),
                     run_parallel = TRUE, ncores = ncores, product = "MOD15A2H", band = "Lai_500m",
                     package_method = "MODISTools", QC_filter = TRUE, progress = FALSE)
    lai_data <- rbind(lai_data, lai)
    
    sd = rbind(PEcAn.data.remote::call_MODIS(outdir = NULL, var = "LAI", site_info = site_info, product_dates = c(start_YEARDOY, end_YEARDOY),
                          run_parallel = TRUE, ncores = ncores, product = "MOD15A2H", band = "LaiStdDev_500m",
                          package_method = "MODISTools", QC_filter = TRUE, progress = FALSE), sd)
    
  }
  lai_sd <- sd
  
  #extracting AGB data
  work_dir <- getwd()
  med_agb_data <- extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean", AGB_dir, product_dates=start_year:end_year)[[1]]
  sdev_agb_data <- extract.LandTrendr.AGB(site_info, "stdv", buffer = NULL, fun = "mean", AGB_dir, product_dates=start_year:end_year)[[1]]
  
  #formatting AGB data
  ndates = colnames(med_agb_data)[-c(1:2)] # getting dates
  med_agb_data$Site_Name = as.character(med_agb_data$Site_Name, stringsAsFactors = FALSE)
  med_agb_data = reshape2::melt(med_agb_data, id.vars = "Site_ID", measure.vars = colnames(med_agb_data)[-c(1:2)])
  
  sdev_agb_data$Site_Name = as.character(sdev_agb_data$Site_Name, stringsAsFactors = FALSE)
  sdev_agb_data = reshape2::melt(sdev_agb_data, id.vars = "Site_ID", measure.vars = colnames(sdev_agb_data)[-c(1:2)])
  
  agb_data = as.data.frame(cbind(med_agb_data, sdev_agb_data$value))
  names(agb_data) = c("Site_ID", "Date", "Median", "SD")
  agb_data$Date = as.character(agb_data$Date, stringsAsFactors = FALSE)
  
  #format LAI data
  names(lai_sd) = c("modis_date", "calendar_date", "band", "tile", "site_id", "lat", "lon", "pixels", "sd", "qc")
  output = cbind(lai_data, lai_sd$sd)
  names(output) = c(names(lai_data), "sd")
  save(output, file = paste0(out_dir,"/all_lai_data.Rdata"))#export all LAI data
  output = output[,c(5, 2, 9, 11)]
  colnames(output) = c("Site_ID", "Date", "Median", "SD")
  
  #compute peak LAI per year
  data = output
  peak_lai = data.frame()
  years = unique(year(as.Date(data$Date, "%Y-%m-%d")))
  for (i in seq_along(years))
  {
    d = data[grep(data$Date, pattern = years[i]),]
    sites = unique(d$Site_ID)
    for (j in seq_along(sites))
    {
      index = which(d$Site_ID == site_info$site_id[j])
      site = d[index,]
      if (length(index) > 0)
      {
        # peak lai is the max value that is the value <95th quantile to remove potential outlier values
        max = site[which(site$Median == max(site$Median[which(site$Median <= quantile(site$Median, probs = 0.95))], na.rm = T))[1],] #which(d$Median == max(d$Median[index], na.rm = T))[1]
        peak = data.frame(max$Site_ID, Date = paste("Year", years[i], sep = "_"), Median = max$Median, SD = max$SD)
        peak_lai = rbind(peak_lai, peak)
        
      }
    }
  }
  peak_lai$SD[peak_lai$SD < 0.66] = 0.66
  names(peak_lai) = c("Site_ID", "Date", "Median", "SD")
  save(peak_lai, file = paste0(out_dir,"/peak_lai_data.Rdata"))
  
  #making obs.mean and obs.cov
  peak_lai$Site_ID = as.numeric(as.character(peak_lai$Site_ID, stringsAsFactors = F))
  peak_lai$Date = as.character(peak_lai$Date, stringsAsFactors = F)
  observed_vars = c("AbvGrndWood", "LAI")
  observed_data = merge(agb_data, peak_lai, by = c("Site_ID", "Date"), all = T)
  names(observed_data) = c("Site_ID", "Date", "med_agb", "sdev_agb", "med_lai", "sdev_lai")
  
  observed_data = observed_data[order(observed_data$Date),]
  dates = sort(unique(observed_data$Date))
  
  # create the obs.mean list --> this needs to be adjusted to work with load.data in the future (via hackathon)
  obs.mean = data.frame(date = observed_data$Date, site_id = observed_data$Site_ID, med_agb = observed_data$med_agb, med_lai = observed_data$med_lai)
  obs.mean$date = as.character(obs.mean$date, stringsAsFactors = FALSE)
  
  obs.mean = obs.mean %>%
    split(.$date)
  
  # change the dates to be middle of the year
  date.obs <- strsplit(names(obs.mean), "_") %>%
    map_chr(~.x[2]) %>% paste0(.,"/07/15")
  
  obs.mean = names(obs.mean) %>%
    map(function(namesl){
      obs.mean[[namesl]] %>%
        split(.$site_id) %>%
        map(~.x[3:4] %>% setNames(c("AbvGrndWood", "LAI")) %>% `row.names<-`(NULL))
      #setNames(site.ids)
    }) %>% setNames(date.obs)
  
  #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  names = date.obs
  for (name in names)
  {
    for (site in names(obs.mean[[name]]))
    {
      na_index = which(!(is.na(obs.mean[[ name]][[site]])))
      colnames = names(obs.mean[[name]][[site]])
      if (length(na_index) > 0)
      {
        obs.mean[[name]][[site]] = obs.mean[[name]][[site]][na_index]
      }
    }
  }
  
  # fillers are 0's for the covariance matrix. This will need to change for differing size matrixes when more variables are added in.
  # filler_0 = as.data.frame(matrix(0, ncol = length(observed_vars), nrow = nrow(observed_data)))
  # names(filler_0) = paste0("h", seq_len(length(observed_vars)))
  
  # create obs.cov dataframe -->list by date
  obs.cov = data.frame(date = observed_data$Date, site_id = observed_data$Site_ID, sdev_agb = observed_data$sdev_agb, sdev_lai = observed_data$sdev_lai)#, filler_0)
  obs.cov$date = as.character(obs.cov$date, stringsAsFactors = F)
  
  obs.cov = obs.cov %>%
    split(.$date)
  
  obs.cov = names(obs.cov) %>%
    map(function(namesl){
      obs.cov[[namesl]] %>%
        split(.$site_id) %>%
        map(~.x[3:4]^2 %>% unlist %>% diag(nrow = 2, ncol = 2) ) 
    }) %>% setNames(date.obs)
  
  
  names = date.obs
  for (name in names)
  {
    for (site in names(obs.cov[[name]]))
    {
      bad = which(apply(obs.cov[[name]][[site]], 2, function(x) any(is.na(x))) == TRUE)
      if (length(bad) > 0)
      {
        obs.cov[[name]][[site]] = obs.cov[[name]][[site]][,-bad]
        if (is.null(dim(obs.cov[[name]][[site]])))
        {
          obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad]
        } else {
          obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad,]
        }
      }
    }
  }
  save(obs.mean, file = paste0(out_dir, '/obs_mean.Rdata'))
  save(obs.cov, file = paste0(out_dir, '/obs_cov.Rdata'))
  list(cov=obs.cov, mean=obs.mean)
}