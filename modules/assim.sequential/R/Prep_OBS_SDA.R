#' SDA observation preparation function for LAI and AGB
#'
#' @param settings multi.settings objects that contains multiple sites info
#' @param out_dir  output dir
#' @param AGB_dir  AGB data dir
#' @param Search_Window search window for locate available LAI values
#'
#' @return mean and covariance of observations
#'
#' @importFrom magrittr %>%
#' @export
#'
Prep_OBS_SDA <- function(settings, out_dir, AGB_dir, Search_Window=30){
  ####working on downloading LAI and extraction AGB
  
  #getting site ID
  observations <- c()
  for (i in 1:length(settings)) {
    observations <- c(observations,settings[[i]]$run$site$id)
  }
  
  #query site info
  bety <- dplyr::src_postgres(dbname   = settings$database$bety$dbname,
                              host     = settings$database$bety$host,
                              user     = settings$database$bety$user,
                              password = settings$database$bety$password)
  con <- bety$con
  
  #grab site info
  site_ID <- observations
  suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                              ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                              ids = site_ID, .con = con))
  suppressWarnings(qry_results <- PEcAn.DB::db.query(site_qry, con))
  site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                    lon=qry_results$lon, time_zone=qry_results$time_zone)
  
  #convert year to YEARDOY
  #setting up start and end date based on settings specs
  start_date <- as.Date(settings$state.data.assimilation$start.date, tz="UTC")
  end_date <- as.Date(settings$state.data.assimilation$end.date, tz="UTC")
  
  #converting from date to YEAR-DOY(example: 2012-01-01 to 2012001)
  start_YEARDOY <- paste0(lubridate::year(start_date),sprintf("%03d",lubridate::yday(start_date)))
  end_YEARDOY <- paste0(lubridate::year(end_date),sprintf("%03d",lubridate::yday(end_date)))
  
  #assigning ncores
  if(length(observations) <= 10){
    ncores <- length(observations)
  }else{ncores <- 10}
  
  #parallelly download LAI and LAI std bands
  round.length <- ceiling(length(observations)/ncores)
  
  #initialize lai and lai.sd
  lai_data <- c()
  lai_sd <- c()
  
  for (i in 1:round.length) {
    #prepare for parallel
    start = (1+((i-1)*ncores))
    end = start+ncores-1
    
    #grab temp site_info for current loop
    temp_site_info <- furrr::future_pmap(list(site_info, start, end), function(X, start, end){X[start:end]})
    
    #download LAI data and LAI std
    lai_data <- rbind(PEcAn.data.remote::call_MODIS(outdir = NULL, var = "LAI", site_info = site_info, product_dates = c(start_YEARDOY, end_YEARDOY),
                     run_parallel = TRUE, ncores = ncores, product = "MOD15A2H", band = "Lai_500m",
                     package_method = "MODISTools", QC_filter = TRUE, progress = FALSE), lai_data)
    
    lai_sd = rbind(PEcAn.data.remote::call_MODIS(outdir = NULL, var = "LAI", site_info = site_info, product_dates = c(start_YEARDOY, end_YEARDOY),
                          run_parallel = TRUE, ncores = ncores, product = "MOD15A2H", band = "LaiStdDev_500m",
                          package_method = "MODISTools", QC_filter = TRUE, progress = FALSE), lai_sd)
  }
  
  #format LAI data by "Site_ID", "Date", "Median", "qc", "SD"
  LAI <- cbind(lai_data[,c(5, 2, 9, 10)], lai_sd$data)
  colnames(LAI) <- c("Site_ID", "Date", "Median", "qc", "SD")
  
  #filter by qc band
  LAI <- LAI[-(which(LAI$qc=="001")),]
  
  #compute peak LAI per year per site
  peak_lai = data.frame()
  
  #loop over each site
  for (i in 1:length(unique(LAI$Site_ID))) {
    site_ID <- unique(LAI$Site_ID)[i]
    site_LAI <- LAI[which(LAI$Site_ID==site_ID),]
    
    #loop over each year
    for (j in 1:length(unique(lubridate::year(site_LAI$Date)))) {
      site_LAI_year <- site_LAI[which(lubridate::year(site_LAI$Date)==unique(lubridate::year(site_LAI$Date))[j]),]
      
      #calculate the difference between target date and record date
      target_date <- lubridate::date(as.Date(paste0(as.character(unique(lubridate::year(site_LAI$Date))[j]),"/07/15")))
      diff_days <- abs(lubridate::days(lubridate::date(site_LAI_year$Date)-lubridate::date(target_date))@day)
      
      #find records within search window
      max <- site_LAI_year[which(diff_days<=Search_Window),]
      
      #if no record in search window
      if(nrow(max)==0){
        peak <- data.frame(Site_ID=site_ID, Date=paste0("Year_", unique(lubridate::year(site_LAI$Date))[j]), Median=NA, SD=NA)
      }else{#we do have records
        peak <- data.frame(Site_ID=site_ID, Date=paste0("Year_", unique(lubridate::year(site_LAI$Date))[j]), Median=mean(max$Median), SD=max(max$SD))
      }
      peak_lai <- rbind(peak_lai, peak)
    }
  }
  #lower boundaries for LAI std
  peak_lai$SD[peak_lai$SD < 0.66] = 0.66
  
  #extracting AGB data
  med_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info, "median", buffer = NULL, fun = "mean", AGB_dir, product_dates=lubridate::year(start_date):lubridate::year(end_date))[[1]]
  sdev_agb_data <- PEcAn.data.remote::extract.LandTrendr.AGB(site_info, "stdv", buffer = NULL, fun = "mean", AGB_dir, product_dates=lubridate::year(start_date):lubridate::year(end_date))[[1]]
  
  #formatting AGB data
  ndates = colnames(med_agb_data)[-c(1:2)] # getting dates
  med_agb_data$Site_Name = as.character(med_agb_data$Site_Name, stringsAsFactors = FALSE)
  med_agb_data = reshape2::melt(med_agb_data, id.vars = "Site_ID", measure.vars = colnames(med_agb_data)[-c(1:2)])
  
  sdev_agb_data$Site_Name = as.character(sdev_agb_data$Site_Name, stringsAsFactors = FALSE)
  sdev_agb_data = reshape2::melt(sdev_agb_data, id.vars = "Site_ID", measure.vars = colnames(sdev_agb_data)[-c(1:2)])
  
  agb_data = as.data.frame(cbind(med_agb_data, sdev_agb_data$value))
  names(agb_data) = c("Site_ID", "Date", "Median", "SD")
  agb_data$Date = as.character(agb_data$Date, stringsAsFactors = FALSE)
  
  
  
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
  
  obs.mean <- split(obs.mean, obs.mean$date)
  
  # change the dates to be middle of the year
  date.obs <- strsplit(names(obs.mean), "_") %>%
    purrr::map_chr(~paste0(.x[2], "/07/15"))

  obs.mean <- purrr::map(
    names(obs.mean),
    function(namesl){
      split(
        obs.mean[[namesl]],
        obs.mean[[namesl]]$site_id) %>%
      purrr::map(
        ~.x[3:4] %>%
          stats::setNames(c("AbvGrndWood", "LAI")) %>%
          `row.names<-`(NULL))
    }
  ) %>% stats::setNames(date.obs)
  
  #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  names = date.obs
  for (name in names)
  {
    for (site in names(obs.mean[[name]]))
    {
      na_index = which(!(is.na(obs.mean[[ name]][[site]])))
      colnames = names(obs.mean[[name]][[site]])
      #we have some records that are not NA
      if (length(na_index) > 0)
      {
        obs.mean[[name]][[site]] = obs.mean[[name]][[site]][na_index]
      }else if(length(na_index) == 0){#we don't have any observations (they are all NAs), we then just remove the whole site
        obs.mean[[name]][[site]] <- NULL
      }
    }
  }
  
  # fillers are 0's for the covariance matrix. This will need to change for differing size matrixes when more variables are added in.
  # filler_0 = as.data.frame(matrix(0, ncol = length(observed_vars), nrow = nrow(observed_data)))
  # names(filler_0) = paste0("h", seq_len(length(observed_vars)))
  
  # create obs.cov dataframe -->list by date
  obs.cov = data.frame(date = observed_data$Date, site_id = observed_data$Site_ID, sdev_agb = observed_data$sdev_agb, sdev_lai = observed_data$sdev_lai)#, filler_0)
  obs.cov$date = as.character(obs.cov$date, stringsAsFactors = F)
  
  obs.cov <- split(obs.cov, obs.cov$date)

  obs.cov <- purrr::map(
    names(obs.cov),
    function(namesl){
      purrr::map(
        split(
          obs.cov[[namesl]],
          obs.cov[[namesl]]$site_id),
        ~.x[3:4]^2 %>%
          unlist %>%
          diag(nrow = 2, ncol = 2))
    }
  ) %>% stats::setNames(date.obs)
  
  
  names = date.obs
  for (name in names)
  {
    for (site in names(obs.cov[[name]]))
    {
      #if we don't have any observation (diag(cov)==NA) then we remove the whole site
      if(length(which(!is.na(diag(obs.cov[[name]][[site]])))) == 0)
      {
        obs.cov[[name]][[site]] <- NULL
        next
      }
      #else we do have some records
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
  save(peak_lai, file = paste0(out_dir, '/peak_lai.Rdata'))
  save(obs.mean, file = paste0(out_dir, '/obs_mean.Rdata'))
  save(obs.cov, file = paste0(out_dir, '/obs_cov.Rdata'))
  list(cov=obs.cov, mean=obs.mean)
}
