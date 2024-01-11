phenology_MODIS_extract<- function(settings,NCore = NULL, run_parallel = TRUE,update_csv = TRUE){
  outdir<-settings$model$leaf_phenology$outdir
  site_info <- settings %>% 
    purrr::map(~.x[['run']] ) %>% 
    purrr::map('site')%>% 
    purrr::map(function(site.list){
      #conversion from string to number
      site.list$lat <- as.numeric(site.list$lat)
      site.list$lon <- as.numeric(site.list$lon)
      list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
    })%>% 
    dplyr::bind_rows() %>% 
    as.list()
  
  start_date <- as.Date(settings$state.data.assimilation$start.date, tz="UTC")
  end_date <- as.Date(settings$state.data.assimilation$end.date, tz="UTC")
  
  if(file.exists(file.path(outdir, "leaf_phenology_pecan.csv"))){
    Previous_CSV <- utils::read.csv(file.path(outdir, "leaf_phenology_pecan.csv"))
  }

  start_YEARDOY <- paste0(lubridate::year(start_date),sprintf("%03d", 1))#using 1 and 365 DOY to avoid any possible missing data.
  end_YEARDOY <- paste0(lubridate::year(end_date),sprintf("%03d", 365))
  
  leafon_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                               var = "Greenup", 
                                               site_info = site_info, 
                                               product_dates = c(start_YEARDOY, end_YEARDOY),
                                               run_parallel = as.logical(run_parallel), 
                                               ncores = NCore, 
                                               product = "MCD12Q2", 
                                               band = "MidGreenup.Num_Modes_01",
                                               package_method = "MODISTools", 
                                               QC_filter = FALSE, 
                                               progress = TRUE)
  leafoff_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                                var = "Greendown", 
                                                site_info = site_info, 
                                                product_dates = c(start_YEARDOY, end_YEARDOY),
                                                run_parallel = as.logical(run_parallel), 
                                                ncores = NCore, 
                                                product = "MCD12Q2", 
                                                band = "MidGreendown.Num_Modes_01",
                                                package_method = "MODISTools", 
                                                QC_filter = FALSE, 
                                                progress = TRUE)
  
  
  qc_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                           var = "QA", 
                                           site_info = site_info, 
                                           product_dates = c(start_YEARDOY, end_YEARDOY),
                                           run_parallel = as.logical(run_parallel), 
                                           ncores = NCore, 
                                           product = "MCD12Q2", 
                                           band = "QA_Detailed.Num_Modes_01",
                                           package_method = "MODISTools", 
                                           QC_filter = FALSE, 
                                           progress = TRUE)
  UnpackDetailedQA <- function(x){
    bits <- as.integer(intToBits(x))
    quals <- sapply(seq(1, 16, by=2), function(i) sum(bits[i:(i+1)] * 2^c(0, 1)))[1:7]
    return(quals)
  }
  
  qa_ind<-matrix(NA,nrow=length(qc_data[,1]),ncol=2)
  for (i in seq_along(qc_data$data)){
    qa_ind[i,]<-cbind(UnpackDetailedQA(qc_data$data[i])[2],UnpackDetailedQA(qc_data$data[i])[6])
  }
  
  leafphdata <- cbind(leafon_data %>% as.data.frame %>% dplyr::select("calendar_date", "site_id", "lat", "lon", "data"), leafoff_data$data,qa_ind)%>% 
    `colnames<-`(c("year", "site_id", "lat", "lon", "leafonday","leafoffday","leafon_qa","leafoff_qa"))
  leafphdata$leafonday[leafphdata$leafonday==32767]<-NA
  leafphdata$leafoffday[leafphdata$leafoffday==32767]<-NA
  leafphdata$leafonday[leafphdata$leafon_qa==3]<-NA
  leafphdata$leafoffday[leafphdata$leafoff_qa==3]<-NA
  leafphdata$leafonday[leafphdata$leafonday>leafphdata$leafoffday]<-NA
  leafphdata$leafonday<-lubridate::yday(as.Date(leafphdata$leafonday))
  leafphdata$leafoffday<-lubridate::yday(as.Date(leafphdata$leafoffday))
  leafphdata$year<-lubridate::year(leafphdata$year)
  leafphdata$site_id<-as.character(leafphdata$site_id)
  
  if(as.logical(update_csv)){
    if(exists("Previous_CSV")){#we already read the csv file previously.
      Current_CSV <- rbind(Previous_CSV, leafphdata)
      Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$year)),]#using site_id and date to remove duplicated records.
      utils::write.csv(Current_CSV, file = file.path(outdir, "leaf_phenology_pecan.csv"), row.names = FALSE)
    }else{
      Current_CSV <- leafphdata
      utils::write.csv(Current_CSV, file = file.path(outdir, "leaf_phenology_pecan.csv"), row.names = FALSE)
    }
  }
}