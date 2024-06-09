##' Get MODIS phenology data by date and location  
##' 
##' @export
##' @param site_info A dataframe of site info containing the BETYdb site ID, 
##' site name, latitude, and longitude, e.g. 
##' @param start_date Start date to download data
##' @param end_date End date to download data
##' @param outdir Path to store the outputs
##' @param run_parallel optional method to download data parallely. Only works if more than 1 
##' site is needed and there are >1 CPUs available.
##' @param ncores number of cpus to use if run_parallel is set to TRUE. If you do not know the 
##' number of CPU's available, enter NULL. 
##' @return the path for output file 
##' The output file will be saved as a CSV file to the outdir.
##' Output column names are "year", "site_id", "lat", "lon", "leafonday","leafoffday","leafon_qa","leafoff_qa"
##' @author Qianyu Li


extract_phenology_MODIS<- function(site_info,start_date,end_date,outdir,run_parallel = TRUE,ncores = NULL){ 
 
  if (is.null(outdir)) {
    PEcAn.logger::logger.error("No output directory found. Please provide it.")
  } else {
    #Set up the start and end date of the extraction
    start_YEARDOY <- paste0(lubridate::year(start_date),sprintf("%03d", 1))
    end_YEARDOY <- paste0(lubridate::year(end_date),sprintf("%03d", 365))
    #Extracting leaf-on, leaf-off, and Quality Assurance (QA) from MODIS MCD12Q2 product
    leafon_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                               var = "Greenup", 
                                               site_info = site_info, 
                                               product_dates = c(start_YEARDOY, end_YEARDOY),
                                               run_parallel = as.logical(run_parallel), 
                                               ncores = ncores, 
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
                                                ncores = ncores, 
                                                product = "MCD12Q2", 
                                                band = "MidGreendown.Num_Modes_01",
                                                package_method = "MODISTools", 
                                                QC_filter = FALSE, 
                                                progress = TRUE)

    qa_data <- PEcAn.data.remote::call_MODIS(outdir = NULL, 
                                           var = "QA", 
                                           site_info = site_info, 
                                           product_dates = c(start_YEARDOY, end_YEARDOY),
                                           run_parallel = as.logical(run_parallel), 
                                           ncores = ncores, 
                                           product = "MCD12Q2", 
                                           band = "QA_Detailed.Num_Modes_01",
                                           package_method = "MODISTools", 
                                           QC_filter = FALSE, 
                                           progress = TRUE)

    #Example function for unpacking QA for each date phenometric from MCD12Q2_Collection6_UserGuide
    #in the order: Greenup, MidGreenup, Maturity, Peak, Senescence, MidGreendown, Dormancy                                        
    UnpackDetailedQA <- function(x){
       bits <- as.integer(intToBits(x))
       quals <- sapply(seq(1, 16, by=2), function(i) sum(bits[i:(i+1)] * 2^c(0, 1)))[1:7]
       return(quals)
      }
    #Extract QA for leaf-on and leaf-off dates. Values are in the range 0–3 corresponding to “best”, “good”, “fair”, and “poor”.
    qa_ind<-matrix(NA,nrow=length(qa_data[,1]),ncol=2)
    for (i in seq_along(qa_data$data)){
      qa_ind[i,]<-cbind(UnpackDetailedQA(qa_data$data[i])[2],UnpackDetailedQA(qa_data$data[i])[6])
    }
  
    leafphdata <- cbind(leafon_data %>% as.data.frame %>% dplyr::select("calendar_date", "site_id", "lat", "lon", "data"), leafoff_data$data,qa_ind)%>% 
    `colnames<-`(c("year", "site_id", "lat", "lon", "leafonday","leafoffday","leafon_qa","leafoff_qa"))
    leafphdata$leafonday[leafphdata$leafonday==32767]<-NA #exclude the data with fill values
    leafphdata$leafoffday[leafphdata$leafoffday==32767]<-NA
    leafphdata$leafonday[leafphdata$leafon_qa==3]<-NA #exclude the data when QA is poor
    leafphdata$leafoffday[leafphdata$leafoff_qa==3]<-NA
    leafphdata$leafonday[leafphdata$leafonday>leafphdata$leafoffday]<-NA #exclude the data when leaf-on date is larger than leaf-off date
    leafphdata$leafonday<-lubridate::yday(as.Date(leafphdata$leafonday)) #convert the dates to Day-of-Year format
    leafphdata$leafoffday<-lubridate::yday(as.Date(leafphdata$leafoffday))
    leafphdata$year<-lubridate::year(leafphdata$year)
    leafphdata$site_id<-as.character(leafphdata$site_id)
    
    file_path<-file.path(outdir,"leaf_phenology.csv")
    PEcAn.logger::logger.info(paste0("Storing results in: ",file_path))
    utils::write.csv(leafphdata, file = file_path, row.names = FALSE)
    return(file_path)
  }
}