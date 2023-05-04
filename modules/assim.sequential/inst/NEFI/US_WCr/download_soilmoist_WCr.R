download_soilmoist_WCr <- function(start_date, end_date) {
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wcreek/flux/prelim/clean/ameriflux/US-WCr_HH_"
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # Reading in the data
  raw.data <- start_year:end_year %>%
    purrr::map_df(function(syear) {
      influx <-
        tryCatch(
          read.table(
            paste0(base_url, syear, "01010000_", syear+1, "01010000.csv"),
            sep = ",",
            header = TRUE, stringsAsFactors = F
          ) %>%
            apply(2, trimws) %>%
            apply(2, as.character) %>%
            data.frame(stringsAsFactors = F),
          error = function(e) {
            NULL
          },
          warning = function(e) {
            NULL
          }
        )
    }) %>%
    mutate_all(funs(as.numeric))
  
  #Constructing the date based on the columns we have
  if(dim(raw.data)[1] > 0 & dim(raw.data)[2] > 0){
    raw.data$Time <-as.POSIXct(as.character(raw.data$TIMESTAMP_START),
                               format="%Y%m%d%H%M", tz="UTC")
    # Some cleaning and filtering 
    # SWC origionally has units = % at depths 2_1 = 5cm, 2_2 = 10cm, 2_3 = 20cm, 2_4 = 30cm, 2_5 = 40cm, 2_6 = 50cm
    raw.data <- raw.data %>% 
      dplyr::select(SWC_1_1_1, SWC_1_2_1, SWC_1_3_1, SWC_1_4_1, SWC_1_5_1, SWC_2_1_1, SWC_2_2_1, SWC_2_3_1, SWC_2_4_1, SWC_2_5_1, SWC_2_6_1, Time) %>%
      na_if(-9999) %>% 
      filter(Time >= start_date & Time <=end_date)
    
    #get average soil moisture 
    
    #with all depths
    #raw.data$avgsoil <- raw.data$SWC_2_1_1*.05 + raw.data$SWC_2_2_1*.10 + raw.data$SWC_2_3_1*.20 + raw.data$SWC_2_4_1*.30 + raw.data$SWC_2_5_1*.40 + raw.data$SWC_2_6_1*.50
    
    #shallow depths (>30cm)
    raw.data$avgsoil2 <- raw.data$SWC_2_1_1 #*.05 + raw.data$SWC_2_2_1*.10 + raw.data$SWC_2_3_1*.20
    raw.data$avgsoil1 <- raw.data$SWC_1_2_1*0.12 + raw.data$SWC_1_3_1*0.16 + raw.data$SWC_1_4_1*0.32 + raw.data$SWC_1_5_1*0.4  #old sensor
    raw.data <- raw.data %>% dplyr::select(Time, avgsoil1, avgsoil2)
  }else(raw.data <- NULL)
  return(raw.data)
}
