library(tidyr)
library(tidyverse)
library(lubridate)

download_US_Los_met <- function(start_date, end_date) {
  
  base_url <- "http://co2.aos.wisc.edu/data/cheas/lcreek/flux/current/ameriflux/US-Los_HH_"   
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # Reading in the data
  data <- start_year:end_year %>%
    purrr::map_df(function(syear){
      influx <-
        tryCatch(
          read.table(
            paste0(base_url, syear, '01010000_', syear + 1, "01010000.csv"),
            sep = ",",
            header = TRUE
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
    mutate_all(funs(as.numeric)) %>% 
    mutate(Time = lubridate::ymd_hm(TIMESTAMP_START)) %>% 
    dplyr::select(Time, WS_1_1_1, TA_1_1_1) %>% 
    dplyr::na_if(-9999) %>% 
    add_column(rH = NA)%>% 
    filter(Time >= start_date & Time <=end_date) 
  colnames(data) = c("date", "ws", 'Tair', "rH")
  return(data)
}
