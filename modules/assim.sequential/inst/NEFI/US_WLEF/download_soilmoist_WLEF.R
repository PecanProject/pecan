download_soilmoist_WLEF <- function(start_date, end_date) {
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wlef/flux/prelim/clean/ameriflux/US-PFa_HR_"
  
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
  raw.data$Time <-as.POSIXct(as.character(raw.data$TIMESTAMP_START),
                             format="%Y%m%d%H%M", tz="UTC")
  # Some cleaning and filtering 
  raw.data <- raw.data %>% 
    dplyr::select(SWC_1_1_1, Time) %>%
    na_if(-9999) %>% 
    filter(Time >= start_date & Time <=end_date)
  colnames(raw.data) <- c('avgsoil', 'Time')
  return(raw.data)
}
