download_US_WLEF_met <- function(start_date, end_date) {
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wlef/flux/prelim/"
  
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # Reading in the data
  raw.data <- start_year:end_year %>%
    purrr::map_df(function(syear) {
      influx <-
        tryCatch(
          read.table(
            paste0(base_url, syear, "/met_", syear, ".txt"),
            sep = "",
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
    mutate_all(funs(as.numeric))
  
  #Constructing the date based on the columns we have
  raw.data$date <-as.POSIXct(paste0(raw.data$Year,"/",raw.data$MO,"/",raw.data$DD," ", raw.data$HH),
                             format="%Y/%m/%d %H", tz="UTC")
  
  # Some cleaning and filtering 
  raw.data <- raw.data %>% 
    dplyr::select(Year,MO,DD,HH,DOY, fDOY, T122, RH122, Patm, Precip, PAR, date) %>%
    filter(date >= start_date & date <=end_date)
  
  colnames(raw.data) <- c("Year", "Month", "Day", "Hour", "DOY", "FjDay", "Tair", "rH", "P_atm", "Rain", "PAR", "date")
  
  return(raw.data)
}