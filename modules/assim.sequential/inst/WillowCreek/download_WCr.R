download_US_WCr_met <- function(start_date, end_date) {
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek"
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # Reading in the data
  raw.data <- start_year:end_year %>%
    purrr::map_df(function(syear) {
      influx <-
        tryCatch(
          read.table(
            paste0(base_url, syear, "_met.txt"),
            sep = "",
            header = FALSE
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
  #Converting the WCR data from CST to UTC 
  raw.data$date <-lubridate::with_tz(as.POSIXct(paste0(raw.data$V1,"/",raw.data$V2,"/",raw.data$V3," ", raw.data$V4 %>% as.integer(), ":",(raw.data$V4-as.integer(raw.data$V4))*60),
                                                format="%Y/%m/%d %H:%M", tz="US/Central"), tz = "UTC")
  
  
  
  start_date <- as.POSIXct(start_date, format = "%Y-%m-%d", tz = "UTC")
  end_date <- as.POSIXct(end_date, format = "%Y-%m-%d", tz = "UTC")
  # Some cleaning and filtering 
  raw.data <- raw.data %>% 
    dplyr::select(V1,V2,V3,V4,V5, V6, V26, V35, V40, V59, date) %>%
    filter(date >= start_date & date <=end_date)
  
  #Colnames changed
  colnames(raw.data) <- c("Year", "Month", "Day", "Hour", "DoY", "FjDay", "Tair", "rH", "Tsoil", "Rg", "date")
  
  return(raw.data)
}

download_US_WCr_flux <- function(start_date, end_date) {
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek"
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # Reading in the data
  raw.data <- start_year:end_year %>%
    purrr::map_df(function(syear) {
      influx <-
        tryCatch(
          read.table(
            paste0(base_url, syear, "_flux.txt"),
            sep = "",
            header = FALSE
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
  raw.data$date <-as.POSIXct(paste0(raw.data$V1,"/",raw.data$V2,"/",raw.data$V3," ", raw.data$V4 %>% as.integer(), ":",(raw.data$V4-as.integer(raw.data$V4))*60),
                             format="%Y/%m/%d %H:%M", tz="UTC")
  
  start_date <- as.POSIXct(start_date, format = "%Y-%m-%d", tz = "UTC")
  end_date <- as.POSIXct(end_date, format = "%Y-%m-%d", tz = "UTC")
  
  # Some cleaning and filtering 
  raw.data <- raw.data %>% 
    # select(-V5, -V6) %>%
    dplyr::filter(date >= start_date & date <=end_date) 
  #Colnames changed
  colnames(raw.data) <- c("Year", "Month", "Day", "Hour", "DoY", "FjDay", "SC", "FC", "NEE", "LE", "H", "Ustar", "Flag", "date")
  
  return(raw.data)
}
