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
  
 
  if(dim(raw.data)[1] > 0 & dim(raw.data)[2] > 0){
  #Constructing the date based on the columns we have
  raw.data$date <-as.POSIXct(paste0(raw.data$V1,"/",raw.data$V2,"/",raw.data$V3," ", raw.data$V4 %>% as.integer(), ":",(raw.data$V4-as.integer(raw.data$V4))*60),
                             format="%Y/%m/%d %H:%M", tz="UTC")
  # Some cleaning and filtering 
  raw.data <- raw.data %>% 
    dplyr::select(V1,V2,V3,V4,V5, V6, V26, V35, V40, V59, V51, V61, V17, V58, date) %>%
    filter(date >= start_date & date <=end_date)
  
  #Colnames changed
  colnames(raw.data) <- c("Year", "Month", "Day", "Hour", "DoY", "FjDay", "Tair", "rH", "Tsoil", "Rg", "P_atm", "LW", "WS" , "Rain", "date")
  }else(raw.data = NULL)
  return(raw.data)
}
# start_date <- as.Date("2017-01-01")
# end_date <- as.Date("2018-10-01")
# 
#download_US_WCr_met(start_date, end_date) ->met
# 
# tail(pp)