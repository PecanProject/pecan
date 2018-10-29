download_US_WCr <- function(start_date, end_date) {
  base_url <- "http://flux.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek"
  
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
  # Some cleaning and filtering 
  #raw.data <- raw.data %>% 
   # select(-V5, -V6) %>%
  #  filter(date <=end_date)
  
  #Colnames changed
  
  return(raw.data)
}
# start_date <- as.Date("2017-01-01")
# end_date <- as.Date("2018-10-01")
# 
# download_US_WCr(start_date, end_date) ->pp
# 
# tail(pp)