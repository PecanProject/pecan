sipnet2datetime <- function(year, doy, hour){

  hr <- floor(hour)
  minsec <- (hour - hr) * 60
  minute <- floor(minsec)

  sec <- (minsec - minute) * 60

  minute <- ifelse(sec == 60, minute + 1, minute)
  sec <- ifelse(sec == 60, 0, sec)

  hr <- ifelse(minute == 60, hr + 1, hr)
  minute <- ifelse(minute == 60, 0, minute)

  datetime <- strptime(
    paste(year, doy, hr, minute, sec),
    "%Y %j %H %M %S",
    tz = "UTC"
  )

  as.POSIXct(datetime, tz = "UTC")
}



datetime2sipnet <- function(dt) {
  list(
    year = lubridate::year(dt),
    yday = lubridate::yday(dt),
    hour = lubridate::hour(dt) +
      lubridate::minute(dt)/60 +
      lubridate::second(dt)/3600
  )
}
