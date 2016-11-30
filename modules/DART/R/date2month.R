days2month <- function(days) {
  ec <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 302, 334, 365)
  
  for (i in 1:12) {
    if (days > ec[i]) {
      if (days < ec[i + 1]) {
        month <- i
      }
    }
  }
  
  day <- days - ec[month]
  return(c(day, month))
} # days2month
