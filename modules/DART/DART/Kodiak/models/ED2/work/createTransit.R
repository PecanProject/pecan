# The R script that produces the ED2 namelist from the T_ED2IN template.
# The only things changed from the template are the initial and ending dates.
# DART control vector gives the values as days from an arbitrary starting point
# Here that starting point is the first day of the year.
# The script here converts those days in to a date used by the ED2IN.

createTransit <- function() {
    year <- as.numeric(readLines('sim_year'))

    ayears <- year 
#    adays <- 365
    adays <- as.numeric(readLines('end_date'))

    zyears <- year + 1
    zdays <- 105

    start <- days2month(adays)
    end <- days2month(zdays)

    ed2in.text <- readLines(con='R_ED2IN', n=-1)
    ed2in.text <- gsub('@START_DAY@',start[1],ed2in.text)
    ed2in.text <- gsub('@START_MONTH@',start[2],ed2in.text)
    ed2in.text <- gsub('@START_YEAR@',ayears,ed2in.text)
    ed2in.text <- gsub('@END_DAY@',end[1],ed2in.text)
    ed2in.text <- gsub('@END_MONTH@',end[2],ed2in.text)
    ed2in.text <- gsub('@END_YEAR@',zyears,ed2in.text)

    writeLines(ed2in.text, con='ED2IN')
}


days2month <- function(days){
    ec <- c(0,31,59,90,120,151,181,212,243,273,304,334,365)

    for(i in 1:12){
      if(days > ec[i]){
        if(days <= ec[i+1]){
	  month <- i
	}
      }
    }

    day <- days - ec[month]
    c(day,month)
}
createTransit()
