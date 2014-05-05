# The R script that produces the ED2 namelist from the T_ED2IN template.
# The only things changed from the template are the initial and ending dates.
# DART control vector gives the values as days from an arbitrary starting point
# Here that starting point is the first day of the year.
# The script here converts those days in to a date used by the ED2IN.

createInput <- function() {
    days <- read.table("4Rdate.dat")
    year <- as.numeric(readLines('sim_year'))
#    year <- 2002

    if(days[2,2] == 365){
     ayears <- year + days[2,2]%/%365 - 1
     adays <- 365
     }
    else {
     ayears <- year + days[2,2]%/%365
     adays <- days[2,2]%%365
    }

    if(days[1,2] == 365){
     zyears <- year + days[1,2]%/%365 -1
     zdays <- 365
     }
    else {
      zyears <- year + days[1,2]%/%365
      zdays <- days[1,2]%%365
    }

    begin <- days2month(adays)
    final <- days2month(zdays)
    histo <- readLines(con='histo.dat')

    ed2in.text <- readLines(con='T_ED2IN', n=-1)
    ed2in.text <- gsub('@HISTO@',histo,ed2in.text)
    ed2in.text <- gsub('@START_DAY@',begin[1],ed2in.text)
    ed2in.text <- gsub('@START_MONTH@',begin[2],ed2in.text)
    ed2in.text <- gsub('@START_YEAR@',ayears,ed2in.text)
    ed2in.text <- gsub('@END_DAY@',final[1],ed2in.text)
    ed2in.text <- gsub('@END_MONTH@',final[2],ed2in.text)
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
createInput()
