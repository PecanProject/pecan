createInput <- function() {
    days <- read.table("4Rdate.dat")

    start <- days2month(days[2,2])
    end <- days2month(days[1,2])

    ed2in.text <- readLines(con='ED2IN', n=-1)
    ed2in.text <- gsub('@START_DAY@',start[1],ed2in.text)
    ed2in.text <- gsub('@START_MONTH@',start[2],ed2in.text)
    ed2in.text <- gsub('@END_DAY@',end[1],ed2in.text)
    ed2in.text <- gsub('@END_MONTH@',end[2],ed2in.text)

    writeLines(ed2in.text, con='R_ED2IN')
}


days2month <- function(days){
    ec <- c(0,31,59,90,120,151,181,212,243,273,302,334,365)

    for(i in 1:12){
      if(days > ec[i]){
        if(days < ec[i+1]){
	  month <- i
	}
      }
    }

    day <- days - ec[month]
    c(day,month)
}
