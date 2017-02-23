# The R script that produces the ED2 namelist from the T_ED2IN template.
# The only things changed from the template are the initial and ending dates.
# DART control vector gives the values as days from an arbitrary starting point
# Here that starting point is the first day of the year.
# The script here converts those days in to a date used by the ED2IN.

createInput <- function() {
    years <- read.table("date.dat")

    ayear <- years[1,]
    zyear <- years[2,]

    histo <- readLines(con='histo.dat')
    analy <- readLines(con='analy.dat')

    ed2in.text <- readLines(con='T_ED2IN', n=-1)
    ed2in.text <- gsub('@ANALY@',analy,ed2in.text)
    ed2in.text <- gsub('@HISTO@',histo,ed2in.text)
    ed2in.text <- gsub('@START_YEAR@',ayear,ed2in.text)
    ed2in.text <- gsub('@END_YEAR@',zyear,ed2in.text)

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
