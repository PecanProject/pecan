
get.latlonbox <- function(lati, loni, Lat = Lat, Lon = Lon){
    lat <- c(mean(Lat[lati:(lati-1)]), mean(Lat[lati:(lati+1)]))
    lon <- c(mean(Lon[loni:(loni-1)]), mean(Lon[loni:(loni+1)]))
    return(c(sort(lat), sort(lon)))
}
 

ncep_dt2weather <- function(weather = result, lati = lati){

    currentlat <- round(Lat[lati], 2)

    x <- weather[year < 2012 & year > 1948,
                 list(year, day = doy,
                      tavg = ud.convert(qctemp(air), "Kelvin", "Celsius"),
                      tmax = ud.convert(qctemp(tmax), "Kelvin", "Celsius"),
                      tmin = ud.convert(qctemp(tmin), "Kelvin", "Celsius"),
                      dswrf.MJ   = ud.convert(qcsolar(dswrf), "watt day", "megajoule"), 
                      wnd  = sqrt(qcwind(uwnd)^2 + qcwind(vwnd)^2),
                      precip.day = ud.convert(qcprecip(prate), "mm s-1", "mm day-1"),
                      shum = qcshum(shum),
                      rhum = qcrh(rhum))]
        
    x$rhmax <- x[, qair2rh(shum, tmin)]
    x$rhmin <- x[, qair2rh(shum, tmax)]
    x$rhavg <- x[, (rhmax + rhmin) / 2]

    forweach <- x[,list(year, day, dswrf.MJ,
                        tmax, tmin, tavg,
                        rhmax, rhmin, rhavg,
                        wnd, precip.day)]
 
    dat <- weachDT(forweach, lati = lati)
    return(dat)
}

datetoDMY<-function(date){
  day   <- as.numeric(substr(date,start=4,stop=5))
  month <- as.numeric(substr(date,start=1,stop=2))
  year  <- as.numeric(substr(date,start=7,stop=10))
  list(day=day,month=month,year=year)
}

# function to check leap year; returns 1 if leap year;  0 if not leap year
isleapyear<-function(year){
  if(year%%100==0)
  {
    if(year%%400==0)test<-1
  }
  else
  {
    if(year%%4==0) {
      test<-1
    }
    else test<-0
  }
  test
}



# function to obtain day1 and dayn based on planting and harvest date
# input must be in the form of output generated from datetoDMY function
#returns a list with day1 and dayn

getday1dayn<-function(pdate,hdate)
{
  
  d1<-pdate$day
  m1<-pdate$month
  y1<-pdate$year
  dn<-hdate$day
  mn<-hdate$month
  yn<-hdate$year
  
  # calculating day lost in planting year
  
  # get number of days from date of planting and beginning of year
  if((isleapyear(y1))==1)
    days<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  if(!(isleapyear(y1))==1)
    days<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(m1==1)
    doy1<-pdate$day
  else
    doy1<-sum(days[1:(m1-1)])+pdate$day
  
  # no of days between day of harvesting and 1st day of year
  if((isleapyear(yn))==1)
    days<-c(31,29,31,30,31,30,31,31,30,31,30,31)
  if(!(isleapyear(yn))==1)
    days<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(mn==1)
    doyn<-hdate$day
  else
    doyn<-sum(days[1:(mn-1)])+hdate$day
  
  if(y1==yn)    #case2 when planting and harvest takes place in the same year
  {
    doy1<-doy1
    doyn<-doyn
  }
  else    #case2 when planting and harvest do not take place in the same year
  {
    dayn<-0
    for (i in y1:(yn-1)){
      if (isleapyear(i)==1)
        days<-sum(c(31,29,31,30,31,30,31,31,30,31,30,31))
      if (!(isleapyear(i)==1))
        days<-sum(c(31,28,31,30,31,30,31,31,30,31,30,31))
      dayn<-dayn+days
    }
    doyn=dayn+doyn
    doy1=doy1
  }
  list(day1=doy1,dayn=doyn)
}
