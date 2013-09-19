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
