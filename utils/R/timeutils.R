#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
############################################################################################
#
#		Set of time utilities for manipulating time variables
#		and creating time axes for plots  
#
############################################################################################

#data(time.constants)

#==========================================================================================#
#==========================================================================================#
#      Function that determines whether the year is leap or not.                           #
#------------------------------------------------------------------------------------------#
is.leap = function(when){

   if (is.chron(when)){
      year = numyears(when)
   }else{
      year = when
   }#end if

   leaptf = year %% 400 == 0 | (year %% 4 == 0 & year %% 100 != 0)
   return(leaptf)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that determines the number of days in a given month.                       #
#------------------------------------------------------------------------------------------#
daymax = function(month,year){
  mmm  = c(31,28,31,30,31,30,31,31,30,31,30,31)
  mday = mmm[month]

  addone       = month == 2 & is.leap(year)
  mday[addone] = mday[addone] + 1

  return(mday)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      
#------------------------------------------------------------------------------------------#
##' convert month abbrev. to numeric
##'
##' Function that determines the number of the month given the character name.
##' @title mmm2mon
##' @param mmm three letter character string for month
##' @param lang currently english and portugese 
##' @return month as three letter 
mmm2mon = function(mmm,lang="English"){
  lang = tolower(lang)
  if (lang == "english"){
     m3l  = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  }else if(lang == "portuguese"){
     m3l  = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
  }#end if

  mmmloc = tolower(substring(as.character(mmm),1,3))
  monout = match(mmmloc,m3l)
  return(monout)
} #end function
#==========================================================================================#


#==========================================================================================#
#==========================================================================================#
#
#------------------------------------------------------------------------------------------#
##' Convert numeric month to 3-letter abbrev.
##'
##' Function that determines 3-letter name of the month given their number.
##' @title mon2mmm
##' @param mon 3-letter month abbreviation
##' @param lang charcter, currently supports "english", "portugese"
##' @return month as three letter abbreviation
##' @export
##' @examples
##' mon2mmm(1)
##' mon2mmm(1:3)
mon2mmm = function(mon,lang="English"){
  lang = tolower(lang)
  if (lang == "english"){
    m3l  = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  }else if(lang == "portuguese"){
    m3l  = c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")
  }#end if
  
  monout = m3l[mon]
  return(monout)
} #end function
#==========================================================================================#
#==========================================================================================#

#==========================================================================================#
#==========================================================================================#
##' Convert a chron object to numeric years.
##'
##' @title numyears
##' @param when chron object
##' @return numeric year
##' @author Shawn Serbin
numyears = function(when){
   yrs    = years(when)
   lyrs   = levels(yrs)
   yrout  = as.numeric(lyrs[match(yrs,lyrs)])
   return(yrout)
}#end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that converts a chron object to numeric months.                            #
#------------------------------------------------------------------------------------------#
nummonths = function(when){
   mos    = months(when)
   lmos   = levels(mos)
   moout  = match(mos,lmos)
   return(moout)
}#end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that converts a chron object to numeric days.                              #
#------------------------------------------------------------------------------------------#
numdays = function(when){
   dys    = days(when)
   ldys   = levels(dys)
   dyout  = match(dys,ldys)
   return(dyout)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that returns the dates as characters.                                      #
#------------------------------------------------------------------------------------------#
chardates = function(when){
   mymonth = substring(100   + nummonths(when),2,3)
   myday   = substring(100   + numdays  (when),2,3)
   myyear  = substring(10000 + numyears (when),2,5)
   mydate  = paste(mymonth,myday,myyear,sep="/")
  return(mydate)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that returns the dates as characters.                                      #
#------------------------------------------------------------------------------------------#
label.dates = function(when,add.hours=TRUE){
   mymonth = substring(100   + nummonths(when),2,3)
   myday   = substring(100   + numdays  (when),2,3)
   myyear  = substring(10000 + numyears (when),2,5)
   mydate  = paste(myyear,mymonth,myday,sep="-")

   if (add.hours){
      mytime  = paste(substring(100 + hours  (when),2,3)
                     ,substring(100 + minutes(when),2,3)
                     ,substring(100 + seconds(when),2,3)
                     ,sep="")
      mylabel = paste(mydate,mytime,sep="-")
   }else{
      mylabel = mydate
   }#end if

  return(mylabel)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that returns the times as characters.                                      #
#------------------------------------------------------------------------------------------#
chartimes = function(when){
   myhour = substring(100 + hours  (when),2,3)
   myminu = substring(100 + minutes(when),2,3)
   myseco = substring(100 + seconds(when),2,3)
   mytime = paste(myhour,myminu,myseco,sep=":")
  return(mytime)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that finds the fraction of the day.                                        #
#------------------------------------------------------------------------------------------#
hms2frac = function(when){
   thishour  = hours    (when)
   thismin   = minutes  (when)
   thissec   = seconds  (when)

   elapsed = thishour / day.hr + thismin / day.min + thissec / day.sec
   return(elapsed)
}#end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      Function that finds the numeric version of the days.                                #
#------------------------------------------------------------------------------------------#
dayofyear = function(when){
   offdays   = c(0, 31,59,90,120,151,181,212,243,273,304,334,365)

   thisday   = numdays  (when)
   thismonth = nummonths(when)
   thisyear  = numyears (when)
   thisfrac  = hms2frac (when)
   
   addone    = as.integer(thismonth > 2 & is.leap(when))

   doy =  thisday + offdays[thismonth] + addone + thisfrac
   return(doy)
} #end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#     This function appends several time-related variables for a given data frame.         #
#------------------------------------------------------------------------------------------#
alltimes = function(datin,lon,lat,ed21=TRUE,zeronight=FALSE,meanval=FALSE,imetavg=1
                   ,nmean=120,...){
   #------ Copy the input data frame, and call the other functions. -----------------------#
   datout = datin
   datout$year       = numyears (datout$when)
   datout$month      = nummonths(datout$when)
   datout$day        = numdays  (datout$when)
   datout$hour       = hours    (datout$when)
   datout$minu       = minutes  (datout$when)
   datout$today      = dates    (datout$when)
   datout$tomonth    = chron(paste(datout$month,1,datout$year,sep="/"))
   datout$doy        = dayofyear(datout$when)
   zenith            = ed.zen   (when=datout$when,lon=lon,lat=lat,ed21=ed21
                                ,zeronight=zeronight,meanval=meanval,imetavg=imetavg
                                ,nmean=nmean,...)
   datout$cosz       =   zenith$cosz
   datout$sunhgt     =   zenith$hgt
   datout$nighttime  =   zenith$night
   datout$daytime    =   zenith$day
   datout$twilight   = (! zenith$night) & (! zenith$day)
   datout$notdaytime = ! zenith$day

   return(datout)
}#end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      List of trimestral seasons.                                                         #
#------------------------------------------------------------------------------------------#
season <<- function(when,add.year=FALSE){


   #----- Get the year and month. ---------------------------------------------------------#
   year = numyears (when)
   mon  = nummonths(when)
   #---------------------------------------------------------------------------------------#



   #----- We don't give summer/winter, instead we make generic season names. --------------#
   sidx = c( 4, 4, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Assign the season names depending on the month and year.                          #
   #---------------------------------------------------------------------------------------#
   if (add.year){
      #----- Add year before the season. --------------------------------------------------#
      seasout      = paste(year       ,substring(100+sidx[mon     ],2,3),sep="")
      #------------------------------------------------------------------------------------#


      #----- December, January, and February have two years. ------------------------------#
      mm1          = mon %in%  c(1,2)
      seasout[mm1] = paste(year[mm1]-1,substring(100+sidx[mon[mm1]],2,3),sep="")
      #------------------------------------------------------------------------------------#
   }else{
      #----- No year to be tagged. --------------------------------------------------------#
      seasout = sidx[mon]
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#



   #----- Return variable. ----------------------------------------------------------------#
   return(seasout)
   #---------------------------------------------------------------------------------------#
}#end for
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#     This function creates a pretty time scale.  It is loosely based on pretty, but here  #
# we make extensive use of the chron functions, and define the suitable scales in a        #
# different way as time has a non-decimal scale.                                           #
#  The result is a list containing the levels, and nice labels for the plots.              #
#------------------------------------------------------------------------------------------#
pretty.time = function(when,n=10,...){

   #----- Find the 1st and last time. -----------------------------------------------------#
   whena = min(when,na.rm=TRUE)
   whenz = max(when,na.rm=TRUE)

   #----- Table for accepted bases for months, hours, minutes, and seconds. ---------------#
   base.months = c(1,2,3,4,6)
   base.days   = c(1,2,4,7,15)
   base.hours  = c(1,2,3,4,6,12)
   base.minsec = c(1,2,5,10,15,20,30)

   #----- Convert time to seconds. --------------------------------------------------------#
   when.sec = as.numeric(when) * day.sec

   #---------------------------------------------------------------------------------------#
   #    Find a first guess of the step size, so we decide whether to use years, months,    #
   # days, hours, minutes, or seconds.                                                     #
   #---------------------------------------------------------------------------------------#
   wstep.1st  = mean(diff(pretty(when.sec,n)))

   if (wstep.1st == 0){
      myunit=NA
      #---- Whatever, use just what comes out from the regular pretty. --------------------#
      vlevels = chron(pretty(when,n))
      vlabels = as.character(vlevels)
      padj    = rep(0,times=length(vlabels))
   }else if(wstep.1st / yr.sec > 0.8){
      myunit="years"
      #------------------------------------------------------------------------------------#
      #     Years are the best scale for the plots.                                        #
      #------------------------------------------------------------------------------------#
      yrrange = numyears(when)
      vlevels = pretty(yrrange,n)
      vlevels = dates(x=paste(1,1,vlevels,sep="/"))

      vlabels = paste(months(vlevels),years(vlevels),sep="-")
      padj    = rep(0,times=length(vlabels))
   }else if(wstep.1st / (30. * day.sec) > 0.8){
      myunit="months"
      #------------------------------------------------------------------------------------#
      #     Months are the best scale for the plots.                                       #
      #------------------------------------------------------------------------------------#
      #----- Find the time step that is the closest to the base. --------------------------#
      wstep     = wstep.1st / (30. * day.sec)
      whichbase = base.months[which.min(abs(wstep-base.months))]

      #----- Find the list of years to plot. ----------------------------------------------#
      allyears = numyears(when)
      yeara    = min(allyears,na.rm=TRUE)
      yearz    = max(allyears,na.rm=TRUE)+1
      vlevels  = seq.dates(from = paste(1,1,yeara,sep="/")
                          ,to   = paste(1,1,yearz,sep="/")
                          ,by   = "months")
      mon1st   = nummonths(vlevels)
      monlevs  = seq(from=1,to=12,by=whichbase)

      #----- Find the limits that will keep the labels not too far from the data. ---------#
      wlaba    = dates(paste(nummonths(whena),1,numyears(whena),sep="/"))
      monz     = nummonths(whenz) %% 12 + 1
      yearz    = numyears(whenz) + as.integer(monz == 1)
      wlabz    = dates(paste(monz,1,yearz,sep="/"))
      sel      = ( mon1st %in% monlevs 
                 & vlevels >= min(wlaba,na.rm=TRUE)
                 & vlevels <= max(wlabz,na.rm=TRUE) )
      vlevels = dates(vlevels[sel],out.format="m/d/year")
      vlabels = paste(months(vlevels),years(vlevels),sep="-")
      padj    = rep(0,times=length(vlabels))

   }else if(wstep.1st / day.sec > 0.8){
      myunit="days"
      #------------------------------------------------------------------------------------#
      #     Days are the best scale for the plots, but we keep them tethered to months,    #
      # even if the grid becomes slightly irregular.                                       #
      #------------------------------------------------------------------------------------#
      #----- Find the time step that is the closest to the base. --------------------------#
      wstep     = wstep.1st / day.sec
      whichbase = base.days[which.min(abs(wstep-base.days))]
      #----- Find the list of years to plot. ----------------------------------------------#
      allyears = numyears(when)
      yeara    = min(allyears,na.rm=TRUE)
      yearz    = max(allyears,na.rm=TRUE)+1
      #------------------------------------------------------------------------------------#
      #     Impose the list of months to be from January to December, we will trim the     #
      # numbers later.                                                                     #
      #------------------------------------------------------------------------------------#
      montha   = 1
      monthz   = 12
      #------------------------------------------------------------------------------------#
      #     Impose the list of months to be from January to December, we will trim the     #
      # numbers later.                                                                     #
      #------------------------------------------------------------------------------------#
      daylevs=seq(from=1,to=31-whichbase+1,by=whichbase)
      #----- First guess for the levels. --------------------------------------------------#
      vlevels  = seq.dates(from = paste(1,1,yeara,sep="/")
                          ,to   = paste(1,1,yearz,sep="/")
                          ,by   = "days")
      day1st   = numdays(vlevels)

      #----- Find the limits that will keep the labels not too far from the data. ---------#
      wlaba    = dates(whena)
      dayz     = numdays(whenz) %% daymax(nummonths(whenz),numyears(whenz)) + 1
      monz     = 1 + (nummonths(whenz) - 1 + as.integer(dayz==1)) %% 12
      yearz    = numyears(whenz) + as.integer(monz == 1)
      wlabz    = dates(paste(monz,dayz,yearz,sep="/"))
      sel      = ( day1st %in% daylevs 
                 & vlevels >= min(wlaba,na.rm=TRUE)
                 & vlevels <= max(wlabz,na.rm=TRUE) )
      vlevels = dates(vlevels[sel],out.format="m/d/y")
      vlabels    = paste(months(vlevels),days(vlevels),sep="/")

      padj    = rep(0,times=length(vlabels))

      sel        = vlevels == vlevels[1] | (numdays(vlevels) == 1 & nummonths(vlevels) == 1)
      vlabels[sel] = paste(months(vlevels[sel]),"/",days(vlevels[sel]),"\n"
                          ,years(vlevels[sel]),sep="")
      padj[sel]    = 0.5
   }else if(wstep.1st / hr.sec > 0.8){
      myunit="hours"
      #------------------------------------------------------------------------------------#
      #     Hours are the best scale for the plots.                                        #
      #------------------------------------------------------------------------------------#
      #----- Find the time step that is the closest to the base. --------------------------#
      wstep     = wstep.1st / hr.sec
      whichbase = base.hours[which.min(abs(wstep-base.hours))]
      #----- Find the list of days to plot. -----------------------------------------------#
      when1st  = dates(min(when  ,na.rm=TRUE))
      whenlast = dates(max(when+1,na.rm=TRUE))
      mydates  = seq.dates(from=when1st,to=whenlast,by="days")
      mytimes  = times(seq(from=0,to=day.sec-1,by=whichbase*hr.sec)) / day.sec
      ndates   = length(mydates)
      ntimes   = length(mytimes)
      #----- First guess for the levels. --------------------------------------------------#
      vlevels  = chron(dates=rep(x=mydates,each=ntimes),times=rep(x=mytimes,times=ndates))
      wlaba    = chron(dates=paste(nummonths(whena),numdays(whena),numyears(whena),sep="/"),
                       times=paste(hours(whena),0,0,sep=":"))
      hourz    = (hours(whenz) + 1) %% 24
      d2831    = daymax(nummonths(whenz),numyears(whenz))
      dayz     = (numdays(whenz) - 1 + as.integer(hourz == 0)) %% d2831 + 1
      monz     = (nummonths(whenz) - 1 + as.integer(dayz == 1)) %% 12 + 1
      yearz    = numyears(whenz) + as.integer(monz == 1)
      wlabz    = chron(dates=paste(monz,dayz,yearz,sep="/"),times=paste(hourz,0,0,sep=":"))
      sel      = ( vlevels >= min(wlaba,na.rm=TRUE)
                 & vlevels <= max(wlabz,na.rm=TRUE) )

      #------------------------------------------------------------------------------------#
      #     Make the labels, and put day and month information only on the first time of   #
      # the day, and all information in the second time, and the first time of the year.   #
      #------------------------------------------------------------------------------------#
      vlabels      = paste(substring(100+hours(vlevels),2,3)
                          ,substring(100+minutes(vlevels),2,3),sep=":")
      padj         = rep(0,times=length(vlabels))
      #----- First time of the day. -------------------------------------------------------#
      sel          = hours(vlevels) == 0
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #----- First time of the year. ------------------------------------------------------#
      sel          = ( vlevels == vlevels[1] 
                     |  ( nummonths(vlevels) == 1 & numdays(vlevels) == 1 
                        & hours(vlevels) == 0 ))
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),"\n"
                          ,years(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #------------------------------------------------------------------------------------#


   }else if(wstep.1st / min.sec > 0.8){
      myunit="minutes"
      #------------------------------------------------------------------------------------#
      #     Minutes are the best scale for the plots.                                      #
      #------------------------------------------------------------------------------------#
      #----- Find the time step that is the closest to the base. --------------------------#
      wstep     = wstep.1st / min.sec
      whichbase = base.minsec[which.min(abs(wstep-base.minsec))]
      #----- Find the list of days to plot. -----------------------------------------------#
      when1st  = dates(min(when  ,na.rm=TRUE))
      whenlast = dates(max(when+1,na.rm=TRUE))
      mydates  = seq.dates(from=when1st,to=whenlast,by="days")
      mytimes  = times(seq(from=0,to=day.sec-1,by=whichbase*min.sec)) / day.sec
      ndates   = length(mydates)
      ntimes   = length(mytimes)
      #----- First guess for the levels. --------------------------------------------------#
      vlevels  = chron(dates=rep(x=mydates,each=ntimes),times=rep(x=mytimes,times=ndates))

      wlaba    = chron(dates=paste(nummonths(whena),numdays(whena),numyears(whena),sep="/"),
                       times=paste(hours(whena),minutes(whena),0,sep=":"))
      minz     = (minutes(whenz) + 1) %% 60
      hourz    = (hours(whenz) + as.integer(minz == 0)) %% 24
      d2831    = daymax(nummonths(whenz),numyears(whenz))
      dayz     = (numdays(whenz) - 1 + as.integer(hourz == 0)) %% d2831 + 1
      monz     = (nummonths(whenz) - 1 + as.integer(dayz == 1)) %% 12 + 1
      yearz    = numyears(whenz) + as.integer(monz == 1)
      wlabz    = chron(dates=paste(monz,dayz,yearz,sep="/")
                      ,times=paste(hourz,minz,0,sep=":"))
      sel      = ( vlevels >= min(wlaba,na.rm=TRUE)
                 & vlevels <= max(wlabz,na.rm=TRUE) )

      #------------------------------------------------------------------------------------#
      #     Make the labels, and put day and month information only on the first time of   #
      # the day, and all information in the second time, and the first time of the year.   #
      #------------------------------------------------------------------------------------#
      vlabels      = paste(substring(100+hours(vlevels),2,3)
                          ,substring(100+minutes(vlevels),2,3),sep=":")
      padj         = rep(0,times=length(vlabels))
      #----- First time of the day. -------------------------------------------------------#
      sel          = hours(vlevels) == 0 & minutes(vlevels) == 0
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #----- First time of the year. ------------------------------------------------------#
      sel          = ( vlevels == vlevels[1] 
                     |  ( nummonths(vlevels) == 1 & numdays(vlevels) == 1 
                        & hours(vlevels) == 0 & minutes(vlevels) == 0))
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),"\n"
                          ,years(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #------------------------------------------------------------------------------------#



   }else{
      myunit="seconds"
      #------------------------------------------------------------------------------------#
      #     Minutes are the best scale for the plots.                                      #
      #------------------------------------------------------------------------------------#
      #----- Find the time step that is the closest to the base. --------------------------#
      wstep     = wstep.1st
      whichbase = base.minsec[which.min(abs(wstep-base.minsec))]
      #----- Find the list of days to plot. -----------------------------------------------#
      when1st  = dates(min(when  ,na.rm=TRUE))
      whenlast = dates(max(when+1,na.rm=TRUE))
      mydates  = seq.dates(from=when1st,to=whenlast,by="days")
      mytimes  = times(seq(from=0,to=day.sec-1,by=whichbase)) / day.sec
      ndates   = length(mydates)
      ntimes   = length(mytimes)
      #----- First guess for the levels. --------------------------------------------------#
      vlevels  = chron(dates=rep(x=mydates,each=ntimes),times=rep(x=mytimes,times=ndates))

      wlaba    = chron(dates=paste(nummonths(whena),numdays(whena),numyears(whena),sep="/"),
                       times=paste(hours(whena),minutes(whena),seconds(whena),sep=":"))
      secz     = (seconds(whenz) + 1) %% 60
      minz     = (minutes(whenz) + as.integer(secz == 0)) %% 60
      hourz    = (hours(whenz) + as.integer(minz == 0)) %% 24
      d2831    = daymax(nummonths(whenz),numyears(whenz))
      dayz     = (numdays(whenz) - 1 + as.integer(hourz == 0)) %% d2831 + 1
      monz     = (nummonths(whenz) - 1 + as.integer(dayz == 1)) %% 12 + 1
      yearz    = numyears(whenz) + as.integer(monz == 1)
      wlabz    = chron(dates=paste(monz,dayz,yearz,sep="/")
                      ,times=paste(hourz,minz,secz,sep=":"))
      sel      = ( vlevels >= min(wlaba,na.rm=TRUE)
                 & vlevels <= max(wlabz,na.rm=TRUE) )

      #------------------------------------------------------------------------------------#
      #     Make the labels, and put day and month information only on the first time of   #
      # the day, and all information in the second time, and the first time of the year.   #
      #------------------------------------------------------------------------------------#
      vlabels      = paste(substring(100+hours(vlevels),2,3)
                          ,substring(100+minutes(vlevels),2,3)
                          ,substring(100+seconds(vlevels),2,3),sep=":")
      padj         = rep(0,times=length(vlabels))
      #----- First time of the day. -------------------------------------------------------#
      sel          = hours(vlevels) == 0 & minutes(vlevels) == 0 & seconds(vlevels) == 0
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),":"
                          ,substring(100+seconds(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #----- First time of the year. ------------------------------------------------------#
      sel          = ( vlevels == vlevels[1] 
                     |  ( nummonths(vlevels) == 1 & numdays(vlevels) == 1 
                        & hours(vlevels) == 0  & minutes(vlevels) == 0 
                        & seconds(vlevels) == 0))
      vlabels[sel] = paste(substring(100+hours(vlevels[sel]),2,3),":"
                          ,substring(100+minutes(vlevels[sel]),2,3),":"
                          ,substring(100+seconds(vlevels[sel]),2,3),"\n"
                          ,months(vlevels[sel]),"-",days(vlevels[sel]),"\n"
                          ,years(vlevels[sel]),sep="")
      padj[sel]    = 0.5
      #------------------------------------------------------------------------------------#
   }#end if

   vresult=list(levels=vlevels,labels=vlabels,n=length(vlevels),scale=myunit,padj=padj)
   return(vresult)
}#end function
#==========================================================================================#
#==========================================================================================#






#==========================================================================================#
#==========================================================================================#
#      List with the names of months and seasons.                                          #
#------------------------------------------------------------------------------------------#
mlist        <<- c("January","February","March","April","May","June","July","August"
                  ,"September","October","November","December")
season.list  <<- c("MAM","JJA","SON","DJF")
#==========================================================================================#
#==========================================================================================#
