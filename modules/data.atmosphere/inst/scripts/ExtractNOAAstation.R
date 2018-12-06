###define defaults for debugging
##elem       <- NULL
##ID         <- NULL
##DSETcheck  <- NULL
##Flag1check <- c("M","S")
##Flag2check <- c("2","3","T","U")

ExtractNOAAstation <- function(data,elem=NULL,ID=NULL,DSETcheck=NULL,Flag1check=c("M","S"),Flag2check=c("2","3","T","U")){
  ## function that converts/extracts NOAA met station data
  ## INPUTS:
  ## data - raw data table
  ## elem - variables to extract (default: extract all)
  ## ID - station ID's to extract (default: extract all)
  ## DSETcheck - values to ignore (default: none)
  ##     3200 -> valid, DYSW table D
  ##     3210 -> mostly valid, DYSW table E, invalid SNOW & SNWD after 06/1996, TMIN/TMAX often recorded as missing from 1990-1994 if <= -10 F
  ##     3201 & 3202 -> preliminary
  ## Flag1check - values to ignore (default: c("M","S"))
  ## (notimp)   by default if flag is "A" or "B" the value is difference from previous
  ##  (notimp)  if A follows a "M" or "S" then average value is assigned across days
  ##  (notimp)  if flag is "T" value is set to 1/2 trace value
  ## Flag2check - values to ignore (default: c("2","3","T","U"))
  ##
  ## OUTPUTS::
  ##   list with a "date" table and a table for each elem (date X ID)
  ##   date table contains year, month, day, decimal day (aka "julian") and decimal year
  ##   values are converted to NA if they fail the checks
  
  sel <- which(!(as.character(data$DSET) == "----"))
  data <- data[sel,]
  
  ## fill in defaults & set up storage
  out <- list()
  if(is.null(elem)) {   ## set list of variables if not given
    elem <- as.character(unique(data$ELEM))
    elem <- elem[which(!(elem == "----"))]
  }
  obsID <- as.character(unique(data$COOPID)) # get list of observed stations
  obsID <- obsID[which(!(obsID == "------"))]
  if(is.null(ID)) ID <- obsID
  ID <- ID[ID %in% obsID]  ## exclude ID's not in data set

  nvar <- length(elem)     # number of variables 
  nstation <- length(ID)   # number of stations

  ## remove invalid DSET
  if(!is.null(DSETcheck)){
    sel <- which(!(data$DSET %in% DSETcheck))
    data <- data[sel,]
  }

  ##create date
  yrmo <- sort(as.character(unique(data$YEARMO)))
  yr <- as.numeric(substr(as.character(data$YEARMO),1,4))
  mo <- as.numeric(substr(as.character(data$YEARMO),5,6))
  data <- cbind(data,yr,mo)
  days <- diff(monthday)
  ldays <- diff(leapmonthday)
  date <- NULL
  for(i in 1:length(yrmo)){
    y <- as.numeric(substr(yrmo[i],1,4))
    m <- as.numeric(substr(yrmo[i],5,6))
    n <- days[m]
    if(y %% 4 == 0) n <- ldays[m]
    dtmp <- cbind(rep(y,n),rep(m,n),1:n,(1:n)+monthday[m])
    if(is.null(date)) {
      date <- dtmp
    } else {date <- rbind(date,dtmp)}
  }
  date <- cbind(date,jday(date[,1],date[,2],date[,3]))
  colnames(date) <- c("year","month","day","julian","decyr")
  out[[1]] <- date
  ddate <- date[,1]*10000+date[,2]*100+date[,3]

  ## create variables
  for(i in 1:nvar){
    vtemp <- matrix(NA,nrow(date),nstation)
    vsel <- which(as.character(data$ELEM) == elem[i])
    for(j in 1:nstation){
      
    idsel <- vsel[which(as.character(data$COOPID[vsel]) == ID[j])]
      
    for(d in 1:31){
      ##find right column
      coln <- which(colnames(data) == paste("DAY",cday[d],sep=""))
      val <- as.numeric(as.character(data[idsel,coln]))
      ## check flags
      flag1 <- as.character(data[idsel,coln+1])
      flag2 <- as.character(data[idsel,coln+2])
      val[flag1 %in% Flag1check] <- NA
      val[flag2 %in% Flag2check] <- NA

      ## define units conversion
      if(elem[i] %in% c("PRCP","SNOW","SNWD","TMIN","TMAX","TOBS")){
        val <- switch(elem[i],
                      PRCP = 0.254*val,  ## hundreth inches/day -> mm/day
                      SNOW = 0.245*val,  ## tenth in/day ->cm/day
                      SNWD = 2.45*val,   ## in ->cm
                      TMAX = (val-32)*5/9, ## F -> C
                      TMIN = (val-32)*5/9, ## F -> C
                      TOBS = (val-32)*5/9  ## F -> C
                      )
      }
        
      ## match data to correct date
      mydate <- data$yr[idsel]*10000+data$mo[idsel]*100+d
      mtch <- match(mydate,ddate)
      vtemp[mtch[!is.na(mtch)],j] <- val[!is.na(mtch)]
      
    } ## loop over days
  } ## loop over stations

    out[[i+1]] <- vtemp
    
  } ## loop over variables

  names(out) <- c("date",elem)
  return(out)
}


ExtractNOAAstationMonthly <- function(data,elem=NULL,ID=NULL,DSETcheck=NULL,Flag1check=c("M","S"),Flag2check=c("2","3","T","U")){
  ## function that converts/extracts NOAA met station data
  ## INPUTS:
  ## data - raw data table
  ## elem - variables to extract (default: extract all)
  ## ID - station ID's to extract (default: extract all)
  ## DSETcheck - values to ignore (default: none)
  ##     3200 -> valid, DYSW table D
  ##     3210 -> mostly valid, DYSW table E, invalid SNOW & SNWD after 06/1996, TMIN/TMAX often recorded as missing from 1990-1994 if <= -10 F
  ##     3201 & 3202 -> preliminary
  ## Flag1check - values to ignore (default: c("M","S"))
  ## (notimp)   by default if flag is "A" or "B" the value is difference from previous
  ##  (notimp)  if A follows a "M" or "S" then average value is assigned across days
  ##  (notimp)  if flag is "T" value is set to 1/2 trace value
  ## Flag2check - values to ignore (default: c("2","3","T","U"))
  ##
  ## OUTPUTS::
  ##   list with a "date" table and a table for each elem (date X ID)
  ##   date table contains year, month, day, decimal day (aka "julian") and decimal year
  ##   values are converted to NA if they fail the checks
  
  sel <- which(!(as.character(data$COOPID) == "------"))
  data <- data[sel,]
  
  ## fill in defaults & set up storage
  out <- list()
  if(is.null(elem)) {   ## set list of variables if not given
    elem <- as.character(unique(data$ELEM))
    elem <- elem[which(!(elem == "----"))]
  }
  obsID <- as.character(unique(data$COOPID)) # get list of observed stations
  obsID <- obsID[which(!(obsID == "------"))]
  if(is.null(ID)) ID <- obsID
  ID <- ID[ID %in% obsID]  ## exclude ID's not in data set

  nvar <- length(elem)     # number of variables 
  nstation <- length(ID)   # number of stations

  ## remove invalid DSET
##  if(!is.null(DSETcheck)){
##    sel <- which(!(data$DSET %in% DSETcheck))
##    data <- data[sel,]
##  }

  ##create date
  yr <- as.numeric(as.character(sort(unique(data$YEAR))))
  mo <- rep(1:12,length(yr))
  yr <- rep(yr,each=12)
  out[[1]] <- date <- cbind(yr,mo,yr+(mo-0.5)/12)
    

  ## create variables
  for(i in 1:nvar){
    vtemp <- matrix(NA,nrow(date),nstation)            ## set storage
    vsel <- which(as.character(data$ELEM) == elem[i])  ## select variable
    for(j in 1:nstation){
      
      idsel <- vsel[which(as.character(data$COOPID[vsel]) == ID[j])] ## select station
      
      for(m in 1:12){
        ##find right column
        coln <- 7+5*m #which(colnames(data) == paste("DAY",cday[d],sep=""))
        val <- as.numeric(as.character(data[idsel,coln]))
        ## check flags
        flag1 <- as.character(data[idsel,coln+1])
        flag2 <- as.character(data[idsel,coln+2])
        val[flag1 %in% Flag1check] <- NA
        val[flag2 %in% Flag2check] <- NA
        
        ## define units conversion
        if(elem[i] %in% c("TPCP","TSNW","MNTM")){
          val <- switch(elem[i],
                        TPCP = 0.254*val,  ## hundreth inches/month -> mm/month
                        TSNW = 0.254*val,  ## tenth in/month ->cm/month
                        MNTM = (val*0.1-32)*5/9, ## tenth F -> C
                        )
        }
        
        ## match data to correct date
        mydate <- as.numeric(as.character(data$YEAR[idsel]))+(m-0.5)/12
        mtch <- match(mydate,date[,3])
        vtemp[mtch[!is.na(mtch)],j] <- val[!is.na(mtch)]
        
      } ## loop over months
    } ## loop over stations
    
    out[[i+1]] <- vtemp
    
  } ## loop over variables

  out[[i+2]] <- ID
  
  names(out) <- c("date",elem,"ID")
  return(out)
}
