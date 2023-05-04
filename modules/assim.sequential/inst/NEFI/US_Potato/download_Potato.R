#if then statement for date? Only the last 4 days are available in the current folder
#second part would be in the year folder that corresponds with the start date. 
#only need the current system date from the current folder. The rest are in the year folder

download_US_Potato<- function(start_date, end_date) {
  
  
  #Each .dat file is a seperate half hour file.  
  date = seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = 'day')
  
  t <- format(seq(from=as.POSIXct("0000","%H%M", tz="UTC"),
                  to=as.POSIXct("2330", "%H%M", tz="UTC"),
                  by="30 min", format = "%H%M"), "%H%M")
  #read in the headers to start the data frame
  data = read.delim("http://co2.aos.wisc.edu/data/potato/2019/20190128/Potato_flux_2019_01_28_0000.dat", sep = ",", skip = 1, stringsAsFactors = FALSE)[1,]
  
  #the current day is saved in a different folder so we need a different url
  if(end_date == Sys.Date()){ 
    
    for(j in 1: (length(date)-1)){
      
      for( i in 1: length(t)){
        
        
        baseurl <- "http://co2.aos.wisc.edu/data/potato/"
        url <- paste0(baseurl, lubridate::year(date[j]), "/", gsub("-", "", (date[j])), "/Potato_flux_", gsub("-", "_", (date[j])), "_", t[i], ".dat")
        if(url.exists(url)){
          data<- rbind(data, read.delim(url, sep = ",", skip = 1, stringsAsFactors = FALSE)[3,])
        }else {
          index.time <- strsplit(t[i], "")
          index <- c(paste0(date[j], " ", index.time[[1]][1], index.time[[1]][2] , ":", index.time[[1]][3], index.time[[1]][4] , ":00"), rep("NA", 97))
          data <- rbind(data, index )
        }
        
      }
    }
    
    #Have to adjust the time length because it's current. Going back 2 hours just to be safe with lag and errors
    p <- format(seq(from=as.POSIXct("0000","%H%M", tz="UTC"),
                    to=as.POSIXct(format(lubridate::round_date(Sys.time() - lubridate::hours(2), unit = "30 min"), "%H%M"), "%H%M", tz="UTC"),
                    by="30 min", format = "%H%M"), "%H%M")
    
    for(i in 1:length(p)){
      
      url1 <- paste0("http://co2.aos.wisc.edu/data/potato/current/Potato_flux_", gsub("-", "_", end_date), "_", p[i], ".dat")
      data<- rbind(data, read.delim(url1, sep = ",", skip = 1, stringsAsFactors = FALSE)[3,])
      
    }
  } else{
    
    for(j in 1: (length(date))){
      
      for( i in 1: length(t)){
        
        baseurl <- "http://co2.aos.wisc.edu/data/potato/"
        url <- paste0(baseurl, lubridate::year(date[j]), "/", gsub("-", "", (date[j])), "/Potato_flux_", gsub("-", "_", (date[j])), "_", t[i], ".dat")
        if(url.exists(url)){
          data<- rbind(data, read.delim(url, sep = ",", skip = 1, stringsAsFactors = FALSE)[3,])
        }else {
          index.time <- strsplit(t[i], "")
          index <- c(paste0(date[j], " ", index.time[[1]][1], index.time[[1]][2] , ":", index.time[[1]][3], index.time[[1]][4] , ":00"), rep("NA", 97))
          data <- rbind(data, index )
        }
        
      }
    }
  }
  #want to pull out timestamp, u_star, 
  
  #clean data 
  
  data <- data[-1,] #remove units 
  
  data <- data %>% dplyr::select("TIMESTAMP", "u_star" ,"LE_wpl", "Fc_wpl", "CO2_sig_strgth_mean", "H2O_sig_strgth_mean") %>% 
    mutate(NEE = replace(Fc_wpl, u_star < 0.1, "NA"), LE = replace(LE_wpl, u_star < 0.1, "NA")) %>% 
    mutate(NEE = replace(NEE, CO2_sig_strgth_mean < 0.6, "NA"), LE = replace(LE, H2O_sig_strgth_mean < 0.6, "NA")) %>% 
    dplyr::select("TIMESTAMP", "NEE", "LE") %>% 
    mutate(NEE = as.numeric(NEE), LE = as.numeric(LE)) %>% 
    na_if( -999) %>%
    mutate(NEE = PEcAn.utils::misc.convert(NEE, "umol C m-2 s-1", "kg C m-2 s-1"), LE = as.numeric(LE)) 
  
  

  colnames(data) <- c("Time", "NEE", "LE")
  return(data)
}



#download_potato(start_date, end_date)
#start_date = "2019-07-22"
#end_date = "2019-07-24"


