variable = c("dlwrf","dswrf","prcp","pres","shum","tas","wind")

year = 1948:2008

for(v in variable){
  for(y in year){
    
    fname = paste(v,"_3hourly_",y,"-",y,".nc",sep="")
    system(paste("wget http://hydrology.princeton.edu/data/pgf/1.0deg/3hourly/",fname,sep=""),wait=TRUE)
  }
}
