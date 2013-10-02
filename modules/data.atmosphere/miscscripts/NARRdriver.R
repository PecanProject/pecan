## script to drive NARR download

#year <- 2008
#mo <- 8
moTXT <- c("01","02","03","04","05","06","07","08","09","10","11","12")


for(year in seq(1984,1979,by=-1)){
  for(mo in c(1:12)){
    system(paste("./NARR.wget",year,moTXT[mo]))
  }
}
