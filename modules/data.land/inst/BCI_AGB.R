## Barro Colorado Island AGB timeseries
library(dplyr)
indir <- "~/Dropbox/Desktop/Projects/BCI/"
files <- dir(indir,pattern = "*.txt",full.names = TRUE)
files <- files[grep(pattern = "Census",files)]
nf <- length(files)
stats <- data.frame(date=rep(NA,nf),AGB=rep(NA,nf))

area <- 50 ## ha

for(f in seq_along(files)){
  myfile = files[f]
  
  ## get census number
  sl <- nchar(myfile)
  i  <- as.numeric(substr(myfile,start = sl-4,stop = sl-4))
  print(c(f,i))
  
  ## read raw data
  dat <- read.table(myfile,header = TRUE,as.is = TRUE,sep="\t")
  
  ## filter data
  dat <- dplyr::filter(dat,Status == 'alive')
  dbh <- dat$DBH/10
  
  ## allometric estimation of height and AGB
  m = 0.64
  c = 0.37
  max_dbh = 68
  wood_density = 0.75 ## shameless hack -- should be set on a species basis
  h = ifelse(dbh < max_dbh,10^(log10(dbh) * m + c),10^(log10(max_dbh)*m + c)) ## FATES allom, meters
  bdead = 0.06896*(h^0.572)*(dbh^1.94)*wood_density^0.931  ## FATES allom, units = KgC??
  
  ## conversion to plot-level mean
  stats$AGB[i] <- udunits2::ud.convert(sum(bdead,na.rm=TRUE)/area,"kg ha-1","kg m-2")  #AbvGrndWood	kgC m-2
  
  ## extract dates
  date <- as.Date(dat$Date)
  
  ## assign the plot the mean census date
  stats$date[i] <- mean(date,na.rm=TRUE)
  
  ## save in case there's a crash
  saveRDS(stats,"BCI_AGB.RDS")
}

## sanity check
agb <- udunits2::ud.convert(stats$AGB,"kg m-2","Mg ha-1")/0.48
plot(as.Date(stats$date,origin = "1970-01-01"),agb)
stats$AGB[3] <- NA ## odd outlier, original value = 22.95379

## save as csv
outstats <- data.frame(date=as.Date(stats$date,origin = "1970-01-01"),AGB=stats$AGB)
write.csv(outstats,file = "BCI_AGB.csv",quote = FALSE,row.names = FALSE)
