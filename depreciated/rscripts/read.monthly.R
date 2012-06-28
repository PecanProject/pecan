  ### CODE for simple diagnostic plots for ED2.1 HDF output files

  #########  MONTHLY   #########
dat <- read.monthly("/home/dlebauer/output/rockspring/","an_panvar4.")

  ## path <- "~/output/rockspring/"
  ## prefix <- "analysis."
  plot.monthly(dat)


  read.monthly <- function(path,prefix){

  require(hdf5,lib.loc="/home/dlebauer/lib/R")

  flist <- dir(path,prefix,full.names=TRUE)         ## grab all files
  flist <- flist[grep("-E-",flist)] ## select just monthly

  out <- list()

  for(i in 1:length(flist)){

  dat <- hdf5load(flist[i],load=FALSE)

  if(length(out) == 0){
    for(j in 1:length(dat)){
      out[[j]] <- list()
      out[[j]][[1]] <- dat[[j]]        
    }
    names(out) <- names(dat)
  } else {
    n <- length(out[[1]]) + 1
    for(j in 1:length(dat)){
      out[[j]][[n]] <- dat[[j]]
    }      
  }

  }

  return(out)

}



plot.monthly <- function(dat){

nmonth <- length(dat[[1]])

## First the simple diagnostics that don't depend on number of cohorts or patches  

## AGB by PFT
dt<-1:657
agb <- array(NA,dim=c(nmonth))
for(i in dt){
agb[i] <- dat$AGB_PFT[[i]][1,15]
}
x <- (dt)/12 + 1948
par(cex=1.5)
plot(x[dt],agb[dt], ylab = "Aboveground Biomass (Mg ha-1)", xlab = "year",type="l", ylim = c(0,max(agb[!is.na(agb)])*1.2),xlim = c(1948,2006))
# points(c(2002+10.5/12,2003+3.5/12,2003+11/12,2004+3.3/12,2004+11.3/12,2005+3.2/12),c(6.69,6.83,6.95,4.72,7.02,4.03))
# lines(c(2002+10.5/12,2003+3.5/12,2003+11/12,2004+3.3/12,2004+11.3/12,2005+3.2/12),c(6.69,6.83,6.95,4.72,7.02,4.03),lty=2)
# text(1998,2,"solid line = model") 
# text(1998,1.6,"circles/dashed line = observed")

}

