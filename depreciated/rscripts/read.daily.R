### CODE for simple diagnostic plots for ED2.1 HDF output files

#########  Daily   #########

##path <- "~/output/rockspring/"
##prefix <- "analysis."

read.daily <- function(path,prefix){

  require(hdf5,lib.loc="/home/dlebauer/lib/R")
  
  flist <- dir(path,prefix,full.names=TRUE)         ## grab all files
  flist <- flist[grep("-D-",flist)] ## select just daily

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

#dat <- read.monthly(path,prefix)

plot.daily <- function(dat){

  npoly <- dat$NPOLYGONS_GLOBAL[[1]]
  npft <- ncol(dat$AGB_PFT[[1]])
  nmonth <- length(dat[[1]])
  
  ## First the simple diagnostics that don't depend on number of cohorts or patches  

  ## AGB by PFT
  agb_pft <- array(NA,dim=c(nmonth,npoly,npft))
  for(i in 1:nmonth){
    agb_pft[i,,] <- dat$AGB_PFT[[i]]
  }
  agb_poly <- matrix(NA,nmonth,npoly)
  agb_gpft <- matrix(NA,nmonth,npft)
  for(i in 1:nmonth){
    for(j in 1:npoly){
      agb_poly[i,j] <- sum(agb_pft[i,j,])
    }
    for(j in 1:npft){
      agb_gpft[i,j] <- sum(agb_pft[i,,j])
    }
  }  
  agb <- apply(agb_pft,1,sum)/npoly

  par(mfrow=c(3,1))
  plot(agb)
  plot(agb_poly[,1],ylim=range(agb_poly),type='n')
  for(i in 1:npoly) lines(agb_poly[,i],col=i)
  plot(agb_gpft[,1],ylim=range(agb_gpft),type='n')
  for(i in 1:npft) lines(agb_gpft[,i],col=i)
  legend(1,max(agb_gpft),1:npft,col=1:npft,lty=1,cex=0.5)




}

