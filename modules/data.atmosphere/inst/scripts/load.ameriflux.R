## general code to load Ameriflux/Fluxnet "LaThuille" files
##

load.Ameriflux <- function(path,level=4,freq="h"){
  
  post <- "3.txt"
  if(level == 4){post <- paste(level,"_",freq,".txt",sep="")}
  files <- dir(path,pattern=post,full.names=TRUE)

  dat <- list()
  for( i in 1:length(files)){
    dat.tmp <- read.csv(files[i],header=TRUE)
    dat.tmp[dat.tmp <= -9999.0] <- NA
    if(length(dat) == 0){
      for(j in 1:ncol(dat.tmp)){
        dat[[j]] <- dat.tmp[,j]
      }
      names(dat) <- names(dat.tmp)
    } else {
      for(j in 1:ncol(dat.tmp)){
        dat[[j]] <- c(dat[[j]],dat.tmp[,j])
      }
    }
  }
  dat
}


## example:
## flux <- load.Ameriflux("~/inputs/fluxnet/morgan_monroe/")
##
## access values based on list notation or by name
## e.g. plot(flux[[23]]) or plot(flux$NEE_st_fMDS)

  
