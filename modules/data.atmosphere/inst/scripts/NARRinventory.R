## code to find and download missing NARR files

##functions
num <- function(x){as.numeric(as.character(x))}
mchar <- c("01","02","03","04","05","06","07","08","09","10","11","12")
mlennorm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
mlenleap <- c(31,29,31,30,31,30,31,31,30,31,30,31)
mlen <- function(m,y){
  if(y%%4 == 0){ ##leap year
    return(mlenleap[m])
  }
  return(mlennorm[m])  
}

files    <- dir(pattern="tar")
flxfiles <- files[grep("flx",files)]
sfcfiles <- files[grep("sfc",files)]

# authenticate
system('wget --no-check-certificate -O /dev/null --save-cookies auth.dss_ucar_edu --post-data="email=mdietze@illinois.edu&passwd=fromgood&action=login" https://dss.ucar.edu/cgi-bin/login')


## process FLX files
yr <- num(substr(flxfiles,9,12))
mo <- num(substr(flxfiles,13,14))
t0 <- num(substr(flxfiles,16,17))
tf <- num(substr(flxfiles,18,19))

for(y in 2008:2009){
  ycode <- ""
#  if(y %in% 2003:2005) ycode <- "rerun"
  ysel <- which(yr == y)
  if(length(ysel) < 48){
    for(m in 1:12){
      msel <- which(mo[ysel] == m)
      if(length(msel) < 4){
        if(!(1 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRflx_",y,mchar[m],"_0108.tar",sep=""))
        }
        if(!(9 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRflx_",y,mchar[m],"_0916.tar",sep=""))
        }
        if(!(17 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu http://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRflx_",y,mchar[m],"_1724.tar",sep=""))
        }
        if(!(25 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRflx_",y,mchar[m],"_25",mlen(m,y),".tar",sep=""))
        }        
      }
    }
  }
}


## PROCESS SFC FILES
yr <- num(substr(sfcfiles,9,12))
mo <- num(substr(sfcfiles,13,14))
t0 <- num(substr(sfcfiles,16,17))
tf <- num(substr(sfcfiles,18,19))

for(y in 1979:2009){
  ycode <- ""
#  if(y %in% 2003:2005) ycode <- "rerun"
  ysel <- which(yr == y)
  if(length(ysel) < 36){
    for(m in 1:12){
      msel <- which(mo[ysel] == m)
      if(length(msel) < 3){
        if(!(1 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRsfc_",y,mchar[m],"_0109.tar",sep=""))
        }
        if(!(10 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRsfc_",y,mchar[m],"_1019.tar",sep=""))
        }
        if(!(20 %in% t0[msel])){
          system(paste("wget --no-check-certificate -N --load-cookies auth.dss_ucar_edu https://dss.ucar.edu/datazone/dsszone/ds608.0/NARR/3HRLY_TAR/",y,mchar[m],ycode,"/NARRsfc_",y,mchar[m],"_20",mlen(m,y),".tar",sep=""))
        }        
      }
    }
  }
}


# clean up
system("rm auth.dss_ucar_edu")
