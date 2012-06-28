
plot.monthly <- function(dat){

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

  par(mfrow=c(2,1))
#  plot(agb)
  plot(agb_poly[,1],ylim=range(agb_poly),type='n')
  for(i in 1:npoly) lines(agb_poly[,i],col=i)
  plot(agb_gpft[,1],ylim=range(agb_gpft),type='n')
  for(i in 1:npft) lines(agb_gpft[,i],col=i)
  legend(1,max(agb_gpft),1:npft,col=1:npft,lty=1,cex=0.5)
  #legend(1,1,1:npft,col=1:npft,lty=1,cex=.8)

X11()
### LAI
  lai_pft <- array(0,dim=c(nmonth,npoly,npft))
  for(i in 1:nmonth){
    for(j in 1:npoly){
      py0 <- dat$PYSI_ID[[i]][j]       ##beginning of polygon
      pyF <- length(dat$LAI_CO[[i]])   ##end of polygon
      if(j < npoly){
        pyF <- dat$PYSI_ID[[i]][j+1]-1
      }
      sites <- which(dat$SIPA_ID[[i]] %in% py0:pyF) ## list of site beginnings
      nsites <- length(sites)
      for(k in sites){
        As <- dat$AREA_SI[[i]][k]  ## site area
        s0 <- dat$SIPA_ID[[i]][k]  ## beginning of site
        sF <- length(dat$LAI_CO[[i]])   ##end of polygon
        if(k < sites[nsites]){
          sF <- dat$SIPA_ID[[i]][k+1]-1
        } 
        patches <- which(dat$PACO_ID[[i]] %in% s0:sF) ## list of site beginnings
        npatches <- length(patches)
        for(l in patches){
          Ap <- dat$AREA[[i]][l]
          pa0 <- dat$PACO_ID[[i]][l]
          paF <- length(dat$LAI_CO[[i]])   ##end of polygon
          if(l < patches[npatches]){
            paF <- dat$PACO_ID[[i]][l+1]-1
          } 
          lai <- tapply(dat$LAI_CO[[i]][pa0:paF],dat$PFT[[i]][pa0:paF],sum,na.rm=TRUE)*As*Ap
          lai_pft[i,j,as.numeric(names(lai))] <- lai_pft[i,j,as.numeric(names(lai))] + lai          
        } ## patches
      } ## sites
    } ## polygons
  } ## months

  lai_poly <- matrix(NA,nmonth,npoly)
  lai_gpft <- matrix(NA,nmonth,npft)
  for(i in 1:nmonth){
    for(j in 1:npoly){
      lai_poly[i,j] <- sum(lai_pft[i,j,])
    }
    for(j in 1:npft){
      lai_gpft[i,j] <- sum(lai_pft[i,,j])
    }
  }  
  lai <- apply(lai_pft,1,sum)/npoly
## Creates plots for LAI
  par(mfrow=c(2,1))
#  plot(lai)
  plot(lai_poly[,1],ylim=range(lai_poly),type='n')
  for(i in 1:npoly) lines(lai_poly[,i],col=i)
  plot(lai_gpft[,1],ylim=range(lai_gpft),type='n')
  for(i in 1:npft) lines(lai_gpft[,i],col=i)
  legend(1,max(lai_gpft),1:npft,col=1:npft,lty=1,cex=0.5)

## Creates plots for GPP, Ra, Rh, and NEP, 
X11()
par(mfrow=c(4,1))
  ##GPP
  gpp <- rep(NA,nmonth)
  for(i in 1:nmonth){
    gpp[i] <- dat$MMEAN_GPP[[i]]
  }
  plot(gpp,type='l',ylim=range(gpp))

  ##Ra
  
  Rl <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Rl[i] <- dat$MMEAN_LEAF_RESP[[i]]
  }
  Rr <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Rr[i] <- dat$MMEAN_ROOT_RESP[[i]]
  }
  Rstorage <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Rstorage[i] <- dat$MMEAN_STORAGE_RESP[[i]]
  }
  Rgrowth <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Rgrowth[i] <- dat$MMEAN_GROWTH_RESP[[i]]
  }
  
  Ra <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Ra[i] <- dat$MMEAN_LEAF_RESP[[i]] + dat$MMEAN_ROOT_RESP[[i]] + dat$MMEAN_STORAGE_RESP[[i]] + dat$MMEAN_GROWTH_RESP[[i]]
  }
  plot(Ra,type='l',ylim=range(Ra))	

  ## Rh

  Rh <- rep(NA,nmonth)
  for(i in 1:nmonth){
    Rh[i] <- dat$MMEAN_RH[[i]]
  }
  plot(Rh,type='l',ylim=range(Rh))

  ##NEP
  nep <- rep(NA,nmonth)
  for(i in 1:nmonth){
    nep[i] <- dat$MMEAN_NEP[[i]]
  }
  plot(nep,type='l',ylim=range(nep))
  abline(a=0,b=0)


## Creates graphs for the components of Ra
X11()
par(mfrow=c(4,1))
plot(Rl,type='l',ylim=range(Rl))
plot(Rr,type='l',ylim=range(Rr))
plot(Rstorage,type='l',ylim=range(Rstorage))
plot(Rgrowth,type='l',ylim=range(Rgrowth))

}
