## A function to estimate PAR if there is no information about it.
## By Francesco Minunno, 22.4.2014
## Modified by Mikko Peltoniemi 23.4.2014 (made random term optional)

dPAR <- function(LAT,DOY,VPD, radmodel=1){
            pf1 <- c(2.46051603,-0.06927806)
            pf2 <- c(2.547053e+03, -3.245261e-03, 2.549779e+03)
            moments_un <- c(-0.08096889, 6.08357932, -2.23500725, 9.68680891)
            moments_ov <- c(-0.4825768, 8.5180209, -0.7531573, 4.1647394)
            fconv <- 2.0513 #conversion factor
            f1 <- rep(NA,length(DOY))
            eps <- rep(NA,length(DOY))
            hourly <- rep(NA,24)
            for(j in 1:length(DOY)){
                for(i in 1:24) hourly[i] <- elev(LAT,DOY[j],i,pf1[1],pf1[2])$RADI
                f1[j] <- sum(hourly) * fconv
            }
            f2 <- pf2[1]/(pf2[2]*VPD +1) - pf2[3]
            ind1 <- which(VPD < 0.1 & f1>=5)
            ind2 <- which(VPD >= 0.1& f1>=5)
            ind3 <- which(VPD < 0.1 & f1<5)
            ind4 <- which(VPD >= 0.1& f1<5)

            if (radmodel==1) {
                if(length(ind1)>0) eps[ind1] <- rnorm(length(VPD[ind1]),mean=moments_un[1],sd=moments_un[2])
                if(length(ind2)>0) eps[ind2] <- rnorm(length(VPD[ind2]),mean=moments_ov[1],sd=moments_ov[2])
                if(length(ind3)>0) eps[ind3] <- rnorm(length(VPD[ind3]),mean=moments_un[1],sd=(moments_un[2]/2))
                if(length(ind4)>0) eps[ind4] <- rnorm(length(VPD[ind4]),mean=moments_ov[1],sd=(moments_ov[2]/2))
            }

            if (radmodel == 2) { ## Mikko: note that taking random term fixed may cause different GPP response
                                        # in photosynthesis model, as effect of PAR is non-linear.

                if(length(ind1)>0) eps[ind1] <- rep(moments_un[1],length(VPD[ind1]))
                if(length(ind2)>0) eps[ind2] <- rep(moments_ov[1], length(VPD[ind2]))
                if(length(ind3)>0) eps[ind3] <- rep(moments_un[1],length(VPD[ind3]))
                if(length(ind4)>0) eps[ind4] <- rep(moments_ov[1], length(VPD[ind4]))
            }
            
            PAR <- f1 + f2 + eps
            PAR[which(PAR<0)] <- 0
            return(PAR)
}
