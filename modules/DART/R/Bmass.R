Bmass <- function(){

library(rhdf5)

con <- file('obs_seq.final')
open(con)

read.table(con,skip=12,nrow=1)

obs <- c()
pr <- c()
po <- c()
spr <- c()
spo <- c()
year <- c()
k <- c()

for(i in 1:390){
      read.table(con,nrow=1)[,1]
      obs[i] <- read.table(con,nrow=1)[,1]
      pr[i] <- read.table(con,nrow=1)[,1]
      po[i] <- read.table(con,nrow=1)[,1]
      
      spr[i] <- read.table(con,nrow=1)[,1]
      spo[i] <- read.table(con,nrow=1)[,1]

      read.table(con,skip=6,nrow=1)

      k[i] <- -read.table(con,nrow=1)[,1]

      year[i] <- read.table(con,nrow=1)[,1]

      read.table(con,nrow=1)
      }

close(con)

    y <- 2010

    y_k <- k[year == y]

    y_obs <- obs[year == y]    

    y_pr <- pr[year == y]
    y_po <- po[year == y]

    OBS_V <- matrix(0,nrow=2,ncol=65)

    PR_V <- matrix(0,nrow=2,ncol=65)
    PO_V <- matrix(0,nrow=2,ncol=65)

    for(i in 1:65){
     OBS_V[1,i] <- y_obs[y_k == i]
     PR_V[1,i] <- y_pr[y_k == i]
     PO_V[1,i] <- y_po[y_k == i]

     OBS_V[2,i] <- y_obs[y_k == i+65]
     PR_V[2,i] <- y_pr[y_k == i+65]
     PO_V[2,i] <- y_po[y_k == i+65]
     }

    OBS_DBH <- t(matrix(OBS_V[1,],nrow=13,ncol=5))
    OBS_N <- t(matrix(OBS_V[2,],nrow=13,ncol=5))

    FR_DBH <- t(matrix(PR_V[1,],nrow=13,ncol=5))
    FR_N <- t(matrix(PR_V[2,],nrow=13,ncol=5))

    FO_DBH <- t(matrix(PO_V[1,],nrow=13,ncol=5))
    FO_N <- t(matrix(PO_V[2,],nrow=13,ncol=5))

    write.csv(FR_N,'Pr_FC.csv',row.names=FALSE)
    write.csv(FO_N,'Po_FC.csv',row.names=FALSE)
    write.csv(OBS_N,'Obs_FC.csv',row.names=FALSE)

    OBS_N <- OBS_N/(3.14*400)
    FR_N <- FR_N/(3.14*400)
    FO_N <- FO_N/(3.14*400)

    fname <- paste('duke-2k-S-',y,'-06-01-000000-g01.h5',sep="")

    PFT <- h5read(fname,'PFT')
    DBH <- h5read(fname,'DBH')
    NP <- h5read(fname,'NPLANT')
    P_C <- h5read(fname,'PACO_ID')

    C_P <- vector()

    C_Count <- rep(1,length(DBH))

    P_DBH <- matrix(0,nrow=max(PFT),ncol=length(P_C))     
    P_N <- matrix(0,nrow=max(PFT),ncol=length(P_C))     

    for(i in 1:(length(P_C)-1)){
      C_P[P_C[i]:(P_C[i+1]-1)] <- i

      T_DBH <- DBH[P_C[i]:(P_C[i+1]-1)]
      T_N <- NP[P_C[i]:(P_C[i+1]-1)]
      T_PFT <- PFT[P_C[i]:(P_C[i+1]-1)]

      for(j in 1:max(PFT)){
      	    a <- which(T_DBH > 5.)
	    b <- which(T_PFT == j)
	    c <- intersect(a,b)

	    C_Count[P_C[i]+c-1] <- length(c)
	    
            F_DBH <- T_DBH[c]
      	    F_N <- T_N[c]

      	    P_N[j,i] <- sum(F_N)
      	    if(sum(F_N) >0) P_DBH[j,i] <- sum(F_DBH*F_N)/sum(F_N)
	    }
     }

     C_P[P_C[length(P_C)]:length(DBH)] <- length(P_C)

     T_DBH <- DBH[P_C[length(P_C)]:length(C_P)]
     T_N <- NP[P_C[length(P_C)]:length(C_P)]
     T_PFT <- PFT[P_C[length(P_C)]:length(C_P)]

     for(j in 1:max(PFT)){
      	    a <- which(T_DBH > 5.)
	    b <- which(T_PFT == j)
	    c <- intersect(a,b)

	    C_Count[P_C[length(P_C)]+c-1] <- length(c)
	    
            F_DBH <- T_DBH[c]
      	    F_N <- T_N[c]

      	    P_N[j,length(P_C)] <- sum(F_N)
      	    if(sum(F_N) > 0) P_DBH[j,length(P_C)] <- sum(F_DBH*F_N)/sum(F_N)
	    }

    dr_dbh <- FR_DBH - P_DBH[7:11,]
    dr_N <- FR_N - P_N[7:11,]

    do_dbh <- FO_DBH - P_DBH[7:11,]
    do_N <- FO_N - P_N[7:11,]

    B_DBH <- DBH
    B_NP <- NP

    P2B <- read.csv('P2B.csv')

    C2B <- 2.
    agf_bs <- 0.7
    hgt_ref <- 1.3

    b1Bl <- vector()
    b2Bl <- vector()
    b1Bs <- vector()
    b2Bs <- vector()
    b1Ht <- vector()
    b2Ht <- vector()
    qsw <- vector()

    PR_DBH <- DBH
    PR_NP <- NP

    PO_DBH <- DBH
    PO_NP <- NP

    for(i in 1:length(DBH)){
     if(DBH[i] >= 5.){
      PR_DBH[i] <- DBH[i] + dr_dbh[PFT[i]-6,C_P[i]]
      PR_NP[i] <- NP[i] + dr_N[PFT[i]-6,C_P[i]]/C_Count[i]

      if(PR_NP[i] < 0) PR_NP[i] <- B_NP[i]
      if(PR_DBH[i] < 0) PR_DBH[i] <- B_DBH[i]

      PO_DBH[i] <- DBH[i] + do_dbh[PFT[i]-6,C_P[i]]
      PO_NP[i] <- NP[i] + do_N[PFT[i]-6,C_P[i]]/C_Count[i]

      if(PO_NP[i] < 0) PO_NP[i] <- B_NP[i]
      if(PO_DBH[i] < 0) PR_DBH[i] <- B_DBH[i]

      }
      b1Bl[i] <- P2B$b1Bl[which(P2B$PFT == PFT[i])]
      b2Bl[i] <- P2B$b2Bl[which(P2B$PFT == PFT[i])]
      b1Bs[i] <- P2B$b1Bs[which(P2B$PFT == PFT[i])]
      b2Bs[i] <- P2B$b2Bs[which(P2B$PFT == PFT[i])]
      b1Ht[i] <- P2B$b1Ht[which(P2B$PFT == PFT[i])]
      b2Ht[i] <- P2B$b2Ht[which(P2B$PFT == PFT[i])]
      qsw[i] <- P2B$qsw[which(P2B$PFT == PFT[i])]
    }

    #Dead biomass [kg/plant]
    Pr_Bd <- b1Bs/C2B*PR_DBH**b2Bs
    Po_Bd <- b1Bs/C2B*PO_DBH**b2Bs

    #Leaf biomass [kg/plant]
    Pr_Bl <- b1Bl/C2B*PR_DBH**b2Bl
    Po_Bl <- b1Bl/C2B*PO_DBH**b2Bl

    Pr_Bl <- 0.7*Pr_Bl
    Po_Bl <- 0.7*Po_Bl

    #Approximate height from dbh
    Pr_ht <- hgt_ref + b1Ht*(1.0 - exp(b2Ht*PR_DBH))
    Po_ht <- hgt_ref + b1Ht*(1.0 - exp(b2Ht*PO_DBH))

    #Sapwood biomass [kg/plant]
    Pr_Bsw <- qsw*Pr_ht*Pr_Bl
    Po_Bsw <- qsw*Po_ht*Po_Bl

    #Above-ground biomass [kg/plant]
    Pr_Ba <- agf_bs*Pr_Bd + Pr_Bl + Pr_Bsw
    Po_Ba <- agf_bs*Po_Bd + Po_Bl + Po_Bsw

    Pr_B <- rep(0,length=length(P_C))
    Po_B <- rep(0,length=length(P_C))

    for(i in 1:(length(P_C)-1)){
     for(j in P_C[i]:(P_C[i+1]-1)){
       if(PR_DBH[j] > 5.) Pr_B[i] <- Pr_B[i] + Pr_Ba[j]*PR_NP[j]*3.14*400
       if(PO_DBH[j] > 5.) Po_B[i] <- Po_B[i] + Po_Ba[j]*PO_NP[j]*3.14*400
     }
    }

    for(j in P_C[length(P_C)]:length(DBH)){
       if(PR_DBH[j] > 5.) Pr_B[length(Pr_B)] <- Pr_B[length(Pr_B)] + Pr_Ba[j]*PR_NP[j]*3.14*400
       if(PO_DBH[j] > 5.) Po_B[length(Po_B)] <- Po_B[length(Po_B)] + Po_Ba[j]*PO_NP[j]*3.14*400
}

    B <- cbind(Pr_B,Po_B)

#    print(FR_DBH)
#    print(FO_DBH)

    write.csv(B,'est_B.csv',row.names=FALSE)
}

Bmass()