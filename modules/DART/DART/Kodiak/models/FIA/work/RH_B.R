BRevOp <- function(){
    library(rhdf5)

    fname <- readLines(con="file_name.txt",n=1)

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

    PV <- read.table('4Rvalues.dat')
    F_DBH <- t(matrix(unlist(PV[1,]),nrow=13,ncol=5))
    F_N <- t(matrix(unlist(PV[2,]),nrow=13,ncol=5))/(3.14*400)

    d_dbh <- F_DBH - P_DBH[7:11,]
    d_N <- F_N - P_N[7:11,]

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

    for(i in 1:length(DBH)){
     if(DBH[i] >= 5.){
      DBH[i] <- DBH[i] + d_dbh[PFT[i]-6,C_P[i]]
      NP[i] <- NP[i] + d_N[PFT[i]-6,C_P[i]]/C_Count[i]

      if(NP[i] < 0.) NP[i] <- B_NP[i]
      if(DBH[i] < 0) DBH[i] <- B_DBH[i]
      }

      b1Bl[i] <- P2B$b1Bl[which(P2B$PFT == PFT[i])]
      b2Bl[i] <- P2B$b2Bl[which(P2B$PFT == PFT[i])]
      b1Bs[i] <- P2B$b1Bs[which(P2B$PFT == PFT[i])]
      b2Bs[i] <- P2B$b2Bs[which(P2B$PFT == PFT[i])]
      b1Ht[i] <- P2B$b1Ht[which(P2B$PFT == PFT[i])]
      b2Ht[i] <- P2B$b2Ht[which(P2B$PFT == PFT[i])]
      qsw[i] <- P2B$qsw[which(P2B$PFT == PFT[i])]
    }

    NP[NP < 0] <- 0.0001
    DBH[DBH < 0.] <- 3

    #Dead biomass [kg/plant]
    Bd <- b1Bs/C2B*DBH**b2Bs

    #Leaf biomass [kg/plant]
    Bl <- b1Bl/C2B*DBH**b2Bl

    #Approximate height from dbh
    ht <- hgt_ref + b1Ht*(1.0 - exp(b2Ht*DBH))

    #Sapwood biomass [kg/plant]
    Bsw <- qsw*ht*Bl

    #Above-ground biomass [kg/plant]
    Ba <- agf_bs*Bd + Bl + Bsw

    Br <- h5read(fname,'BROOT')
    Bswb <- h5read(fname,'BSAPWOODB')

    Balive <- Bl + Bsw + Br + Bswb

    h5write(DBH,fname,'DBH')
    h5write(NP,fname,'NPLANT')
    h5write(Bd,fname,'BDEAD')
    h5write(Bl,fname,'BLEAF')
    h5write(Bsw,fname,'BSAPWOODA')
    h5write(Balive,fname,'BALIVE')
    
}
BRevOp()