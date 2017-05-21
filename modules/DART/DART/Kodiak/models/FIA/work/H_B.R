BObsOp <- function(){
     library(rhdf5)

     fname <- readLines(con="end_file.txt",n=1)

     PFT <- h5read(fname,'PFT')
     BA <- h5read(fname,'BALIVE')
     BL <- h5read(fname,'BLEAF')
     BS <- h5read(fname,'BSAPWOODA')
     BD <- h5read(fname,'BDEAD')
     B <- 0.7*BD + BL + BS

     P_C <- h5read(fname,'PACO_ID')
     A <- h5read(fname,'AREA')
     NP <- h5read(fname,'NPLANT')
     DBH <- h5read(fname,'DBH')

     C_P <- c(1:length(B))
     P_B <- c()
     P_BA <- c()
     P_BD <- c()
     P_BL <- c()
     P_BS <- c()
     P_N <- c()
     P_DBH <- c()
     P_count <- c()

     C_N <- matrix(0,nrow=max(PFT),ncol=length(C_P))     

     P_DBH <- matrix(0,nrow=max(PFT),ncol=length(P_C))     
     P_N <- matrix(0,nrow=max(PFT),ncol=length(P_C))     
     DBH_error <- matrix(0,nrow=max(PFT),ncol=length(P_C))     


     for(i in 1:(length(P_C)-1)){
      C_P[P_C[i]:(P_C[i+1]-1)] <- i
      P_count[i] <- length(C_P[P_C[i]:(P_C[i+1]-1)])

      T_DBH <- DBH[P_C[i]:(P_C[i+1]-1)]
      T_N <- NP[P_C[i]:(P_C[i+1]-1)]
      T_PFT <- PFT[P_C[i]:(P_C[i+1]-1)]

      for(j in 1:max(PFT)){
      	    a <- which(T_DBH > 5.)
	    b <- which(T_PFT == j)
	    c <- intersect(a,b)
	    
            F_DBH <- T_DBH[c]
      	    F_N <- T_N[c]

      	    P_N[j,i] <- sum(F_N)
      	    if(sum(F_N) >0) P_DBH[j,i] <- sum(F_DBH*F_N)/sum(F_N)

      	    DBH_error[j,i] <- sd(F_DBH)
	    }

      P_B[i] <- sum(B[P_C[i]:(P_C[i+1]-1)]*NP[P_C[i]:(P_C[i+1]-1)]*3.14*400)
      P_BA[i] <- sum(BA[P_C[i]:(P_C[i+1]-1)]*NP[P_C[i]:(P_C[i+1]-1)]*3.14*400)
      P_BD[i] <- sum(BD[P_C[i]:(P_C[i+1]-1)]*NP[P_C[i]:(P_C[i+1]-1)]*3.14*400)
      P_BL[i] <- sum(BL[P_C[i]:(P_C[i+1]-1)]*NP[P_C[i]:(P_C[i+1]-1)]*3.14*400)
      P_BS[i] <- sum(BS[P_C[i]:(P_C[i+1]-1)]*NP[P_C[i]:(P_C[i+1]-1)]*3.14*400)
     }

     C_P[P_C[length(P_C)]:length(C_P)] <- length(A)
     P_count[length(A)] <- length(C_P[P_C[length(P_C)]:length(C_P)])

     T_DBH <- DBH[P_C[length(P_C)]:length(C_P)]
     T_N <- NP[P_C[length(P_C)]:length(C_P)]
     T_PFT <- PFT[P_C[length(P_C)]:length(C_P)]

      for(j in 1:max(PFT)){
      	    a <- which(T_DBH > 5.)
	    b <- which(T_PFT == j)
	    c <- intersect(a,b)
	    
            F_DBH <- T_DBH[c]
      	    F_N <- T_N[c]

      	    P_N[j,length(P_C)] <- sum(F_N)
      	    if(sum(F_N) > 0) P_DBH[j,length(P_C)] <- sum(F_DBH*F_N)/sum(F_N)

      	    DBH_error[j,length(P_C)] <- sd(F_DBH)
	    }



     P_B[length(P_C)] <- sum(B[P_C[length(P_C)]:length(C_P)]*NP[P_C[length(P_C)]:length(C_P)]*3.14*400)
     P_BA[length(P_C)] <- sum(BA[P_C[length(P_C)]:length(C_P)]*NP[P_C[length(P_C)]:length(C_P)]*3.14*400)
     P_BD[length(P_C)] <- sum(BD[P_C[length(P_C)]:length(C_P)]*NP[P_C[length(P_C)]:length(C_P)]*3.14*400)
     P_BL[length(P_C)] <- sum(BL[P_C[length(P_C)]:length(C_P)]*NP[P_C[length(P_C)]:length(C_P)]*3.14*400)
     P_BS[length(P_C)] <- sum(BS[P_C[length(P_C)]:length(C_P)]*NP[P_C[length(P_C)]:length(C_P)]*3.14*400)

     N_vector <- as.vector(t(P_N[7:11,]))*3.14*400
     DBH_vector <- as.vector(t(P_DBH[7:11,]))

     state <- matrix(nrow=2,ncol=length(N_vector))
     state[1,] <- DBH_vector
     state[2,] <- N_vector

     print(sum(N_vector))

     write.table(state,"Routput.dat",row.names=FALSE,col.names=FALSE)
}
BObsOp()