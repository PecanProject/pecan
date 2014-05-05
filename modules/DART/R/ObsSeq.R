ObsSeq <- function(m_file,f_file,cut_day){
       library(ncdf)
       modis <- open.ncdf(m_file)
       x_m <- get.var.ncdf(modis,"LAI")
       d_m <- get.var.ncdf(modis,"Dates")
#       t_m <- seq(5,365,8)
       t_m <- as.numeric(substr(as.character(d_m),5,7))+4
       e_m <- rep(0.66,length(t_m))

       flux <- read.csv(f_file)
       P2 <- flux$par_2
       P30 <- flux$par_30
       x2 <- P2[1:17280]
       x30 <- P30[1:17280]
       x2[x2 < 0.1] <- NA
       x30[x30 < 200.] <- NA
       fPAR <- x2/x30
       efPAR <- sqrt(0.02)*fPAR
       lefPAR <- efPAR/fPAR

       period <- rep(1:45, each=48*8)
#       period <- rep(1:90, each=48*4)
       x_f <- tapply(-log(fPAR)/0.52-1.9,period,mean,na.rm=TRUE)
       e_f <- rep(0.95,45)
       t_f <- seq(5,361,8)
#       e_f <- rep(0.95,90)
#       t_f <- seq(5,361,4)
       
       x <- c(x_m,x_f)
       e <- c(e_m,e_f)
       t <- c(t_m,t_f)

       k <- rep(-1,length(x))
       l <- rep(0.,length(x))

       obs <- data.frame(Obs=x,Time=t,Kind=k,Error=e,Loc=l)
       WriteObsSeq(obs,cut_day)
       write.csv(obs,'obs.dat')
}

WriteObsSeq <- function(obs.file,day){
       obs.file$Obs[obs.file$Time < day] <- NA
       obs.file$Obs[obs.file$Obs < 0.] <- NA
       obs.file <- obs.file[complete.cases(obs.file),]

       o <- order(obs.file$Time)
       x <- obs.file$Obs[o]
       loc <- obs.file$Loc[o]
       kind <- obs.file$Kind[o]
       t <- obs.file$Time[o]
       e <- obs.file$Error[o]     

       num_c <- 1
       num_qc <- 0

       write('obs_sequence',file='obs_seq.out')
       write('obs_kind_definitions',file='obs_seq.out',sep="\n",append=TRUE)
       write('		1',file='obs_seq.out',sep="\n",append=TRUE)
       write('		1 LAI',file='obs_seq.out',sep="\n",append=TRUE)
       write(paste('num_copies:		',num_c,'num_qc:	',num_qc),file='obs_seq.out',sep="\n",append=TRUE)
       write(paste('num_obs:		',length(x),'max_num_obs:	',length(x)),file='obs_seq.out',sep="\n",append=TRUE)
       write('observations',file='obs_seq.out',sep="\n",append=TRUE)
       write(paste('  first:	1	last:	', length(x)), file='obs_seq.out', sep="\n", append=TRUE)
       for(j in 1:length(x)){
      	     write(paste(' OBS	', j),file='obs_seq.out',sep="\n",append=TRUE)
	     write(x[j],file='obs_seq.out',sep="\n",append=TRUE)

	     pl = c(j-1,j+1,-1)
	     pl[pl < 1] = -1
	     pl[pl > length(x)] = -1

	     write(pl,file='obs_seq.out', sep="		",append=TRUE)
	     write('obdef',file='obs_seq.out',sep="\n",append=TRUE)
	     write('loc1d',file='obs_seq.out',sep="\n",append=TRUE)
	     write(loc[j],file='obs_seq.out',sep="\n",append=TRUE)
	     write('kind',file='obs_seq.out',sep="\n",append=TRUE)
	     write(kind[j],file='obs_seq.out',sep="\n",append=TRUE)

	     write(paste('   0		',t[j]),file='obs_seq.out',sep="\n",append=TRUE)
	     write(e[j],file='obs_seq.out',sep="\n",append=TRUE)	     
	     }
}

