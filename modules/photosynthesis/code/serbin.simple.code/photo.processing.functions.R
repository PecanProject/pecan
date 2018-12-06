#--------------------------------------------------------------------------------------------------#
# A set of helper functions and utils for LiCor GE data processing
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' QA/QC functions
##' @param data input licor data
##' @param out.dir output director for QC data and info
##' @param Cond.cutoff cutoff for low conductance. set previously
##' @param Ci.cutoff cutoff for nonsensical Cis
##' @param Tleaf.cutoff cutoff for individual Tleaf variation from mean
##' 
##' @author Shawn P. Serbin
##' 
data.qc <- function(data=NULL,out.dir=NULL,Cond.cutoff=NULL,Ci.cutoff=NULL,
                    Tleaf.cutoff=NULL){
  
  ### Remove samples not passing initial QC
  loc <- match("QC",toupper(names(data)))
  remove <- which(data[loc]==1)
  if(length(remove)>0){
    data <- data[-remove,]
  }
  rm(loc,remove)
  
  ### Remove QC and comments columns (if exists)
  pattern <- c("QC","COMMENTS")
  x <- toupper(names(data))
  remove <- match(pattern,x)
  if (length(remove)>0){
    data <- data[,-remove]
  }
  rm(pattern,x,remove)
  
  ### Find data columns
  pattern <- c("Tair","Tleaf","deltaT","RH_R","RH_S","PRESS","PARi","CO2Ref","PHOTO","COND","Ci")
  pattern <- toupper(pattern)
  x <- toupper(names(data))
  keep <- match(pattern,x)
  
  ### Extract sample info
  sample.info <- data[,-keep]
  data <- data[,keep]
  
  # Clean up sample info, remove any white spaces
  temp <- as.data.frame(lapply(sample.info,gsub,pattern=" ",replacement=""))
  sample.info <- temp
  rm(pattern,x,keep,temp)
  
  ### Apply QC filters to data
  dims <- dim(data)
  loc <- match(c("COND","CI"),toupper(names(data)))
  cond.check <- which(data[,loc[1]]<Cond.cutoff)
  ci.check <- which(data[,loc[2]]<Ci.cutoff[1] | data[,loc[2]]>Ci.cutoff[2])
  all.check <- which(data[,loc[2]]<Ci.cutoff[1] | data[,loc[2]]>Ci.cutoff[2] | data[,loc[1]]<Cond.cutoff)
  #nms <- unique(sample.info[check1,])
  nms1 <- sample.info[cond.check,]
  vals1 <- data[cond.check,loc[1]]
  nms2 <- sample.info[ci.check,]
  vals2 <- data[ci.check,loc[2]]
  
  # Tleaf check
  if (!is.null(Tleaf.cutoff)){
    temp.data <- data.frame(sample.info,data)
    # Create unique index for each sample group
    temp.data.index <- within(temp.data, indx <- as.numeric(interaction(sample.info, 
                                                            drop=TRUE,lex.order=TRUE)))
    #Mean.Tleaf <- aggregate(Tleaf~indx,data=temp.data.index,mean)
    #Stdev.Tleaf <- aggregate(Tleaf~indx,data=temp.data.index,sd)
    #names(Mean.Tleaf) <- c("indx","Mean.Tleaf")
    #names(Stdev.Tleaf) <- c("indx","Stdev.Tleaf")
    #aggregate(Tleaf~indx,data=temp.data.index, FUN = function(x) quantile(x, probs  = c(0.05,0.95)))
    stats <- data.frame(Mean.Tleaf=aggregate(Tleaf~indx,data=temp.data.index,mean),
                        Stdev.Tleaf=aggregate(Tleaf~indx,data=temp.data.index,sd),
                        CI05=aggregate(Tleaf~indx,data=temp.data.index, 
                                       FUN = function(x) quantile(x, probs  = 0.05)),
                        CI95=aggregate(Tleaf~indx,data=temp.data.index, 
                                       FUN = function(x) quantile(x, probs  = 0.95)))
    stats <- stats[,-c(3,5,7)]
    names(stats) <- c("indx","Mean.Tleaf","Stdev.Tleaf","L05.Tleaf","U95.Tleaf")
    #temp.data2 <- merge(temp.data.index,Mean.Tleaf,by="indx",sort=F) # Preserve original order
    temp.data2 <- merge(temp.data.index,stats,by="indx",sort=F) # Preserve original order
    loc <- match(c("INDX"),toupper(names(temp.data2)))
    temp.data2 <- temp.data2[,-loc[1]]
    loc <- match(c("TLEAF","MEAN.TLEAF"),toupper(names(temp.data2)))
    Tleaf.check <- which(temp.data2[,loc[1]] < temp.data2[,loc[2]]-Tleaf.cutoff | 
                           temp.data2[,loc[1]] > temp.data2[,loc[2]]+Tleaf.cutoff)
    nms3 <- sample.info[Tleaf.check,]
    vals3 <- temp.data2[Tleaf.check,loc[1]]
    vals4 <- temp.data2[Tleaf.check,loc[2]]
    temp3 <- data.frame(nms3,Mean.Tleaf=vals4,Tleaf=vals3)
    
    # update all check flags
    all.check <- unique(c(all.check,Tleaf.check))
  } # end if

  ### Output QA/QC info
  temp1 <- data.frame(nms1,Cond=vals1)
  temp2 <- data.frame(nms2,Ci=vals2)

  if (dim(temp1)[1]>0){
    write.csv(temp1,file=paste(out.dir,"/","Failed_QC_Conductance_Check.csv",sep=""),row.names=FALSE)
  }
  if (dim(temp2)[1]>0){
    write.csv(temp2,file=paste(out.dir,"/","Failed_QC_Ci_Cutoff_Check.csv",sep=""),row.names=FALSE)
  }
  if (dim(temp3)[1]>0){
    write.csv(temp3,file=paste(out.dir,"/","Failed_QC_Temperature_Cutoff_Check.csv",sep=""),row.names=FALSE)
  }
  
  ### Remove bad data
  if (length(all.check>0)){
    data <- data[-all.check,]
    sample.info <- sample.info[-all.check,]
  }
  row.names(data) <- seq(len=nrow(data))
  row.names(sample.info) <- seq(len=nrow(sample.info))
  rm(dims,loc,cond.check,ci.check,all.check,nms1,vals1,nms2,vals2,temp1,temp2,Ci.cutoff,Cond.cutoff)
  
  ### Return QC data and sample info
  return(list(Sample.Info=sample.info,GE.data=data))
  
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' Farquhar functions
##' 
##' @author Shawn P. Serbin
##' 
# Rd and Vcmax only
ACi.rubisco <- function(x) {
  keep <- which(Ci<Vcmax.cutoff)
  Vcmax <- x[1]  ## Vcmax param
  Rd    <- x[2]  ## Resp param
  
  Ac    <- (Vcmax*(Ci[keep]-Gstar[keep]))/(Ci[keep]+Km[keep])
  Assim <- pmin(Ac)-Rd
  RMSE <- sqrt(mean((Photo[keep]-Assim)^2))  ## RMSE cost function
  return(RMSE)
}

# Rd and Jmax only
ACi.rubp  <- function(x) {
  keep <- which(Ci>Jmax.cutoff)
  Jmax <- x[1]  ## Jmax param
  Rd   <- x[2]  ## Resp param
  
  Aj    <- Jmax*(Ci[keep]-Gstar[keep])/((4.5*Ci[keep])+(10.5*Gstar[keep]))
  Assim <- pmin(Aj)-Rd
  RMSE <- sqrt(mean((Photo[keep]-Assim)^2))  ## RMSE cost function
  return(RMSE)
}

# Vcmax, Rd, and Jmax
ACi.full <- function(x) {
  Vcmax <- x[1]  ## Vcmax param
  Rd    <- x[2]  ## Resp param
  Jmax  <- x[3]  ## Jmax
  
  Ac <- ifelse(Ci<=Vcmax.cutoff,(Vcmax*(Ci-Gstar)/(Ci + Km))-Rd,9999)
  inter <- ifelse((Ci>Vcmax.cutoff & Ci<Jmax.cutoff),Photo,9999)
  Aj <- ifelse(Ci>=Jmax.cutoff,(Jmax*(Ci-Gstar)/((4.5*Ci)+(10.5*Gstar)))-Rd,9999)
  #Aj <- ifelse(Ci>=Jmax.cutoff,(Jmax*(Ci-Gstar)/((4.5*Ci)+(10.5*Gstar))),9999)
  Assim <- pmin(Ac,Aj,inter)
  RMSE <- sqrt(mean((Photo-Assim)^2))  ## RMSE cost function
  return(RMSE)
}

# A-PPFD parameters
A.Q <- function(x) {
  print("Light Response Curve Analysis Not yet implemented")
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'
##' GE Diagnostic plots 
##' @param type which type of data to invert. A-Ci or A-Q
##' @param data A-Ci data used for model fit
##' @param DEoptim.output DEoptim output structure
##' @param params 
##' @param outdir output directory for A-Ci diagnostic figures
##' @param file output filename
##' @param f.model full, vcmax, or jmax
##' 
##' @author Shawn P. Serbin
##' 
plot.ge.fit <- function(type=c("A-Ci","A-Q"),data,DEoptim.output,params,outdir,file,f.model){
  type <- match.arg(type)
  sep <- .Platform$file.sep
  param.length <- length(DEoptim.output$optim$bestmem)

  ### Plot params
  cexaxis <- 1.2
  cexlab <- 1.4
  
  if (type=="A-Ci"){
    loc1 <- match(c("CI","PHOTO"),toupper(names(data)))
    
    # Figures
    pdf(paste(outdir,sep,file,".pdf",sep=""),height=8,width=10)
    plot(data[,loc1[1]],data[,loc1[2]],pch=21,bg="grey70",cex=3,cex.axis=cexaxis,xlim=c(0,range(data[,loc1[1]])[2]),
         ylim=c(0,range(data[,loc1[2]])[2]),cex.lab=cexlab,xlab="Ci",ylab="Photo",main=paste(file))
    box(lwd=2.2)
    
    Oxygen <- mm.constants$Oxygen
    loc2 <- match(c("KC","KO","GSTAR","TLEAF"),toupper(names(data)))
    Kc <- mean(data[,loc2[1]])
    Ko <- mean(data[,loc2[2]])
    Gstar <- mean(data[,loc2[3]])
    
    plot.x <- (data[,loc1[1]]-Gstar)/(data[,loc1[1]]+(Kc*(1+Oxygen/Ko)))
    plot(plot.x,data[,loc1[2]],pch=21,bg="grey70",cex=3,cex.axis=cexaxis,xlim=c(0,range(plot.x)[2]),
         ylim=c(0,range(data[,loc1[2]])[2]),cex.lab=cexlab,xlab="Ci-Gstar/Ci+Km",ylab="Photo",main=paste(file))
    box(lwd=2.2)

    # DEoptim trace plots
    if (f.model==1){
      par(mfrow=c(3,1),mar=c(4,4.1,1,2)) #b, l, t, r
      plot(DEoptim.output$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Vcmax",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,1],lty=2,lwd=1.8)
      box(lwd=2.2)
      plot(DEoptim.output$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Rd",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,2],lty=2,lwd=1.8)
      box(lwd=2.2)
      plot(DEoptim.output$member$bestmemit[,3],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Jmax",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,3],lty=2,lwd=1.8)
      box(lwd=2.2)
    } else if (f.model==2){
      par(mfrow=c(3,1),mar=c(4,4.1,1,2)) #b, l, t, r
      plot(DEoptim.output$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Vcmax",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,1],lty=2,lwd=1.8)
      box(lwd=2.2)
      plot(DEoptim.output$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Rd",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,2],lty=2,lwd=1.8)
      box(lwd=2.2)
    } else if (f.model==3){
      par(mfrow=c(3,1),mar=c(4,4.1,1,2)) #b, l, t, r
      plot(DEoptim.output$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Jmax",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,1],lty=2,lwd=1.8)
      box(lwd=2.2)
      plot(DEoptim.output$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
           cex=1.2,xlab="Iteration",ylab="Rd",cex.axis=cexaxis,cex.lab=cexlab)
      lines(DEoptim.output$member$bestmemit[,2],lty=2,lwd=1.8)
      box(lwd=2.2)
    }

    # A-Ci diagnostic fit fig
    plotCi = seq(-2,1800,2)
    Oxygen <- mm.constants$Oxygen
    loc2 <- match(c("KC","KO","GSTAR","TLEAF"),toupper(names(data)))
    Kc <- mean(data[,loc2[1]])
    Ko <- mean(data[,loc2[2]])
    Gstar <- mean(data[,loc2[3]])
    Tleaf <- mean(data[,loc2[4]])
    Vcmax.plot <- params[[1]]
    Jmax.plot <- params[[2]]
    Rd.plot <-params[[3]]
    RMSE.plot <- params[[4]]
    
    par(mfrow=c(1,1),mar=c(5,5,2,1)) # bot, left
    ylim <- range(data[,loc1[2]])
    plot(data[,loc1[1]],data[,loc1[2]], main=paste(file), xlab="Ci", ylab="Photo", cex.lab=2,cex=1.8,
         xlim=c(1.1,1500),ylim=c(0,ylim[2]+3))
    legend("bottomright",legend=c(paste("Tleaf =",round(Tleaf,2)),paste("Vcmax =",round(Vcmax.plot,2)),
                                  paste("Jmax = ",round(Jmax.plot,2)),paste("Rd = ",round(Rd.plot,4)),
                                  paste("RMSE = ",round(RMSE.plot,2))),
           bty="n",cex=2)
    legend("topleft",legend=c("Rubisco","RuBP","Photo"),lty=c(2,2,1),
           col=c("dark blue","dark red","dark grey"),bty="n",lwd=6.3,seg.len=3.5,cex=1.5)
    if (Vcmax.plot!=-9999){
      lines(plotCi,(Vcmax.plot*(plotCi-Gstar)/(plotCi+(Kc*(1+Oxygen/Ko))))-Rd.plot,lwd=5,col="dark blue",lty=2)
    }
    if (Jmax.plot!=-9999){
      lines(plotCi,((Jmax.plot*(plotCi-Gstar))/((4.5*plotCi)+(10.5*Gstar)))-Rd.plot,lwd=5,col="dark red",lty=2)
    }
    if (Vcmax.plot!=-9999 & Jmax.plot!=-9999){
      lines(plotCi,pmin(Vcmax.plot*(plotCi-Gstar)/(plotCi+(Kc*(1+Oxygen/Ko))),
                        (Jmax.plot*(plotCi-Gstar))/((4.5*plotCi)+(10.5*Gstar)))-Rd.plot,col="dark grey",lwd=2.0)
    }
    box(lwd=2.2)
    dev.off()
    
  } else if (type=="A-Q"){
    print("Not Yet Implemented")
  } # End A-Ci / A-Q if/else

  
} # End of function
#--------------------------------------------------------------------------------------------------#