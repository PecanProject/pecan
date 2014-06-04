##shaded confidence interval
ciEnvelope <- function(x,ylo,yhi,col="lightgrey",...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), col=col, border = NA,...) 
} 

plot.photo <- function(dat,out,curve=c("ACi","AQ")){
  
  params  = as.matrix(out$params)
  predict = as.matrix(out$predict)
  CI <- apply(predict,2,quantile,c(0.025,0.5,0.975))
  pmean = CI[,grep("pmean",colnames(CI))]
  pA    = CI[,grep("pA",colnames(CI))]
  
  if("aci" %in% tolower(curve)){
    
    ## filter out A-Q curve points
    ref = estimate_mode(dat$PARi); if(ref < 1) return(NULL)
    sel = which(abs(dat$PARi-ref)/ref < tol)
    
    if(length(sel)>0){
      sel <- sel[order(dat$Ci[sel])]
    
      ## reference plot
      plot(dat$Ci[sel],dat$Photo[sel],pch=20,cex=1.5,
         xlab="Ci (ppm)",ylab="An (umol m-2 s-1)",main=dat$fname[1],ylim=range(pmean))
      ciEnvelope(dat$Ci[sel],pA[1,sel],pA[3,sel],col="grey90") #plot PI      
      ciEnvelope(dat$Ci[sel],pmean[1,sel],pmean[3,sel],col="grey60") #plot CI      
      lines(dat$Ci[sel],pmean[2,sel],col=2,lwd=3)  #model line
      points(dat$Ci[sel],dat$Photo[sel],pch=20,cex=1.5)  #licor data points      
      points(dat$Ci[-sel],dat$Photo[-sel],col="yellow",pch=20)
      legend("bottomright",legend=c("curve data","other data","mean","CI","PI"),
             col=c(1,"yellow",2,"grey60","grey90"),pch=18,lty=1,lwd=8,bty='n')
    } else{
      print("No ACi data available") 
    }
      
  }
  
  if("aq" %in% tolower(curve)){
    
    ## filter out A-Ci curve points
    ref = estimate_mode(dat$CO2R); if(ref < 1) return(NULL)
    sel = which(abs(dat$CO2R-ref)/ref < tol)
    
    ## reference plot
    plot(dat$PARi[sel],dat$Photo[sel],col=status[dat$QC[sel]+2],pch=20,cex=2,main=paste("CLICK ON OUTLIERS\n",dat$fname[1]))
    points(dat$PARi[-sel],dat$Photo[-sel],col=status[4],pch=20,cex=2)
    text(ulhc[1],ulhc[2],"FAIL\nALL",col='red')
    legend("bottomright",legend=c("fail","unchecked","pass","other"),col=status,pch=18,cex=1.5,bty='n')
        
    if(length(sel)>0){
      sel <- sel[order(dat$Ci[sel])]
      
      ## reference plot
      plot(dat$PARi[sel],dat$Photo[sel],pch=20,cex=1.5,
           xlab="PAR (umol m-2 s-1)",ylab="An (umol m-2 s-1)",main=dat$fname[1],ylim=range(pmean))
      ciEnvelope(dat$PARi[sel],pA[1,sel],pA[3,sel],col="grey90") #plot PI      
      ciEnvelope(dat$PARi[sel],pmean[1,sel],pmean[3,sel],col="grey60") #plot CI      
      lines(dat$PARi[sel],pmean[2,sel],col=2,lwd=3)  #model line
      points(dat$PARi[sel],dat$Photo[sel],pch=20,cex=1.5)  #licor data points      
      points(dat$PARi[-sel],dat$Photo[-sel],col="yellow",pch=20)
      legend("bottomright",legend=c("curve data","other data","mean","CI","PI"),
             col=c(1,"yellow",2,"grey60","grey90"),pch=18,lty=1,lwd=8,bty='n')
    } else{
      print("No AQ data available") 
    }
    
    
  }
  
  
}