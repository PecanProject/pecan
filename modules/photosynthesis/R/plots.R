##shaded confidence interval
ciEnvelope <- function(x,ylo,yhi,col="lightgrey",...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), col=col, border = NA,...) 
} 

plot.photo <- function(data,out,curve=c("ACi","AQ"),tol=0.05,byLeaf=TRUE){
  
  params  = as.matrix(out$params)
  predict = as.matrix(out$predict)
  CI <- apply(predict,2,quantile,c(0.025,0.5,0.975))
  pmean = CI[,grep("pmean",colnames(CI))]
  pA    = CI[,grep("pA",colnames(CI))]
  
  ## determine whether the fit was done by leaf or not
  # byLeaf = length(grep("tau.",colnames(params))) > 0 || length(grep("beta",colnames(params)))>0
  
  if(byLeaf){
    id = data[,"fname"]
    n.curves = length(unique(id))
    curve.id = as.numeric(as.factor(id))
    curve.code = tapply(as.character(id),curve.id,unique)
  } else {
    n.curves = 1
    curve.id = rep(1,nrow(data))
    curve.code = "COMBINED"
  }
  
  for(c in seq_len(n.curves)){
    srow = which(curve.id == c)
    dat = data[srow,]

  if("aci" %in% tolower(curve)){
    
    ## filter out A-Q curve points
    ref = estimate_mode(dat$PARi); if(ref < 1) return(NULL)
    sel = which(abs(dat$PARi-ref)/ref < tol)
    
    if(length(sel)>3){
      sel <- sel[order(dat$Ci[sel])]
    
      ## reference plot
      plot(dat$Ci[sel],dat$Photo[sel],pch=20,cex=1.5,
         xlab="Ci (ppm)",ylab="An (umol m-2 s-1)",main=paste(curve.code[c],"A-Ci"),ylim=range(pmean))
      ciEnvelope(dat$Ci[sel],pA[1,srow[sel]],pA[3,srow[sel]],col="grey90") #plot PI      
      ciEnvelope(dat$Ci[sel],pmean[1,srow[sel]],pmean[3,srow[sel]],col="grey60") #plot CI      
      lines(dat$Ci[sel],pmean[2,srow[sel]],col=2,lwd=3)  #model line
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
    
    if(length(sel)>3){
      sel <- sel[order(dat$PARi[sel])]
      
      ## reference plot
      plot(dat$PARi[sel],dat$Photo[sel],pch=20,cex=1.5,
           xlab="PAR (umol m-2 s-1)",ylab="An (umol m-2 s-1)",main=paste(curve.code[c],"A-Q"),ylim=range(pmean))
      ciEnvelope(dat$PARi[sel],pA[1,srow[sel]],pA[3,srow[sel]],col="grey90") #plot PI      
      ciEnvelope(dat$PARi[sel],pmean[1,srow[sel]],pmean[3,srow[sel]],col="grey60") #plot CI      
      lines(dat$PARi[sel],pmean[2,srow[sel]],col=2,lwd=3)  #model line
      points(dat$PARi[sel],dat$Photo[sel],pch=20,cex=1.5)  #licor data points      
      points(dat$PARi[-sel],dat$Photo[-sel],col="yellow",pch=20)
      legend("bottomright",legend=c("curve data","other data","mean","CI","PI"),
             col=c(1,"yellow",2,"grey60","grey90"),pch=18,lty=1,lwd=8,bty='n')
    } else{
      print("No AQ data available") 
    }
    
    
  } ## end A-Q
    
  } ## end loop over curves
  
  
}