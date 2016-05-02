##' @name Licor.QC
##' @title Licor.QC
##' @author Mike Dietze
##' @export
##' @param dat  data frame
##' @param curve Whether to do Quality Control by examining the "ACi" curve, the "AQ" curve, or both
##' @param tol   Code automatically tries to separate ACi and AQ curves in the same dataset by detecting the 'reference' condition for light and CO2 respectively. This is the relative error around the mode in that detection.
Licor.QC <- function(dat,curve=c("ACi","AQ"),tol=0.05){
  
  if(!("QC" %in% names(dat))) dat$QC = rep(0,nrow(dat))
  pos = c(0.1,0.9)
  
  status = c("red","black","blue","grey")
  if("aci" %in% tolower(curve)){

    ## filter out A-Q curve points
    ref = estimate_mode(dat$PARi); if(ref < 1) return(NULL)
    sel = which(abs(dat$PARi-ref)/ref < tol)
    
    ulhc = c(sum(rev(pos)*range(dat$Ci[sel],na.rm=TRUE)),sum(pos*range(dat$Photo[sel],na.rm=TRUE))) ## upper left hand corner
      
    ## reference plot
    plot(dat$Ci[sel],dat$Photo[sel],col=status[dat$QC[sel]+2],pch=20,cex=2,main=paste("CLICK ON OUTLIERS\n",dat$fname[1]))
    points(dat$Ci[-sel],dat$Photo[-sel],col=status[4],pch=20,cex=2)
    text(ulhc[1],ulhc[2],"FAIL\nALL",col='red')
    legend("bottomright",legend=c("fail","unchecked","pass","other"),col=status,pch=18,cex=1.5,bty='n')
    
    ## flag outliers
    flag = identify(c(dat$Ci[sel],ulhc[1]),c(dat$Photo[sel],ulhc[2]))
    if(length(flag)>0){
      if(max(flag)>length(sel)){
        dat$QC[sel]=-1
      }else{
        dat$QC[sel[flag]] = -1
      }
    }
    dat$QC[sel[dat$QC[sel]==0]] = 1    

    ## Updated plot
    plot(dat$Ci[sel],dat$Photo[sel],col=status[dat$QC[sel]+2],pch=20,cex=2,main=paste("UPDATED",dat$fname[1],"\nclick to undo Outliers"))
    points(dat$Ci[-sel],dat$Photo[-sel],col=status[4],pch=20,cex=2)
    legend("bottomright",legend=c("fail","unchecked","pass"),col=status,pch=18,cex=1.5,bty='n')

    flag = identify(dat$Ci[sel],dat$Photo[sel])
    ## undo outliers
    if(length(flag)>0){
      dat$QC[sel[flag]] = 1
    }
    
  }
  
  if("aq" %in% tolower(curve)){
    
    ## filter out A-Ci curve points
    ref = estimate_mode(dat$CO2R); if(ref < 1) return(NULL)
    sel = which(abs(dat$CO2R-ref)/ref < tol)
    
    ulhc = c(sum(rev(pos)*range(dat$PARi[sel],na.rm=TRUE)),sum(pos*range(dat$Photo[sel],na.rm=TRUE))) ## upper left hand corner
    
    ## reference plot
    plot(dat$PARi[sel],dat$Photo[sel],col=status[dat$QC[sel]+2],pch=20,cex=2,main=paste("CLICK ON OUTLIERS\n",dat$fname[1]))
    points(dat$PARi[-sel],dat$Photo[-sel],col=status[4],pch=20,cex=2)
    text(ulhc[1],ulhc[2],"FAIL\nALL",col='red')
    legend("bottomright",legend=c("fail","unchecked","pass","other"),col=status,pch=18,cex=1.5,bty='n')
    
    ## flag outliers
    flag = identify(c(dat$PARi[sel],ulhc[1]),c(dat$Photo[sel],ulhc[2]))
    if(length(flag)>0){
      if(max(flag)>length(sel)){
        dat$QC[sel]=-1
      }else{
        dat$QC[sel[flag]] = -1
      }
    }
    dat$QC[sel[dat$QC[sel]==0]] = 1 
    
    ## updated plot
    plot(dat$PARi[sel],dat$Photo[sel],col=status[dat$QC[sel]+2],pch=20,cex=2,main=paste("UPDATED",dat$fname[1],"\nclick to undo Outliers"))
    points(dat$PARi[-sel],dat$Photo[-sel],col=status[4],pch=20,cex=2)
    legend("bottomright",legend=c("fail","unchecked","pass"),col=status,pch=18,cex=1.5,bty='n')

    ## undo outliers
    flag = identify(dat$PARi[sel],dat$Photo[sel])
    if(length(flag)>0){
      dat$QC[sel[flag]] = 1
    }
    
  }
  
  invisible(dat)
}

##' @name estimate_mode
##' @title estimate_mode
##' @author Mike Dietze
##' @author Xiaohui Feng
##' @export
estimate_mode <- function(x,adjust=0.1) {
  d <- density(x,na.rm=TRUE,adjust=adjust)
  d$x[which.max(d$y)]
}
