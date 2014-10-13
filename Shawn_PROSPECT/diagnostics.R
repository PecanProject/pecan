#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' @name plot.prospect.inv
##' @title Diagnostic plots displaying the results of the PROSPECT model inversion
##'
##' @param inv.output output from the PROSPECT inversion
##' @param outdir output directory for diagnostic plots
##' @param file file name for diagnostic plots
##' @return plot diagnostic plots of model inversion
##' @export
##'
##' @author Shawn Serbin
plot.prospect.inv <- function(inv.output,outdir,file){
  
  mod.spectra <- inv.output$PROSPECT.Spectra
  parms <- inv.output$Parameters
  parm.length <- length(inv.output$Parameters)
  
  ### Plot params
  cexaxis <- 1.2
  cexlab <- 1.4
  sep <- .Platform$file.sep
  pdf(paste(outdir,sep,file,".pdf",sep=""),height=8,width=10)
  
  ### Modeled vs. observed spectra
  par(mar=c(5,5,1,5)) #b, l, t, r
  plot(waves,refl,type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
       ylab="Reflectance (%)",cex.axis=cexaxis,cex.lab=cexlab)
  lines(waves,1-tran,lwd=2.5,col="dark grey")
  lines(mod.spectra$Wavelength,mod.spectra$Reflectance,type="l",lty=2,lwd=1.8)
  par(new=TRUE)
  lines(mod.spectra$Wavelength,1-mod.spectra$Transmittance,type="l",
        lty=2,lwd=1.8,col="dark grey",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=cexaxis,
       cex.lab=cexlab)
  mtext("Transmittance (%)",side=4,line=3)
  legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
  box(lwd=2.2)

  if (parm.length==5){
    ### Parameter trace diagnostic plots
    par(mfrow=c(2,2),mar=c(4,4.1,1,2)) #b, l, t, r
    plot(inv.output$DEoptim.obj$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="N",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,1],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Chl a+b",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,2],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,3],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cw",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,3],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,4],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cm",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,4],lty=2,lwd=1.8)
    box(lwd=2.2)
    dev.off()
    
  } else if (parm.length==6){
    par(mfrow=c(3,2),mar=c(4,4.1,1,2)) #b, l, t, r
    plot(inv.output$DEoptim.obj$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="N",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,1],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Chl a+b",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,2],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,3],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Car",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,3],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,4],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cw",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,4],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,5],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cm",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,5],lty=2,lwd=1.8)
    box(lwd=2.2)
    dev.off()
    
  } else {
    par(mfrow=c(3,2),mar=c(4,4.1,1,2)) #b, l, t, r
    plot(inv.output$DEoptim.obj$member$bestmemit[,1],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="N",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,1],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,2],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Chl a+b",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,2],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,3],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Car",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,3],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,4],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cbrown",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,4],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,5],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cw",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,5],lty=2,lwd=1.8)
    box(lwd=2.2)
    plot(inv.output$DEoptim.obj$member$bestmemit[,6],pch=21,bg="dark grey",col="dark grey",
         cex=1.2,xlab="Iteration",ylab="Cm",cex.axis=cexaxis,cex.lab=cexlab)
    lines(inv.output$DEoptim.obj$member$bestmemit[,6],lty=2,lwd=1.8)
    box(lwd=2.2)
    dev.off()
  }

}
