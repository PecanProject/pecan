###-------------------------------------------------------------------###
### ensemble adjustment plots                                         ###
###-------------------------------------------------------------------### 

#function for plotting matplot with ensemble number as label
mattext = function(data, data_names, colors, ylab, xlab, type='b', na.fix = FALSE){
  if(na.fix == TRUE){
    data[is.na(data)] <- 0
  }
  
  matplot(data, pch=NA, type=type, col=colors, ylab = ylab, xlab = xlab)
  for (i in 1:ncol(data)){
    text(x=1:nrow(data), y=data[,i], lab=data_names[i], col=colors[i])
  }
}

#calculate the likelihood of the ensemble members given mu.a and Pa
wt.mat <- matrix(NA,nrow=nens,ncol=nt)
for(t in seq_len(nt)){
  for(i in seq_len(nens)){
    wt.mat[i,t]<-dmnorm_chol(FORECAST[[t]][i,],enkf.params[[t]]$mu.a,enkf.params[[t]]$Pa)
  }
}
#put into weights table  
wt.props <- t(prop.table(wt.mat,2))

pdf(file.path(settings$outdir,'ensemble.weights.time-series.pdf'))
par(mfrow=c(1,1))
mattext(data = wt.props,data_names = as.character(1:nens),colors=rainbow(nens),
        ylab = c('Ensemble Weight'), xlab = c('Time'))
dev.off()

library(Hmisc)
pft.names <- as.character(lapply(settings$pfts, function(x) x[["name"]]))
param.names <- names(params[[1]][[1]])
param.hist <- array(NA,dim=c(length(param.names),length(pft.names),nens))
wt.df <- array(NA, dim = c(length(param.names),length(pft.names),nt,4))

pdf('weighted.param.time-series.pdf')
par(mfrow=c(4,5))
for(p in 1:length(param.names)){
  for(s in 1:length(pft.names)){
    pft <- pft.names[s]
    param.plot <- param.names[p]
    
    param.check <- unlist(lapply(lapply(params,'[[',pft),'[[',param.plot))
    
    if(!is.null(param.check)){
      param.hist[p,s,] <- param.check
      wt.mean <- wt.var <- numeric(nt)
      
      for(t in 1:nt){
        wt.mean[t] <- wtd.mean(x=param.hist[p,s,], w = wt.props[t,])
        wt.var[t] <- wtd.var(x=param.hist[p,s,], w = wt.props[t,])
      }
      
      wt.df[p,s,,1] <- wt.mean
      wt.df[p,s,,2] <- wt.mean - mean(param.hist[p,s,])
      wt.df[p,s,,3] <- wt.var
      wt.df[p,s,,4] <- wt.var - var(param.hist[p,s,])
      
      #plot weighted mean
      plot(wt.mean,type='l',ylab='Weighted Mean',xlab='Time')
      points(wt.mean, pch=19,cex=.4)
      abline(h=mean(param.hist[p,s,]))
      abline(h = param.hist[p,s,which.min(colMeans(wt.props,na.rm = TRUE))],col='red')
      abline(h = param.hist[p,s,which.max(colMeans(wt.props,na.rm = TRUE))],col='green')
      title(main = list(paste(pft,'\n',param.plot), cex = .5))
      
      #coloring by the difference in the mean relative to the scale of the parameter
      diff.mean <- abs(mean(wt.mean) - mean(param.hist[p,s,]))
      if(diff.mean > abs(.00001*mean(param.hist[p,s,]))){
        mtext(text = paste(signif(diff.mean,digits = 3)), side = 3,col = 'red')
      }else{
        mtext(text = paste(signif(diff.mean,digits = 3)), side = 3)
      }
      
      #Plot weighted variance
      plot(wt.var,type='l',ylab='Weighted Variance',xlab='Time')
      points(wt.var, pch=19,cex=.5)
      abline(h=var(param.hist[p,s,]))
      title(main = list(paste(pft,'\n',param.plot), cex = .5))
      
      hist(param.hist[p,s,], freq = FALSE, col= 'lightgrey', main = paste(pft,'\n',param.plot))
      for(t in 1:nt){
        lines(density(param.hist[p,s,], weights = wt.props[t,], na.rm = TRUE),
              lwd = 2, col=rainbow(49)[t])
      }
      
    }else{
      plot.new()
    }
    
  }
}
dev.off()

pdf('weighted.hists.pdf')
par(mfrow = c(4,4))  
plot.new()
legend('center',c('Weighted Means','Prior Means'),pch = 19,col=c('lightgrey','black'))
for(p in 1:length(param.names)){
  hist(wt.df[p,,,1], main=param.names[p], freq = FALSE, col = 'lightgrey', xlab = 'Param Value')
  lines(density(rowMeans(param.hist[p,,]),na.rm = TRUE), lwd = 2)
  
}
dev.off()

which.min(colMeans(wt.props,na.rm = TRUE))
which.max(colMeans(wt.props,na.rm = TRUE))



par(mfrow=c(1,1))
mattext(param.hist[1,,], data_names = as.character(1:nens), colors=rainbow(nens),
        ylab = c('Parameter Value'), xlab = c('PFT'), type='p', na.fix = TRUE)

library(weights)
par(mfrow=c(1,2))
weighted.hist(x = param.hist, w = wt.props[nt,],col = 'lightgrey')
hist(param.hist,col = 'lightgrey',xlim = range(dd$x))
plot(density(param.hist))
plot(density(param.hist*wt.props[nt,]*10))

## weighted quantile
wtd.quantile <- function(x,wt,q){ 
  ord <- order(x)
  wstar <- cumsum(wt[ord])/sum(wt)
  qi <- findInterval(q,wstar); qi[qi<1]=1;qi[qi>length(x)]=length(x)
  return(x[ord[qi]])
}

param.quant <- matrix(NA, 3, nt)

for(t in seq_len(nt)){
  param.quant[,t] <- wtd.quantile(x = param.hist, wt=wt.mat[,t],q=c(.025,.5,.975))
}

plot(param.quant[2,], ylim = range(param.quant,na.rm = TRUE))
ciEnvelope(x = 1:nt, ylo = param.quant[1,1:nt], yhi = param.quant[3,1:nt], col = 'lightblue')
points(param.quant[2,], pch = 19, cex = 1)



