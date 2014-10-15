## development testing of fitA


  setwd("~/Dropbox/Dietze_Lab_Undergrads/JAM - Xsite/UROP/")
  in.folder = "raw" 
  pattern = "JAM_B"
  cov.file = NULL#'c3covariates.txt'

  
  ## Read Photosynthetic gas exchange data
  filenames <- list.files(in.folder,pattern=pattern, full.names=TRUE)
  master = lapply(filenames, read.Licor)
  save(master,file="master.RData")

  ## run QA/QC checks
  for(i in 1:length(master)){
    master[[i]] = Licor.QC(master[[i]])
    save(master,file="master.RData")
  }
  
  ## Merge licor data
  dat<-do.call("rbind", master)
  dat = dat[-which(dat$QC < 1),]  ## remove both unchecked points and those that fail QC
  
  
  ## Read Covariate Data
  if(!is.null(cov.file)){
    if(file.exists){
      cov.data=read.table(cov.file,header=TRUE) #v2 has NA filled-in    
    } else {
      print("Covariate file does not exist",cov.file)
      cov.file=cov.data=NULL
    }
  }
  
fit = fitA(dat)
  
A.model = list(a.fixed=NULL,a.random=NULL,V.fixed=NULL,V.random=NULL,n.iter=5000,match="fname")
  
fit = fitA(dat,cov.data,A.model)
    
plot(fit$params)    ## MCMC diagnostic plots
summary(fit$params) ## parameter estimates  
  
  ## predicted vs observed plot
  mstats = summary(fit$predict)
  pmean = mstats$statistics[grep("pmean",rownames(mstats$statistics)),1]
  plot(pmean,dat$Photo,pch="+")
  abline(0,1,col=2,lwd=2)
  
  
plot.photo(dat,fit)
  