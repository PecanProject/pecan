## development testing of fitA


  setwd("~/Dropbox/Dietze_Lab_Undergrads/JAM - Xsite/UROP/")
  in.folder = "raw" 
  pattern = "JAM_B"
  cov.file = NULL#'c3covariates.txt'

  
  ## Read Photosynthetic gas exchange data
  filenames <- list.files(in.folder,pattern=pattern, full.names=TRUE)
  master = lapply(filenames, read.Licor)
  save(master,file="master.R")

  ## run QA/QC checks
  for(i in 1:length(master)){
    master[[i]] = Licor.QC(master[[i]])
    save(master,file="master.R")
  }
  
  ## Merge licor data
  dat<-do.call("rbind", master)
  dat = dat[-which(dat$QC < 0),]
  
  
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
  
A.model = list(a.fixed=NULL,a.random=NULL,V.fixed=NULL,V.random=NULL,n.iter=5000)
  
fit = fitA(dat,cov.data,A.model)
    
plot(fit)
  
  mstats = summary(fit)
  prean = mstats$statistics[grep("prean",rownames(mstats$statistics)),1]
  plot(prean,dat$Photo,pch="+")
  abline(0,1,col=2,lwd=2)

  