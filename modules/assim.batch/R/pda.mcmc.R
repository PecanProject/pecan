## Paramater Data Assimilation using MCMC
## Mike Dietze
##
## Brute-force, only to be used on simple models
##
## model = name of model (string)
## chain = ID number of mcmc chain
## params = previous output (used when updating MCMC)
## vars = vector of indices of which parameters in the parameter vector to vary
## jvar = jump variance


pda.mcmc <- function(model,chain=1,vars=NULL,jvar=NULL,params=NULL){
  
  ## this bit of code is useful for defining the variables passed to this function 
  ## if you are debugging
  if(FALSE){
    model = "SIPNET"
    chain = 1
    params = NULL
    vars = NULL
    jvar = NULL
  }
  
  ## settings
  weight <- 0.001
  start <- 1
  finish <- as.numeric(settings$assim.batch$iter)
  defaults <- settings$pfts
  outdir <- settings$outdir
  host <- settings$run$host

  ## model-specific functions
  my.write.config <- paste("write.config.",model,sep="")
  if(!exists(my.write.config)){
    print(paste(my.write.config,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
  my.start.run <- paste("start.run.",model,sep="")
  if(!exists(my.start.run)){
    print(paste(my.start.run,"does not exist"))
    print(paste("please make sure that the PEcAn interface is loaded for",model))
    stop()
  }
  read.SIPNET.NEE <- function(run.id,outdir){
    sipnet.output <- read.table(paste(outdir,"/",run.id,"/",run.id,".out",sep=""),header=T,skip=1,sep='')
    sipnet.output.dims <- dim(sipnet.output)
    ### Determine number of years and output timestep
    num.years <- length(unique(sipnet.output$year))
    years <- unique(sipnet.output$year)
    timestep.s <- 86400/length(which(sipnet.output$year==years[1] & sipnet.output$day==1))
    return((sipnet.output$nee/12*1e6)/timestep.s)     # NEE in gC/m2/timestep -> umolC/m2/s 
  }

  
  ## set up priors
  load(file.path(settings$pfts$pft$outdir,"post.distns.Rdata"))
  nvar <- nrow(post.distns)
  dprior <- rprior <-list()
  for(i in 1:nvar){
    if(post.distns$distn[i] == 'exp'){
      dprior[[i]] <- parse(text=paste("dexp(x,",post.distns$parama[i],",log=TRUE)",sep=""))
      rprior[[i]] <- parse(text=paste("rexp(n,",post.distns$parama[i],")",sep=""))
    }else{
      dprior[[i]] <- parse(text=paste("d",post.distns$distn[i],"(x,",post.distns$parama[i],",",post.distns$paramb[i],",log=TRUE)",sep=""))
      rprior[[i]] <- parse(text=paste("r",post.distns$distn[i],"(n,",post.distns$parama[i],",",post.distns$paramb[i],")",sep=""))
    }
  }
  dmvprior <- function(x,log=TRUE){  #multivariate prior - density
    p = rep(NA,nvar)
    for(i in 1:nvar){
      p[i] = eval(dprior[[i]],list(x=x[i]))
    }
    p = sum(p)
    if(log) return(p)
    return(exp(p))
    return(p)
  }
  rmvprior <- function(n){  #multivariate prior - random number
    p = matrix(NA,n,nvar)
    for(i in 1:nvar){
      p[,i] = eval(rprior[[i]],list(n=n))
    }
    return(p)
  }
  pname =  rownames(post.distns)
  if(is.null(vars)){
    vars = 1:nvar
  }
       
  ## load data
  data <- read.csv(settings$assim.batch$input)
  NEEo <- data$Fc   #umolCO2 m-2 s-1
  NEEq <- data$qf_Fc
  NEEo[NEEq > 0] <- NA
  
  ## calculate flux uncertainty parameters
  dTa <- get.change(data$Ta)
  flags <- dTa < 3   ## filter data to temperature differences that are less thatn 3 degrees
  NEE.params <- flux.uncertainty(NEEo,NEEq,flags,20)
  b0 <- NEE.params$intercept
  bp <- NEE.params$slopeP
  bn <- NEE.params$slopeN
  
  ## set up storage
  if(is.null(params)){
    params <- matrix(NA,finish,nvar)
  } else {
    start <- nrow(params)+1
    params <- rbind(params,matrix(NA,finish-start+1,nvar))
  }
  
  ## set initial conditions
  if(start==1){
    parm = as.vector(rmvprior(1))
  } else{
    parm <- params[start-1,]
  }
  names(parm) = pname
  LL.old <- -Inf
  prior.old <- -Inf
  
  ## set jump variance
  if(is.null(jvar)){
    jvar <- rep(0.1,nvar)
  }
  
  ## main MCMC loop
  for(i in start:finish){

    for(j in vars){
      
      ## propose parameter values
      pnew = rnorm(1,parm[j],jvar[j])
      pstar = parm
      pstar[j] = pnew
        
      ## check that value falls within the prior
      prior.star <- dmvprior(pstar)
      if(is.finite(prior.star)){

        ## write config
        run.id = paste(chain,i,j,sep=".")
        do.call(my.write.config,args=list(defaults,list(pft=pstar,env=NA),
                 settings, outdir, run.id))        

        ## start model run
        do.call(my.start.run,args=list(run.id))

        ## read model output        
#        NEEm <- read.output(run.id,outdir,variables="NEE",model=model)$NEE
        NEEm <- read.SIPNET.NEE(run.id,outdir)
        NEEm <- rep(NEEm,each=2)
        set <- 1:length(NEEm)
        
        ## calculate likelihood
        fsel <- which(NEEm > 0)
        LL.star       <- dexp(abs(NEEm-NEEo[set]),1/(b0 + bn*NEEm),log=TRUE)
        LL.star[fsel] <- dexp(abs(NEEm-NEEo[set]),1/(b0 + bp*NEEm),log=TRUE)[fsel]
        LL.star <- sum(LL.star,na.rm=TRUE)
                
        ## accept or reject step
        a = (LL.star - LL.old)*weight + prior.star - prior.old
        if(a > log(runif(1))){
          LL.old <- LL.star
          prior.old <- prior.star
          parm <- pstar 
        }
      }

    } ## end loop over variables
  
    ## save output
    params[i,] <- parm
      
  } ## end MCMC loop

  return(params)
  
} ## end pda.mcmc
