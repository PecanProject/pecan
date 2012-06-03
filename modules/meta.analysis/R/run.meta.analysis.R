#--------------------------------------------------------------------------------------------------#
##' 
##' @name run.meta.analysis.R
##'
##' @title Invoke PEcAn meta.analysis
##'
##'
##'
##'
##' @author Shawn Serbin
#--------------------------------------------------------------------------------------------------#
run.meta.analysis.R <- function() {
  if('meta.analysis' %in% names(settings)) {
    
    ### Get info on how many iterations should be run within the meta.analysis
    ma.iter   = as.numeric(settings$meta.analysis$iter)
    
    ### Warn user if no iterations are specified.  Could also change to throw warning but then use a default number.
    if(length(ma.iter)==0) stop("**** WARNING: PEcAn meta.analysis requested without specifying the number of iterations****")
    
    ### could be
#     if(length(ma.iter)==0) {
#       print("**** WARNING: PEcAn meta.analysis requested without specifying the number of iterations****")
#       print("****Using default number of 10000****")
#       ma.iter=10000
#     }
    ###
    
    ### Identify PFTs in the input settings.xml file
    num.pfts <- length(settings$pfts)
    pft.names=as.list(rep(NA,num.pfts))
    for (i in 1:num.pfts){
      pft.names[i] <- settings$pfts[i]$pft$name
      
      ### If no PFT(s) are specified insert NULL to warn user 
      if(length(pft.names)==0) pft.names[1]="NULL" 
      ###
      
    } ### End of for loop to extract pft names
    
    print(" ")
    print("-------------------------------------------------------------------")
    print("Selected PFT(s) for meta.analysis: ")
    print(pft.names)
    print("-------------------------------------------------------------------")
    print(" ")
    
    ### Check that PFT(s) were specified in settings.xml file.  Otherwise stop and report error.
    if(length(settings$pfts)==0) stop("**** WARNING: No PFT(s) specified in the PEcAn XML settings file ****......Please check.")
    ###
    
    ## loop over pfts
    for(pft in settings$pfts[3]){
      
      print(" ")
      print("-------------------------------------------------------------------")
      print(paste(" Running meta.analysis for PFT: ",pft$name,sep="") )
      print("-------------------------------------------------------------------")
      print(" ")
    
      ### Load trait data for PFT
      load(paste(pft$outdir, 'trait.data.Rdata', sep=''))
      load(paste(pft$outdir, 'prior.distns.Rdata', sep=''))
      
      print("-------------------------------------------------------------------")
      print(paste("Trait data loaded for PFT: ", pft$name,sep=""))
      print("-------------------------------------------------------------------")
      print(" ")
      Sys.sleep(1) 

      ### Jagify trait data for meta.analysis
      jagged.data <- jagify(trait.data)
      ma.data = jagged.data
      for (i in 1:length(jagged.data)){
        ma.data[[i]] <- rename.jags.columns(jagged.data[[i]])
      }
      trait.data <- ma.data
      
      ### Average trait data
      trait.average <- sapply(trait.data,function(x){mean(x$Y,na.rm=TRUE)})
      
      ### Set gamma distribution prior
      prior.variances = as.data.frame(rep(1,nrow(prior.distns)))
      row.names(prior.variances) = row.names(prior.distns)
      prior.variances[names(trait.average),] = 0.001*trait.average^2 
      prior.variances["seedling_mortality",1] = 1.0
      taupriors <- list(tauA = 0.01,
                        tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
      
      ### Run the meta-analysis
      trait.mcmc  <- pecan.ma(trait.data, prior.distns, taupriors, j.iter = ma.iter, 
                              settings, outdir = pft$outdir)
      post.distns <- approx.posterior(trait.mcmc,prior.distns,trait.data,pft$outdir)

      ### Generate summaries and diagnostics
      pecan.ma.summary(trait.mcmc, pft$name, pft$outdir)
      
      ### Save the meta.analysis output
      save(trait.mcmc, file = paste(pft$outdir, 'trait.mcmc.Rdata', sep=''))
      save(post.distns, file = paste(pft$outdir, 'post.distns.Rdata', sep = ''))  

    } ### End meta.analysis for loop
    
  } else {
    
    print('PEcAn settings file does not call for a trait meta-analysis')
    
  } ### End if/else
  
} ### End of function: run.meta.analysis.R
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################