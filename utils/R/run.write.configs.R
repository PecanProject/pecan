#--------------------------------------------------------------------------------------------------#
##' Main driver function to call the ecosystem model specific (e.g. ED, SiPNET) 
##' run and configuration file scripts 
##' 
##' @name run.write.configs
##' @title Run model specific write configuration functions
##' @param model the ecosystem model to generate the configuration files for
##' @export
##'
##' @author Shawn Serbin
run.write.configs <- function(model){

  
  ### Read in settings file if not in workspace.  Should eventually remove.
  if(!exists("settings")){
    settings = read.settings(pecan.settings.file)
    print("-------------------------------------------------------------------")
    print(paste("Using PEcAn settings file: ",pecan.settings.file, sep = ""))
    print("-------------------------------------------------------------------")
    print(" ")
    print(" ")
    exit()
  }
  ###
  
  ### Identify PFTs in the input settings.xml file
  num.pfts <- length(settings$pfts)
  pft.names=as.list(rep(NA,num.pfts))
  outdirs=as.list(rep(NA,num.pfts))
  for (i in 1:num.pfts){
    pft.names[i] <- settings$pfts[i]$pft$name
    
    ### If no PFT(s) are specified insert NULL to warn user 
    if(length(pft.names)==0) pft.names[1]="NULL" 
    ###
    
    ### Get output directory info
    outdirs[i] <- settings$pfts[i]$pft$outdir
    
  } ### End of for loop to extract pft names
  
  print(" ")
  print("-------------------------------------------------------------------")
  print("Selected PFT(s): ")
  print(pft.names)
  print("-------------------------------------------------------------------")
  print(" ")
  
  ### Generate empty list arrays for output.
  trait.samples <- list()
  sa.samples <- list()
  ensemble.samples <- list()
  env.samples <- list()
  ###
  
  ### Define main output directory and host for SA/Ensemble run.
  main.outdir <- settings$outdir
  host <- settings$run$host
  
  ### Prepare for model output.  Cleanup any old config files (if exists)
  remove.config(main.outdir,settings)

  ### Load PFT priors and posteriors
  for (i in seq(pft.names)){
    
    ### Load priors
    load(paste(outdirs[i], 'prior.distns.Rdata', sep=''))
    
    ### Load trait mcmc data (if exists)
    if("trait.mcmc.Rdata" %in% dir(unlist(outdirs))) {
      load(paste(outdirs[i], 'trait.mcmc.Rdata', sep=''))
    }
    
    pft.name <- unlist(pft.names[i])
    
    ### When no ma for a trait, sample from  prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    if(exists('trait.mcmc')) {
      traits <- names(trait.mcmc)
      samples.num <- min(sapply(trait.mcmc, function(x) nrow(as.matrix(x))))
    } else {
      traits <- NA
      samples.num <- 20000
    }
    
    priors <- rownames(prior.distns)
    for (prior in priors) {
      if (prior %in% traits) {
        samples <- as.matrix(trait.mcmc[[prior]][,'beta.o'])
      } else {
        samples <- get.sample(prior.distns[prior,], samples.num)
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
    
    ### Calculate ED specific variables from trait samples for SA.  E.g. dark_resp_factor
    ### trait.samples[[pft.name]] <- calc.ed.specific(trait.samples[[pft.name]])
    ### *** need to move this into model-specific function!!
    
  } ### End for loop
  
  ### NEED TO IMPLEMENT: 
  ## Load Environmental Priors and Posteriors
  ###
  
  ### Sensitivity Analysis
  if('sensitivity.analysis' %in% names(settings)) {
    
    if( is.null(settings$sensitivity.analysis)) {
      print(paste('sensitivity analysis settings are NULL'))
    } else {
      
      ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
      quant <- get.quantiles(settings$sensitivity.analysis$quantiles)
      ### Get info on the years to run the sensitivity analysis (if requested)
      sa.years = data.frame(sa.start = settings$sensitivity.analysis$start.year, 
                            sa.end = settings$sensitivity.analysis$end.year)
      
      print(" ")
      print(" ")
      print("-------------------------------------------------------------------")
      print("Selected Quantiles: ")
      #print("Lower             Mid             Upper")
      print(round(quant,3))
      print("-------------------------------------------------------------------")
      print(" ")
      print(" ")
      
      ### Generate list of sample quantiles for SA run
      sa.samples <-  get.sa.sample.list(trait.samples, 
                                        env.samples,
                                        quant)
      ### Write out SA config files
      if(!exists("cnt")) {            
        cnt=0
        assign("cnt",cnt,.GlobalEnv)
        }
      write.sa.configs(settings$pfts, sa.samples, 
                       host, main.outdir, settings,model=model)
    }
  } ### End of SA
  
  ### Write ENSEMBLE
  if('ensemble' %in% names(settings) && settings$ensemble$size > 0) {
    
    ## subset the trait.samples to ensemble size using Halton sequence 
    ensemble.samples <- get.ensemble.samples(settings$ensemble$size, 
                                             trait.samples, env.samples)
    
    print(" ")
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste("Ensemble size: ",settings$ensemble$size))
    print("-------------------------------------------------------------------")
    print(" ")
    print(" ")
    
    write.ensemble.configs(settings$pfts, ensemble.samples, 
                           host, main.outdir, settings,model=model)
    
  }else{
    print(paste('Ensemble analysis settings are NULL'))
  } ### End of Ensemble
  
  ######################## Finish up runs ########################
  ### Save output from SA/Ensemble runs
  save(ensemble.samples, trait.samples, sa.samples, settings,
       file = paste(main.outdir, 'samples.Rdata', sep=''))
  
  ### Make outdirectory, send samples to outdir
  if(host$name == 'localhost'){
    if(!host$outdir == outdir) {
      dir.create(host$outdir)
      file.copy(from = paste(outdir, 'samples.Rdata', sep=''),
                to   = paste(host$outdir, 'samples.Rdata', sep = ''),
                overwrite = TRUE)
    }
  } else {  
    mkdir.cmd <- paste("'if ! ls ", host$outdir, " > /dev/null ; then mkdir -p ", 
                       host$outdir," ; fi'",sep='')
    system(paste("ssh", host$name, mkdir.cmd))
    system(paste('rsync -routi ', paste(main.outdir, 'samples.Rdata', sep=''),
                 paste(host$name, ':', host$outdir, sep='')))
  }

  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
