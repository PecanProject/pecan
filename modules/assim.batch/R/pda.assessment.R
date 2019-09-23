# This is a diagnostic function that checks for post-pda model-data comparison
# the value it returns can be used in determining whether to stop pda rounds or continue
# it samples from MCMC for parameter vectors, runs a small ensemble (100) unless requested otherwise, compares model with data, returns metric
postpda_assessment <- function(settings, n.param.orig, prior.ind.orig, 
                          n.post.knots, knots.params.temp,
                          prior.list, prior.fn, sf, sf.samp){
  
  # sample from MCMC
  sampled_knots <- sample_MCMC(settings$assim.batch$mcmc.path, n.param.orig, prior.ind.orig, 
                               n.post.knots, knots.params.temp,
                               prior.list, prior.fn, sf, sf.samp)
  
  knots.params.temp <- sampled_knots$knots.params.temp
  probs.round.sf    <- sampled_knots$sf_knots
  pass2bias         <- sampled_knots$pass2bias
  
  ## Set up runs and write run configs for all proposed knots
  run.ids <- pda.init.run(settings, con, my.write.config, workflow.id, knots.params, 
                          n = settings$assim.batch$n.knot, 
                          run.names = paste0(settings$assim.batch$ensemble.id, ".knot.",
                                             1:settings$assim.batch$n.knot))   
  
  ## start model runs
  PEcAn.remote::start.model.runs(settings, (as.logical(settings$database$bety$write) & !remote))
  
  ## Retrieve model outputs and error statistics
  model.out <- list()

  ## read model outputs    
  for (i in seq_len(settings$assim.batch$n.knot)) {
    align.return <- pda.get.model.output(settings, run.ids[i], bety, inputs, external.formats)
    model.out[[i]] <- align.return$model.out
    if(all(!is.na(model.out[[i]]))){
      inputs <- align.return$inputs
    }
  }
  
  ee <- sapply(model.out, function(x) x[[1]][[1]])
  ii <- inputs[[1]]$obs
  ee <- ee[!is.na(ii),]
  ii <- ii[!is.na(ii)]
  rmv <- sapply(seq_len(nrow(ee)), function(x){
    if(length(unique(ee[x,])) == 1){
      return(1)
    }else{
      return(0)
    }
  })
  ee <- ee[rmv==0,]
  ii <- ii[rmv ==0]
  sim = createDHARMa(simulatedResponse = ee, observedResponse = ii)
  # calculate metric
  
}
  


