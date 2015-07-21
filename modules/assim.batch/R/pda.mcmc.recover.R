##' Clean up a failed PDA run
##'
##' @title Clean up a failed PDA run
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return An updated settings list
##'
##' @author Ryan Kelly
##' @export

# This is just a quick kludgey version, that relies on temporary files to recover a failed pda.mcmc() call. It writes all outputs based on whatever runs were done, and returns the same updated settings list that would have been returned if the run completed. So, recover like this:
#
# read.settings(path/to/original/settings/file)
# settings$assim.batch <- pda.mcmc.recover(settings) # wrap up unfinished run
# settings$assim.batch <- pda.mcmc(settings) # start new pda
pda.mcmc.recover <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL, 
                     iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL, n.knot=NULL) {

  if(FALSE){
    params.id <- param.names <- prior.id <- chain <- iter <- NULL 
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- NULL
  }
  
  require(coda)
  
  ## Handle settings
    settings <- pda.settings(
                  settings=settings, params.id=params.id, param.names=param.names, 
                  prior.id=prior.id, chain=chain, iter=iter, adapt=adapt, 
                  adj.min=adj.min, ar.target=ar.target, jvar=jvar, n.knot=n.knot)

  ## Open database connection
  if(settings$database$bety$write){
    con <- try(db.open(settings$database$bety), silent=TRUE)
    if(is.character(con)){
      con <- NULL
    }
  } else {
    con <- NULL
  }

  ## Load priors
  prior <- pda.load.priors(settings, con)$prior
  pname <-  rownames(prior) 
  n.param.all  <- nrow(prior)

  # Get start and finish
  params.dummy <- pda.init.params(settings, con, pname, n.param.all)
    start  <- params.dummy$start
    finish <- params.dummy$finish

  ## Select parameters to constrain
  prior.ind <- which(rownames(prior) %in% settings$assim.batch$param.names)
  n.param <- length(prior.ind)

  ## Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }

  ## Get ensemble id from diagnostic plot dir
  ens.ids <- as.numeric(sub("diag.pda", "", dir(settings$outdir, "diag.pda")))
  settings$assim.batch$ensemble.id <- as.character(max(ens.ids))


  ## Load up temp file to recreate params
  params = as.matrix(read.table(file.path(settings$outdir, "pda.mcmc.txt")))
  colnames(params) <- pname

  ## Update iters
  settings$assim.batch$iter <- finish - nrow(params)

  ## Save outputs to plots, files, and db
  settings <- pda.postprocess(settings, con, params, pname, prior, prior.ind)

  ## close database connection
  if(!is.null(con)) db.close(con)

  ## Output an updated settings list
  return(settings$assim.batch)
  
} ## end pda.mcmc