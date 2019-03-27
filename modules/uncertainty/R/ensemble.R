#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Reads output from model ensemble
##'
##' Reads output for an ensemble of length specified by \code{ensemble.size} and bounded by \code{start.year} 
##' and \code{end.year}
##' @title Read ensemble output
##' @return a list of ensemble model output 
##' @param ensemble.size the number of ensemble members run
##' @param pecandir specifies where pecan writes its configuration files
##' @param outdir directory with model output to use in ensemble analysis
##' @param start.year first year to include in ensemble analysis
##' @param end.year last year to include in ensemble analysis
##' @param variable target variables for ensemble analysis
##' @param ens.run.ids dataframe. Must contain a column named "id" giving the run IDs to be read.
##'   If NULL, will attempt to read IDs from a file named "samples.Rdata" in \code{pecandir}
##' @export
##' @author Ryan Kelly, David LeBauer, Rob Kooper
#--------------------------------------------------------------------------------------------------#
read.ensemble.output <- function(ensemble.size, pecandir, outdir, start.year, end.year, 
                                 variable, ens.run.ids = NULL) {
  if (is.null(ens.run.ids)) {
    samples.file <- file.path(pecandir, "samples.Rdata")
    if (file.exists(samples.file)) {
      load(samples.file)
      ens.run.ids <- runs.samples$ensemble
    } else {
      stop(samples.file, "not found required by read.ensemble.output")
    }
  }
  
  expr <- variable$expression
  variables <- variable$variables
  
  ensemble.output <- list()
  for (row in rownames(ens.run.ids)) {
    run.id <- ens.run.ids[row, "id"]
    PEcAn.logger::logger.info("reading ensemble output from run id: ", run.id)
    
    for(var in seq_along(variables)){
      out.tmp <- PEcAn.utils::read.output(run.id, file.path(outdir, run.id), start.year, end.year, variables[var])
      assign(variables[var], out.tmp[[variables[var]]])
    }
    
    # derivation
    out <- eval(parse(text = expr))
    
    ensemble.output[[row]] <- mean(out, na.rm= TRUE) 
    
  }
  return(ensemble.output)
} # read.ensemble.output


##' Get parameter values used in ensemble
##'
##' Returns a matrix of randomly or quasi-randomly sampled trait values 
##' to be assigned to traits over several model runs.
##' given the number of model runs and a list of sample distributions for traits
##' The model run is indexed first by model run, then by trait
##' 
##' @title Get Ensemble Samples
##' @name get.ensemble.samples
##' @param ensemble.size number of runs in model ensemble
##' @param pft.samples random samples from parameter distribution, e.g. from a MCMC chain  
##' @param env.samples env samples
##' @param method the method used to generate the ensemble samples. Random generators: uniform, uniform with latin hypercube permutation. Quasi-random generators: halton, sobol, torus. Random generation draws random variates whereas quasi-random generation is deterministic but well equidistributed. Default is uniform. For small ensemble size with relatively large parameter number (e.g ensemble size < 5 and # of traits > 5) use methods other than halton. 
##' @param param.names a list of parameter names that were fitted either by MA or PDA, important argument, if NULL parameters will be resampled independently
##' @param ... Other arguments passed on to the sampling method
##' 
##' @return matrix of (quasi-)random samples from trait distributions
##' @export
##' @author David LeBauer, Istem Fer
get.ensemble.samples <- function(ensemble.size, pft.samples, env.samples, 
                                 method = "uniform", param.names = NULL, ...) {
  
  if (is.null(method)) {
    PEcAn.logger::logger.info("No sampling method supplied, defaulting to uniform random sampling")
    method <- "uniform"
  }
  
  ## force as numeric for compatibility with Fortran code in halton()
  ensemble.size <- as.numeric(ensemble.size)
  if (ensemble.size <= 0) {
    ans <- NULL
  } else if (ensemble.size == 1) {
    ans <- PEcAn.utils::get.sa.sample.list(pft.samples, env.samples, 0.5)
  } else {
    pft.samples[[length(pft.samples) + 1]] <- env.samples
    names(pft.samples)[length(pft.samples)] <- "env"
    pft2col <- NULL
    for (i in seq_along(pft.samples)) {
      pft2col <- c(pft2col, rep(i, length(pft.samples[[i]])))
    }
    
    total.sample.num <- sum(sapply(pft.samples, length))
    random.samples <- NULL
    
    
    if (method == "halton") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::halton(n = ensemble.size, dim = total.sample.num, ...)
      ## force as a matrix in case length(samples)=1
      random.samples <- as.matrix(random.samples)
    } else if (method == "sobol") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::sobol(n = ensemble.size, dim = total.sample.num, ...)
      ## force as a matrix in case length(samples)=1
      random.samples <- as.matrix(random.samples)
    } else if (method == "torus") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- randtoolbox::torus(n = ensemble.size, dim = total.sample.num, ...)
      ## force as a matrix in case length(samples)=1
      random.samples <- as.matrix(random.samples)
    } else if (method == "lhc") {
      PEcAn.logger::logger.info("Using ", method, "method for sampling")
      random.samples <- PEcAn.emulator::lhc(t(matrix(0:1, ncol = total.sample.num, nrow = 2)), ensemble.size)
    } else if (method == "uniform") {
      PEcAn.logger::logger.info("Using ", method, "random sampling")
      # uniform random
      random.samples <- matrix(stats::runif(ensemble.size * total.sample.num),
                               ensemble.size, 
                               total.sample.num)
    } else {
      PEcAn.logger::logger.info("Method ", method, " has not been implemented yet, using uniform random sampling")
      # uniform random
      random.samples <- matrix(stats::runif(ensemble.size * total.sample.num),
                               ensemble.size, 
                               total.sample.num)
    }
    
    
    ensemble.samples <- list()
    
    
    col.i <- 0
    for (pft.i in seq(pft.samples)) {
      ensemble.samples[[pft.i]] <- matrix(nrow = ensemble.size, ncol = length(pft.samples[[pft.i]]))
      
      # meaning we want to keep MCMC samples together
      if(length(pft.samples[[pft.i]])>0 & !is.null(param.names)){ 
        # TODO: for now we are sampling row numbers uniformly
        # stop if other methods were requested 
        if(method != "uniform"){
          PEcAn.logger::logger.severe("Only uniform sampling is available for joint sampling at the moment. Other approaches are not implemented yet.")
        }
        same.i <- sample.int(length(pft.samples[[pft.i]][[1]]), ensemble.size)
      }
      
      for (trait.i in seq(pft.samples[[pft.i]])) {
        col.i <- col.i + 1
        if(names(pft.samples[[pft.i]])[trait.i] %in% param.names[[pft.i]]){ # keeping samples
          ensemble.samples[[pft.i]][, trait.i] <- pft.samples[[pft.i]][[trait.i]][same.i]
        }else{
          ensemble.samples[[pft.i]][, trait.i] <- stats::quantile(pft.samples[[pft.i]][[trait.i]],
                                                                  random.samples[, col.i])
        }
      }  # end trait
      ensemble.samples[[pft.i]] <- as.data.frame(ensemble.samples[[pft.i]])
      colnames(ensemble.samples[[pft.i]]) <- names(pft.samples[[pft.i]])
    }  #end pft
    names(ensemble.samples) <- names(pft.samples)
    ans <- ensemble.samples
  }
  return(ans)
} # get.ensemble.samples


##' Write ensemble config files
##'
##' Writes config files for use in meta-analysis and returns a list of run ids.
##' Given a pft.xml object, a list of lists as supplied by get.sa.samples, 
##' a name to distinguish the output files, and the directory to place the files.
##'
##' @param defaults pft
##' @param ensemble.samples list of lists supplied by \link{get.ensemble.samples}
##' @param settings list of PEcAn settings
##' @param model name of model to be run, e.g. "ED2" or "SIPNET"
##' @param clean remove old output first?
##' @param write.to.db logical: Record this run in BETY?
##' @param restart In case this is a continuation of an old simulation. restart needs to be a list with name tags of runid, inputs, new.params (parameters), new.state (initial condition), ensemble.id (ensemble id), start.time and stop.time.See Details.
##'
##' @return list, containing $runs = data frame of runids, $ensemble.id = the ensemble ID for these runs and $samples with ids and samples used for each tag.  Also writes sensitivity analysis configuration files as a side effect
##' @details The restart functionality is developed using model specific functions by calling write_restart.modelname function. First, you need to make sure that this function is already exist for your desired model.See here \url{https://pecanproject.github.io/pecan-documentation/master/pecan-models.html}
##' new state is a dataframe with a different column for each state variable. The number of the rows in this dataframe needs to be the same as the ensemble size.
##' State variables that you can use for setting up the intial conditions differs for different models. You may check the documentation of the write_restart.modelname your model.
##' The units for the state variables need to be in the PEcAn standard units which can be found in \link{standard_vars}.
##' new.params also has similar structure to ensemble.samples which is sent as an argument.
##'
##' @importFrom dplyr %>%
##' @export
##' @author David LeBauer, Carl Davidson, Hamze Dokoohaki
write.ensemble.configs <- function(defaults, ensemble.samples, settings, model, 
                                   clean = FALSE, write.to.db = TRUE,restart=NULL) {
  
  my.write.config <- paste("write.config.", model, sep = "")
  my.write_restart <- paste0("write_restart.", model)
  
  if (is.null(ensemble.samples)) {
    return(list(runs = NULL, ensemble.id = NULL))
  }
  
  # Open connection to database so we can store all run/ensemble information
  if (write.to.db) {
    con <- try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(PEcAn.DB::db.close(con))
    }
  } else {
    con <- NULL
  }
  
  # Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  #------------------------------------------------- if this is a new fresh run------------------  
  if (is.null(restart)){
    # create an ensemble id
    if (!is.null(con)) {
      # write ensemble first
      ensemble.id <- PEcAn.DB::db.query(paste0(
        "INSERT INTO ensembles (runtype, workflow_id) ",
        "VALUES ('ensemble', ", format(workflow.id, scientific = FALSE), ")",
        "RETURNING id"), con = con)[['id']]
      
      for (pft in defaults) {
        PEcAn.DB::db.query(paste0(
          "INSERT INTO posteriors_ensembles (posterior_id, ensemble_id) ",
          "values (", pft$posteriorid, ", ", ensemble.id, ")"), con = con)
      }
    } else {
      ensemble.id <- NA
    }
    #-------------------------generating met/param/soil/veg/... for all ensumbles----
    if (!is.null(con)){
      #-- lets first find out what tags are required for this model
      required_tags <- dplyr::tbl(con, 'models') %>%
        dplyr::filter(id == as.numeric(settings$model$id)) %>%
        dplyr::inner_join(dplyr::tbl(con, "modeltypes_formats"), by = c('modeltype_id')) %>%
        dplyr::collect() %>%
        dplyr::filter(required == TRUE) %>%
        dplyr::pull(tag)
      
    }else{
      required_tags<-c("met","parameters")
      
    }
    
    #now looking into the xml
    samp <- settings$ensemble$samplingspace
    #finding who has a parent
    parents <- lapply(samp,'[[', 'parent')
    #order parents based on the need of who has to be first
    order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()] 
    #new ordered sampling space
    samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]
    #performing the sampling
    samples<-list()
    # For the tags specified in the xml I do the sampling
    for(i in seq_along(samp.ordered)){
      myparent<-samp.ordered[[i]]$parent # do I have a parent ?
      #call the function responsible for generating the ensemble
      samples[[names(samp.ordered[i])]] <- input.ens.gen(settings=settings,
                                                         input=names(samp.ordered)[i],
                                                         method=samp.ordered[[i]]$method,
                                                         parent_ids=if( !is.null(myparent)) samples[[myparent]] # if I have parent then give me their ids - this is where the ordering matters making sure the parent is done before it's asked
      )
    }
    
    # if there is a tag required by the model but it is not specified in the xml then I replicate n times the first element 
    required_tags%>%
      purrr::walk(function(r_tag){
        if (is.null(samples[[r_tag]]) & r_tag!="parameters") samples[[r_tag]]$samples <<- rep(settings$run$inputs[[tolower(r_tag)]]$path[1], settings$ensemble$size)
      })
    
    # Let's find the PFT based on site location, if it was found I will subset the ensemble.samples otherwise we're not affecting anything    
    if(!is.null(con)){
      Pft_Site_df <- dplyr::tbl(con, "sites_cultivars")%>%
        dplyr::filter(site_id == settings$run$site$id) %>%
        dplyr::inner_join(dplyr::tbl(con, "cultivars_pfts"), by = "cultivar_id") %>%
        dplyr::inner_join(dplyr::tbl(con, "pfts"), by = c("pft_id" = "id")) %>%
        dplyr::collect() 
      
      site_pfts_names <- Pft_Site_df$name %>% unlist() %>% as.character()
      
      PEcAn.logger::logger.info(paste("The most suitable pfts for your site are the followings:",site_pfts_names))
      #-- if there is enough info to connect the site to pft
      #if ( nrow(Pft_Site_df) > 0 & all(site_pfts_names %in% names(ensemble.samples)) ) ensemble.samples <- ensemble.samples [Pft_Site$name %>% unlist() %>% as.character()]
    }
    # Reading the site.pft specific tags from xml
    site.pfts.vec <- settings$run$site$site.pft %>% unlist %>% as.character
    
    if(!is.null(site.pfts.vec)){
      # find the name of pfts defined in the body of pecan.xml
      defined.pfts <- settings$pfts %>% purrr::map('name') %>% unlist %>% as.character
      # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
      if (length(which(site.pfts.vec %in% defined.pfts)) > 0 )
        ensemble.samples <- ensemble.samples [site.pfts.vec[ which(site.pfts.vec %in% defined.pfts) ]]
      # warn if there is a pft specified in the site but it's not defined in the pecan xml.
      if (length(which(!(site.pfts.vec %in% defined.pfts)))>0) 
        PEcAn.logger::logger.warn(paste0("The following pfts are specified for the siteid ", settings$run$site$id ," but they are not defined as a pft in pecan.xml:",
                                         site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))]))
    }
    
    # if no ensemble piece was in the xml I replicate n times the first element in params
    if ( is.null(samp$parameters) )            samples$parameters$samples <- ensemble.samples %>% purrr::map(~.x[rep(1, settings$ensemble$size) , ])
    # This where we handle the parameters - ensemble.samples is already generated in run.write.config and it's sent to this function as arg - 
    if ( is.null(samples$parameters$samples) ) samples$parameters$samples <- ensemble.samples
    #------------------------End of generating ensembles-----------------------------------
    # find all inputs that have an id
    inputs <- names(settings$run$inputs)
    inputs <- inputs[grepl(".id$", inputs)]
    
    # write configuration for each run of the ensemble
    runs <- data.frame()
    for (i in seq_len(settings$ensemble$size)) {
      if (!is.null(con)) {
        paramlist <- paste("ensemble=", i, sep = "")
        # inserting this into the table and getting an id back
        run.id <- PEcAn.DB::db.query(paste0(
          "INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, ensemble_id, parameter_list) ",
          "values ('", 
          settings$model$id, "', '", 
          settings$run$site$id, "', '", 
          settings$run$start.date, "', '", 
          settings$run$end.date, "', '", 
          settings$run$outdir, "', ", 
          ensemble.id, ", '", 
          paramlist, "') ",
          "RETURNING id"), con = con)[['id']]
        # associate inputs with runs
        if (!is.null(inputs)) {
          for (x in inputs) {
            PEcAn.DB::db.query(paste0("INSERT INTO inputs_runs (input_id, run_id) ",
                                      "values (", settings$run$inputs[[x]], ", ", run.id, ")"), 
                               con = con)
          }
        }
        
      } else {

        run.id <- PEcAn.utils::get.run.id("ENS", PEcAn.utils::left.pad.zeros(i, 5), site.id=settings$run$site$id)

      }
      runs[i, "id"] <- run.id
      
      # create folders (cleaning up old ones if needed)
      if (clean) {
        unlink(file.path(settings$rundir, run.id))
        unlink(file.path(settings$modeloutdir, run.id))
      }
      dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
      dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)
      # write run information to disk
      cat("runtype     : ensemble\n",
          "workflow id : ", workflow.id, "\n",
          "ensemble id : ", ensemble.id, "\n",
          "run         : ", i, "/", settings$ensemble$size, "\n",
          "run id      : ", run.id, "\n",
          "pft names   : ", as.character(lapply(settings$pfts, function(x) x[['name']])), "\n",
          "model       : ", model, "\n",
          "model id    : ", settings$model$id, "\n",
          "site        : ", settings$run$site$name, "\n",
          "site  id    : ", settings$run$site$id, "\n",
          "met data    : ", samples$met$samples[[i]], "\n",
          "start date  : ", settings$run$start.date, "\n",
          "end date    : ", settings$run$end.date, "\n",
          "hostname    : ", settings$host$name, "\n",
          "rundir      : ", file.path(settings$host$rundir, run.id), "\n",
          "outdir      : ", file.path(settings$host$outdir, run.id), "\n",
          file = file.path(settings$rundir, run.id, "README.txt"))
      
      #changing the structure of input tag to what the models are expecting
      for(input_i in seq_along(settings$run$inputs)){
        input_tag <- names(settings$run$inputs)[[input_i]]
        if (!is.null(samples[[input_tag]]))
          settings$run$inputs[[input_tag]][["path"]] <-
            samples[[input_tag]][["samples"]][[i]]
      }

      
      do.call(my.write.config, args = list( defaults = defaults, 
                                            trait.values = lapply(samples$parameters$samples, function(x, n) { x[n, , drop=FALSE] }, n=i), # this is the params
                                            settings = settings, 
                                            run.id = run.id
      )
      )
      cat(run.id, file = file.path(settings$rundir, "runs.txt"), sep = "\n", append = TRUE)
      
    }
    return(invisible(list(runs = runs, ensemble.id = ensemble.id, samples=samples)))
    #------------------------------------------------- if we already have everything ------------------        
  }else{
    #reading retstart inputs
    inputs<-restart$inputs
    run.id<-restart$runid
    new.params<-restart$new.params
    new.state<-restart$new.state
    ensemble.id<-restart$ensemble.id
    
    # Reading the site.pft specific tags from xml
    site.pfts.vec <- settings$run$site$site.pft %>% unlist %>% as.character
    
    if(!is.null(site.pfts.vec)){
      # find the name of pfts defined in the body of pecan.xml
      defined.pfts <- settings$pfts %>% purrr::map('name') %>% unlist %>% as.character
      # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
      if (length(which(site.pfts.vec %in% defined.pfts)) > 0 )
        new.params <- new.params %>% map(~list(.x[[which(site.pfts.vec %in% defined.pfts)]],restart=.x$restart))
      # warn if there is a pft specified in the site but it's not defined in the pecan xml.
      if (length(which(!(site.pfts.vec %in% defined.pfts)))>0) 
        PEcAn.logger::logger.warn(paste0("The following pfts are specified for the siteid ", settings$run$site$id ," but they are not defined as a pft in pecan.xml:",
                                         site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))]))
    }
    
    
    # stop and start time are required by bc we are wrtting them down into job.sh
    for (i in seq_len(settings$ensemble$size)) {
      do.call(my.write_restart, 
              args =  list(outdir = settings$host$outdir, 
                           runid = run.id[[i]], 
                           start.time = restart$start.time,
                           stop.time =restart$stop.time, 
                           settings = settings,
                           new.state = new.state[i, ], 
                           new.params = new.params[[i]], 
                           inputs =list(met=list(path=inputs$samples[[i]])), 
                           RENAME = TRUE)
      )
    }
    params<-new.params
    return(invisible(list(runs = data.frame(id=run.id), ensemble.id = ensemble.id, samples=list(met=inputs)
    )
    ))
  }
  
  
  
} # write.ensemble.configs



#' Function for generating samples based on sampling method, parent or etc
#'
#' @param settings list of PEcAn settings
#' @param method Method for sampling - For now looping or sampling with replacement is implemented
#' @param parent_ids This is basically the order of the paths that the parent is sampled.See Details.
#'
#' @return For a given input/tag in the pecan xml and a method, this function returns a list with $id showing the order of sampling and $samples with samples of that input.
#' @details If for example met was a parent and it's sampling method resulted in choosing the first, third and fourth samples, these are the ids that need to be sent as
#' parent_ids to this function.
#' @export
#'
#' @examples
#' \dontrun{input.ens.gen(settings,"met","sampling")}
#'  
input.ens.gen<-function(settings,input,method="sampling",parent_ids=NULL){
  
  #-- reading the dots and exposing them to the inside of the function
  samples<-list()
  samples$ids<-c()
  #
  if (is.null(method)) return(NULL)
  # parameter is exceptional it needs to be handled spearatly
  if (input=="parameters") return(NULL)
  #-- assing the sample ids based on different scenarios
  if(!is.null(parent_ids)) {
    samples$ids <- parent_ids$ids  
    out.of.sample.size <- length(samples$ids[samples$ids > settings$run$inputs[[tolower(input)]]$path %>% length])
    #sample for those that our outside the param size - forexample, parent id may send id number 200 but we have only100 sample for param
    samples$ids[samples$ids%in%out.of.sample.size] <- sample(settings$run$inputs[[tolower(input)]]$path %>% seq_along(),
                                                              out.of.sample.size,
                                                              replace = T)
  }else if( tolower(method)=="sampling") {
    samples$ids <- sample(settings$run$inputs[[tolower(input)]]$path %>% seq_along(),
                          settings$ensemble$size,
                          replace = T)  
  }else if( tolower(method)=="looping"){
    samples$ids <- rep_len(settings$run$inputs[[tolower(input)]]$path %>% seq_along(), length.out=settings$ensemble$size)
  }
  #using the sample ids
  samples$samples<-settings$run$inputs[[tolower(input)]]$path[samples$ids]
  
  
  
  return(samples)
}
