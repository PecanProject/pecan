ensemble.gen<-function(settings,nens=10,
                       metF=F,
                       paramsF=T,
                       IC=NULL,
                       years=c(),
                       When=NULL,
                       timez="UTC",
                       restart=NULL){
  
  #-- read settings----------------------------------------------------
  model      <- settings$model$type
  write      <- settings$database$bety$write <-FALSE
  defaults   <- settings$pfts
  outdir     <- settings$modeloutdir # currently model runs locally, this will change if remote is enabled
  rundir     <- settings$host$rundir
  host       <- settings$host
  forecast.time.step <- settings$state.data.assimilation$forecast.time.step  #idea for later generalizing
  #  nens       <- as.numeric(settings$state.data.assimilation$n.ensemble)
  processvar <- settings$state.data.assimilation$process.variance
  sample_parameters <- settings$state.data.assimilation$sample.parameters
  var.names <- unlist(sapply(settings$state.data.assimilation$state.variable, 
                             function(x) {
                               x$variable.name
                             }, 
                             USE.NAMES = FALSE), 
                      use.names = FALSE)
  names(var.names) <- NULL

      #--get model specific functions
      do.call("require", list(paste0("PEcAn.", model)))
      my.write.config  <- paste0("write.config.", model)
      my.read_restart  <- paste0("read_restart.", model)
      my.write_restart <- paste0("write_restart.", model)
      my.split_inputs  <- paste0("split_inputs.", model)
      
      # models that don't need split_inputs, check register file for that
      register.xml <- system.file(paste0("register.", model, ".xml"), package = paste0("PEcAn.", model))
      register <- XML::xmlToList(XML::xmlParse(register.xml))
      no_split <- !as.logical(register$exact.dates)
      
      if (!exists(my.write.config)) {
        PEcAn.logger::logger.warn(my.write.config, "does not exist")
        PEcAn.logger::logger.severe("please make sure that the PEcAn interface is loaded for", model)
      }
      
      if (!exists(my.split_inputs)  &  !no_split) {
        PEcAn.logger::logger.warn(my.split_inputs, "does not exist")
        PEcAn.logger::logger.severe("please make sure that the PEcAn interface is loaded for", model)
      }
  # See if it's restart or a new fresh simulation-------------    
  if (is.null(restart)){
      if (!is.null(rundir))
        dir.create(rundir,recursive=TRUE) # remote will give warning

      #-- open database connection-----------------------
      if (write) {
        con <- try(db.open(settings$database$bety), silent = TRUE)
        if (is(con, "try-error")) {
          con <- NULL
        } else {
          on.exit(db.close(con))
        }
      } else {
        con <- NULL
      }
      #-- get new workflow ids--------------------------------------------------
      if ("workflow" %in% names(settings)) {
        workflow.id <- settings$workflow$id
      } else {
        #    workflow.id <- -1
        settings <- check.workflow.settings(settings,con)
        workflow.id <- settings$workflow$id
        PEcAn.logger::logger.info("new workflow ID - ",workflow.id)
      }
      
      #-- create ensemble ids------------------------------------------------
      if (!is.null(con)) {
        # write ensemble first
        now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", now, 
                       "', 'EnKF', ", workflow.id, ")", sep = ""), con)
        ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep = ""), 
                                con)[["id"]]
      } else {
        ensemble.id <- -1
      }
         #-- Reading met ensembles ------------------------------------------------------------------
      #  if(is.null(restart) & is.null(restart$ens.inputs)){
      #    ens.inputs <- sample_met(settings,nens)
      #}else {
      #    ens.inputs <- restart$ens.inputs
      #}
      #-- putting ensembles into inputs list

      #---------------------------------------- HACK FOR NOT LOOKING FOR WEATHE ENS
      no_split<-T # hamze
      inputs <- list()
      ens.inputs<-settings$run$inputs$met[["path"]]
      #-----------------------------------------------------------------------------
      for(i in seq_len(nens)){
        if(no_split){
          inputs[[i]] <- ens.inputs[[1]] # passing settings$run$inputs$met$path is the same thing, just following the logic despite the hack above
        }else{
          ### get only necessary ensemble inputs. Do not change in analysis
          #ens.inputs[[i]] <- get.ensemble.inputs(settings = settings, ens = sampleIDs[i])
          ### model specific split inputs
          inputs[[i]] <- do.call(my.split_inputs, 
                                 args = list(settings = settings, 
                                             start.time = settings$run$start.date, 
                                             stop.time = as.Date(names(obs.mean)[1]),#settings$run$end.date,
                                             inputs = ens.inputs[[i]]))#,
          #                                       outpath = file.path(rundir,paste0("met",i))))
        }
        
        
      }
      
      

      #-- Load Parameters -------------------------------------------------
      suppressMessages({
        
      
        get.parameter.samples(settings, ens.sample.method = settings$ensemble$method)  ## Aside: if method were set to unscented, would take minimal changes to do UnKF
        load(file.path(settings$outdir, "samples.Rdata"))  ## loads ensemble.samples
      })
      if ("env" %in% names(ensemble.samples)) {
        ensemble.samples$env <- NULL
      }
      #-- For making the ensembls this makes the new params - trait values    
      params <- list()
      for (i in seq_len(nens)) {
        if (sample_parameters == TRUE) {
          params[[i]] <- lapply(ensemble.samples, function(x, n) {
            x[i, ]
          }, n = i)
        } else {
          params[[i]] <- ensemble.samples
        }
      } 
      

      #------------------------------------------------------------------------------------------------  
      #-- Writting the configs and for all ensumbles and run - Loop---------------------
      #-----------------------------------------------------------------------------------------------  
      run.id <- list()
      for (i in seq_len(nens)) {
        ## set RUN.ID
        if (!is.null(con)) {
          now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          paramlist <- paste("EnKF:", i)
          run.id[[i]] <- db.query(paste0("INSERT INTO runs (model_id, site_id, start_time, finish_time, outdir, created_at, ensemble_id,", 
                                         " parameter_list) values ('", settings$model$id, "', '", settings$run$site$id, "', '", 
                                         settings$run$start.date, "', '", settings$run$end.date, "', '", settings$outdir, "', '", 
                                         now, "', ", ensemble.id, ", '", paramlist, "') RETURNING id"), con)
        } else {
          run.id[[i]] <- paste("EnKF", i, sep = ".")
        }
        suppressWarnings({
          dir.create(file.path(settings$rundir, run.id[[i]]), recursive = TRUE)
          dir.create(file.path(settings$modeloutdir, run.id[[i]]), recursive = TRUE)
        })

        # See if there is coming as the IC------------------
        if(is.null(IC)) {
          ic.arg<-NULL
        }else{
          ic.arg<-IC[i,]
        }
        
        do.call(what = my.write.config, args = list(defaults = NULL, 
                                                    trait.values = params[[i]], 
                                                    settings = settings, 
                                                    run.id = run.id[[i]], 
                                                    inputs = inputs[[i]], 
                                                    IC = ic.arg)
        )
  
        ## write a README for the run
        cat("runtype     : sda.enkf\n",
            "workflow id : ", as.character(workflow.id), "\n",
            "ensemble id : ", as.character(ensemble.id), "\n",
            "ensemble    : ", i, "\n",
            "run id      : ", as.character(run.id[[i]]), "\n",
            "pft names   : ", as.character(lapply(settings$pfts, function(x) x[["name"]])), "\n",
            "model       : ", model, "\n",
            "model id    : ", settings$model$id, "\n",
            "site        : ", settings$run$site$name, "\n",
            "site  id    : ", settings$run$site$id, "\n",
            "met data    : ", inputs$met$path, "\n",
            "start date  : ", settings$run$start.date, "\n",
            "end date    : ", settings$run$end.date, "\n",
            "hostname    : ", settings$host$name, "\n",
            "rundir      : ", file.path(settings$host$rundir, run.id[[i]]), "\n",
            "outdir      : ", file.path(settings$host$outdir, run.id[[i]]), "\n",
            file = file.path(settings$rundir, run.id[[i]], "README.txt"), 
            sep='')
      }
      
      ## add the jobs to the list of runs
      cat(as.character(unlist(run.id)), 
          file = file.path(settings$rundir, "runs.txt"),
          sep = "\n", 
          append = FALSE)
      #-Now reading the results---------------------------------------------------------------------------------------
      new.params <- params
  }else{
    #cat("\n Here in restart ense generator------ \n")
    #reading retsrat inputs
    inputs<-restart$inputs
    run.id<-restart$runid
    new.params<-restart$new.params
    new.state<-restart$new.state

      for (i in seq_len(nens)) {
        
        do.call(my.write_restart, 
                args =  list(outdir = outdir, 
                             runid = run.id[[i]], 
                             start.time = restart$start.time,
                             stop.time =restart$stop.time, 
                             settings = settings,
                             new.state = new.state[i, ], 
                             new.params = new.params[[i]], 
                             inputs = inputs[[i]], 
                             RENAME = TRUE)
                )
      }
   # cat('Here9 \n')
     params<-new.params
  }
  #save(params,file = paste0("PromisThisWillneverHappenAgain",Sys.time(),".Rdata"))
  ## running the model model runs-----------------------------------------------------------------------------
  PEcAn.remote::start.model.runs(settings, write)
  #cat('Here6 \n')
  # foreach ensumble
  seq_len(nens)%>%
    purrr::map(function(i){
     if(length(years)==0) year(settings$run$start.date):year(settings$run$end.date)->years
      #for each year in simulation
      years%>%
        purrr::map(function(year.out){
          X_tmp <- vector("list", 2) 
          suppressMessages(
          X_tmp <- do.call(my.read_restart, args = list(outdir = outdir, 
                                                        runid = run.id[[i]], 
                                                        stop.time = paste0(year.out,"/12/31"), 
                                                        settings = settings, 
                                                        var.names = var.names, 
                                                        params = params[[i]],
                                                        When=When,
                                                        timez=timez
          )
          )
          )
          # this checks to see the read restart function sent out the params or not 
          if (!is.null(X_tmp[[2]])){new.params[[i]] <<- (X_tmp[[2]]) }else{new.params[[i]] <<- NULL }
          #collecting the params used for this ensemble
          
          return(X_tmp[[1]])
        })%>%setNames(years)->sims
      return(sims)
    })%>%setNames(unlist(run.id[seq_len(nens)]))->model.output
  

  if (length(new.params)>0) setNames(new.params,unlist(run.id[seq_len(nens)]))->new.params
  #cat("End of the ens gen ---------------------- \n")  
  # states will be in X, but we also want to carry some deterministic relationships to write_restart
  # these will be stored in params
  
  return(list(ensembles=model.output,params=new.params,run.id=run.id,inputs=inputs))
  
}#end of function