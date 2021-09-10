write.ensemble.configs <- function(defaults, ensemble.samples, settings, model, 
                                   clean = FALSE, write.to.db = TRUE,restart=NULL) {
  
  con <- NULL
  my.write.config <- paste("write.config.", model, sep = "")
  my.write_restart <- paste0("write_restart.", model)
  
  if (is.null(ensemble.samples)) {
    return(list(runs = NULL, ensemble.id = NULL))
  }
  
  # See if we need to write to DB
  write.to.db <- as.logical(settings$database$bety$write)
  
  if (write.to.db) {
    # Open connection to database so we can store all run/ensemble information
    con <-
      try(PEcAn.DB::db.open(settings$database$bety))
    on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)
    
    # If we fail to connect to DB then we set to NULL
    if (inherits(con, "try-error"))  {
      con <- NULL
      PEcAn.logger::logger.warn("We were not able to successfully establish a connection with Bety ")
    }
  }
  
  
  
  # Get the workflow id
  if (!is.null(settings$workflow$id)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }
  #------------------------------------------------- if this is a new fresh run------------------  
  if (is.null(restart)){
    # create an ensemble id
    if (!is.null(con) && write.to.db) {
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
    #-------------------------generating met/param/soil/veg/... for all ensembles----
    if (!is.null(con)){
      #-- lets first find out what tags are required for this model
      required_tags <- dplyr::tbl(con, 'models') %>%
        dplyr::filter(id == !!as.numeric(settings$model$id)) %>%
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
        dplyr::filter(site_id == !!settings$run$site$id) %>%
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
    
    if (!is.null(site.pfts.vec)) {
      # find the name of pfts defined in the body of pecan.xml
      defined.pfts <-
        settings$pfts %>% purrr::map('name') %>% unlist %>% as.character
      # subset ensemble samples based on the pfts that are specified in the site and they are also sampled from.
      if (length(which(site.pfts.vec %in% defined.pfts)) > 0)
        ensemble.samples <-
          ensemble.samples [site.pfts.vec[which(site.pfts.vec %in% defined.pfts)]]
      # warn if there is a pft specified in the site but it's not defined in the pecan xml.
      if (length(which(!(site.pfts.vec %in% defined.pfts))) > 0)
        PEcAn.logger::logger.warn(
          paste0(
            "The following pfts are specified for the siteid ",
            settings$run$site$id ,
            " but they are not defined as a pft in pecan.xml:",
            site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))],
            collapse = ","
          )
        )
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
      if (!is.null(con) && write.to.db) {
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
          "workflow id : ", format(workflow.id, scientific = FALSE), "\n",
          "ensemble id : ", format(ensemble.id, scientific = FALSE), "\n",
          "run         : ", i, "/", settings$ensemble$size, "\n",
          "run id      : ", format(run.id, scientific = FALSE), "\n",
          "pft names   : ", as.character(lapply(settings$pfts, function(x) x[["name"]])), "\n",
          "model       : ", model, "\n",
          "model id    : ", format(settings$model$id, scientific = FALSE), "\n",
          "site        : ", settings$run$site$name, "\n",
          "site  id    : ", format(settings$run$site$id, scientific = FALSE), "\n",
          "met data    : ", samples$met$samples[[i]], "\n",
          "start date  : ", as.character(settings$run$start.date), "\n",
          "end date    : ", as.character(settings$run$end.date), "\n",
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
      
      source("/projectnb/dietzelab/dongchen/dongchenpecan/models/sipnet/R/write.configs.SIPNET.R")
      do.call(my.write.config, args = list( defaults = defaults, 
                                            trait.values = lapply(samples$parameters$samples, function(x, n) { x[n, , drop=FALSE] }, n=i), # this is the params
                                            settings = settings, 
                                            run.id = run.id
      )
      )
      cat(format(run.id, scientific = FALSE), file = file.path(settings$rundir, "runs.txt"), sep = "\n", append = TRUE)
      
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