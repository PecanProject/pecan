##' Load Dataset for Paramater Data Assimilation
##'
##' @title Load Dataset for Paramater Data Assimilation
##' @param input.settings = settings$assim.batch$inputs from pecan.xml or similar
##'
##' @return A list containg the loaded input data, plus metadata
##'
##' @author Ryan Kelly
##' @export
load.pda.data <- function(input.settings) {

  ## load data
  # Outlining setup for multiple datasets, although for now the only option is to assimilate 
  # against a single NEE input
  inputs <- list()
  n.input <- length(input.settings)

  for(i in 1:n.input) {
    inputs[[i]] <- list()
    inputs[[i]]$variable.id <- input.settings[[i]]$data.model$variable.id

    ## Load input based on ID, PATH, or SOURCE
    if(!is.null(input.settings[[i]]$id)) {             # Input specified by ID
      ## Get file path from input id
      inputs[[i]]$input.id <- input.settings[[i]]$id
      file <- db.query(paste0('SELECT * FROM dbfiles WHERE container_id = ', input.settings[[i]]$id), con)
      file <- file.path(file$file_path, file$file_name)

      ## Load store data
      inputs[[i]]$data <- read.csv(file)
    } else if(!is.null(input.settings[[i]]$path)) {    # Input specified by PATH
      # Again, for now works with a single test case, Ameriflux NEE.
      inputs[[i]]$data <- read.csv(input.settings[[i]]$path)
      inputs[[i]]$input.id <- -1
    } else if(!is.null(input.settings[[i]]$source)) {  # Input specified by SOURCE
      # TODO: insert code to extract data from standard sources (e.g. AMF)
    } else {
      logger.error("Must provide ID, PATH, or SOURCE for all data assimilation inputs")
    }
      

    ## Preprocess data
    # TODO: Generalize
    if(as.numeric(inputs[[i]]$variable.id) == 297) {
      ## calculate flux uncertainty parameters
      NEEo <- inputs[[i]]$data$NEE_or_fMDS #data$Fc   #umolCO2 m-2 s-1
      NEEq <- inputs[[i]]$data$NEE_or_fMDSqc #data$qf_Fc
      dTa <- get.change(inputs[[i]]$data$Ta_f)
      flags <- dTa < 3   ## filter data to temperature differences that are less than 3 degrees
      NEE.params <- flux.uncertainty(NEEo,NEEq,flags,bin.num=20)
      inputs[[i]]$b0 <- NEE.params$intercept
      inputs[[i]]$bp <- NEE.params$slopeP
      inputs[[i]]$bn <- NEE.params$slopeN
    }
  } # end loop over files
  
  return(inputs)
}


# In a previous version, inputs specified by path would be automatically added to the DB, 
# but we decided this needs some discussion. Below are code blocks that could be useful
# if that functionality is added again. 

#         in.path <- dirname(input.i$path)
#         in.prefix <- basename(input.i$path)
#         mimetype <- 'text/csv'
#         formatname <- 'AmeriFlux.level4.h'
#         
#         year <- strsplit(basename(input.i$path), "_")[[1]][3]
#         startdate <- as.POSIXlt(paste0(year,"-01-01 00:00:00", tz = "GMT"))
#         enddate <- as.POSIXlt(paste0(year,"-12-31 23:59:59", tz = "GMT"))


#       raw.id <- dbfile.input.insert(in.path=in.path,
#                                     in.prefix=in.prefix, 
#                                     siteid = settings$run$site$id,  
#                                     startdate = startdate, 
#                                     enddate = enddate, 
#                                     mimetype=mimetype, 
#                                     formatname=formatname,
#                                     parentid = NA,
#                                     con = con,
#                                     hostname = settings$run$host$name)
#       input.i$id <- raw.id$input.id



##' Set PDA Settings
##'
##' @title Set PDA Settings
##' @param settings: pecan settings list
##'
##' @return An updated settings list
##'
##' @author Ryan Kelly
##' @export
pda.settings <- function(settings, params.id=NULL, param.names=NULL, prior.id=NULL, chain=NULL,
                         iter=NULL, adapt=NULL, adj.min=NULL, ar.target=NULL, jvar=NULL) {
  # Some settings can be supplied via settings (for automation) or explicitly (interactive). 
  # An explicit argument overrides whatever is in settings, if anything.
  # If neither an argument or a setting is provided, set a default value in settings. 
  
  # Each assignment below includes an explicit type conversion to avoid problems later. 

 
  # params.id: Either null or an ID used to query for a matrix of MCMC samples later
  if(!is.null(params.id)) {
    settings$assim.batch$params.id <- params.id
  }
  if(!is.null(settings$assim.batch$params.id)) {
    settings$assim.batch$params.id <- as.character(settings$assim.batch$params.id)
  }


  # param.names: Names of parameters to assimilate against
  if(!is.null(param.names)) {
    settings$assim.batch$param.names <- param.names
  }
  if(is.null(settings$assim.batch$param.names)) {
    logger.error('Parameter data assimilation requested, but no parameters specified for PDA')
  } else {
    settings$assim.batch$param.names <- as.character(settings$assim.batch$param.names)
  }


  # prior: Either null or an ID used to query for priors later
  if(!is.null(prior.id)) {
    settings$assim.batch$prior.id <- prior.id
  }
  if(!is.null(settings$assim.batch$prior.id)) {
    settings$assim.batch$prior.id <- as.character(settings$assim.batch$prior.id)
  }


  # chain: An identifier for the MCMC chain. Currently not used for anything but a label.
  if(!is.null(chain)) {
    settings$assim.batch$chain <- chain
  }
  if(is.null(settings$assim.batch$chain)) {   # Default
    settings$assim.batch$chain <- 1
  }
  settings$assim.batch$chain <- as.numeric(settings$assim.batch$chain)


  # iter: Number of MCMC iterations. 
  if(!is.null(iter)) {
    settings$assim.batch$iter <- iter
  }
  if(is.null(settings$assim.batch$iter)) {   # Default
    settings$assim.batch$iter <- 100
  }
  settings$assim.batch$iter <- as.numeric(settings$assim.batch$iter)



  # ----- Jump distribution / tuning parameters
  # adapt: How often to adapt the MCMC. Defaults to iter/10
  if(!is.null(adapt)) {
    settings$assim.batch$jump$adapt <- adapt
  }
  if(is.null(settings$assim.batch$jump$adapt)) {   # Default
    settings$assim.batch$jump$adapt <- floor(settings$assim.batch$iter/10)
  }
  settings$assim.batch$jump$adapt <- as.numeric(settings$assim.batch$jump$adapt)


  # adj.min: minimum amount to reduce jump distribution by. 
  if(!is.null(adj.min)) {   # Default
    settings$assim.batch$jump$adj.min <- adj.min
  }
  if(is.null(settings$assim.batch$jump$adj.min)) {   # Default
    settings$assim.batch$jump$adj.min <- 0.1
  }
  settings$assim.batch$jump$adj.min <- as.numeric(settings$assim.batch$jump$adj.min)
  
  
  # ar.target: Target acceptance rate. Can be a single value of vector, one for each variable assimilated against. 
  if(!is.null(ar.target)) {
    settings$assim.batch$jump$ar.target <- ar.target
  }
  if(is.null(settings$assim.batch$jump$ar.target)) {   # Default
    settings$assim.batch$jump$ar.target <- 0.5
  }
  settings$assim.batch$jump$ar.target <- as.numeric(settings$assim.batch$jump$ar.target)


  # jvar: Initial jump variances. Defaults to 1, which is foolish but should be fixed adaptively. 
  if(!is.null(jvar)) {
    settings$assim.batch$jump$jvar <- jvar
  } else if(!is.null(settings$assim.batch$jump$jvar)) {
    # If supplied via XML, will be a list; convert to vector
    settings$assim.batch$jump$jvar <- as.numeric(settings$assim.batch$jump$jvar)
  }
  if(is.null(settings$assim.batch$jump$jvar)) {
    settings$assim.batch$jump$jvar <- rep(1, length(param.names))
  }
  settings$assim.batch$jump$jvar <- as.numeric(settings$assim.batch$jump$jvar)
  
  return(settings)
}








pda.load.priors <- function(settings, con) {
  if(is.null(settings$assim.batch$prior.id)){
    ## by default, use the most recent posterior as the prior
    pft.id <-  db.query(paste0("SELECT id from pfts where name = '",settings$pfts$pft$name,"'"),con)
    priors <-  db.query(paste0("SELECT * from posteriors where pft_id = ",pft.id),con)

    prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id IN (", paste(priors$id, collapse=','), ")"),con)

    prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]

    settings$assim.batch$prior.id <- prior.db$container_id[which.max(prior.db$updated_at)]
  }
  prior.db <- db.query(paste0("SELECT * from dbfiles where container_type = 'Posterior' and container_id = ", settings$assim.batch$prior.id),con)
  prior.db <- prior.db[grep("post.distns.Rdata",prior.db$file_name),]
  load(file.path(prior.db$file_path,"post.distns.Rdata"))
}







pda.create.ensemble <- function(settings, con, workflow.id) {
  if (!is.null(con)) {
    # Identifiers for ensemble 'runtype'
    if(settings$assim.batch$method == "bruteforce") {
      ensemble.type <- "pda.MCMC"
    } else if(settings$assim.batch$method == "emulator") {
      ensemble.type <- "pda.emulator"
    }
    
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    db.query(paste("INSERT INTO ensembles (created_at, runtype, workflow_id) values ('", 
                   now, "', '", ensemble.type,"', ", workflow.id, ")", sep=''), con)
    ensemble.id <- db.query(paste("SELECT id FROM ensembles WHERE created_at='", now, "'", sep=''), con)[['id']]
  } else {
    ensemble.id <- "NA"
  }
  
  return(ensemble.id)
}