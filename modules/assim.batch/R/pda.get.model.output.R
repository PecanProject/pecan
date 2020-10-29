##' Get Model Output for PDA
##'
##' @title Get Model Output for PDA
##' @param settings PEcAn settings list
##' @param run.id run ID
##' @param bety bety list
##' @param inputs inputs list
##' @param external.formats format list
##'
##' @return A list containing model outputs extracted to correspond to each observational
##'         dataset being used for PDA. 
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.get.model.output <- function(settings, run.id, bety, inputs, external.formats = NULL) {
  
  input.info <- settings$assim.batch$inputs
  
  start.year <- strftime(settings$run$start.date,"%Y")
  end.year <- strftime(settings$run$end.date,"%Y")
  
  model.out <- list()
  n.input <- length(inputs)
  
  for(k in 1:n.input){
    # if there is a deriation is requested this line takes care of it
    variable <- lapply(input.info[[k]]$variable.name, PEcAn.utils::convert.expr)
    # convert.expr returns variable names in the data and in the model
    # variable names that correspond to model outputs will be in the variable$eqn
    variable.name <- lapply(inputs[[k]]$variable.name, `[[`, "variable.eqn")
    # get the variable names, e.g. 'TotalSoil', 'Litter'
    model.var <- lapply(variable.name, `[[`, "variables")
    # get the derivation expression, e.g. 'TotalSoil-Litter'
    # if no derivation is requested expr will be the same as variable name
    expr <- lapply(variable.name, `[[`, "expression")
    
    if(is.null(bety$con)){
      format <- external.formats[[k]]
    }else{
      format <- PEcAn.DB::query.format.vars(bety = bety,
                                            input.id = settings$assim.batch$inputs[[k]]$input.id)
    }

    for(l in seq_along(model.var)){
      
      if(length(model.var[[l]][model.var[[l]] %in% format$vars$bety_name]) != 0){
        
        # convert names for model outputs
        expr[[l]] <- gsub(model.var[[l]][model.var[[l]] %in% format$vars$bety_name],
                          format$vars$pecan_name[format$vars$bety_name %in% model.var[[l]][model.var[[l]] %in% format$vars$bety_name]],
                          expr[[l]]) 
        
        model.var[[l]][model.var[[l]] %in% format$vars$bety_name] <- 
          format$vars$pecan_name[format$vars$bety_name %in% model.var[[l]][model.var[[l]] %in% format$vars$bety_name]]
        
        
      }

      # this is only for FC-NEE as we are using them interchangably when NEE isn't present, e.g. Ameriflux data  
      # FC - NEE specific hack
      if(any(model.var[[l]] %in% c("FC"))){
        model.var[[l]][model.var[[l]] %in% c("FC")] <- "NEE" 
        expr[[l]] <- gsub("FC", "NEE", expr[[l]]) 
      }  
    }
    
    # prepare model output variable names
    vars.used <- unlist(model.var)
    # UST is never in the model outputs
    vars.used <- vars.used[!vars.used %in% c("UST")]
    
    # We also want 'time' from model outputs for aligning step
    vars <- c("time", vars.used)  
    
    
    
    # read model output
    model.raw <- as.data.frame(PEcAn.utils::read.output(run.id, outdir = file.path(settings$modeloutdir, run.id),
                                           start.year, end.year, variables = vars))
    
    if(length(model.raw) == 0 | all(is.na(model.raw))) {   # Probably indicates model failed entirely
      out <- list()
      out$model.out <- NA
      return(out)
    }
    
    
    # normally there is only one output variable (derived or not) corresponding to data
    # as we split different inputs in the inputs list in the beginning (see that here we loop over the n.input)
    # for Ameriflux we have UST passed through within the same tag which is only data-related
    # but this generalizes it anyway :
    # figure out which variable names to be used in the derivation expression (i.e. non-UST)
    # and calculate the derived variable
    vars.used.ind <- which(sapply(model.var, function(v) all(v %in% vars)) == TRUE)
    sapply(model.var[[vars.used.ind]], function(x) assign(x, model.raw[x], envir = .GlobalEnv))
    out <- eval(parse(text = expr[[vars.used.ind]]))
    
    
    # prepare for the variables that is going to be used in align_data
    # change data variable names (e.g. "LE") to model output variable names (e.g. "Qle")
    data.var <- sapply(inputs[[k]]$variable.name, `[[`, "variable.drv")
    
    data.var[data.var %in% format$vars$input_name] <- 
      format$vars$pecan_name[format$vars$input_name %in% data.var[data.var %in% format$vars$input_name]]
    
    # UST is never in the model outputs
    data.var <- data.var[!data.var %in% c("UST")]
    
    colnames(out) <- data.var
    model <- data.frame(time = model.raw$time, out)
    
    ## Handle model time
    # the model output time is in days since the beginning of the year
    model.secs <- udunits2::ud.convert(model$time, "days" ,"seconds")
    
    # seq.POSIXt returns class "POSIXct"
    # the model output is since the beginning of the year but 'settings$run$start.date' may not be the first day of the year, using lubridate::floor_date
    if(diff(model.secs)[1] != 0){
      model$posix <- seq.POSIXt(from = as.POSIXlt(settings$run$start.date, tz="GMT"), by = round(diff(model.secs)[1]), length.out = length(model$time))
    }else{
      # yearly output
      model$posix <- seq.POSIXt(from = as.POSIXlt(settings$run$start.date, tz="GMT"), by = "year", length.out = length(model$time))
    }
    
    dat <- PEcAn.benchmark::align_data(model.calc = model, obvs.calc = inputs[[k]]$data, var = data.var, align_method = inputs[[k]]$align.method)

    model.out[[k]]  <- dat[,colnames(dat) %in% paste0(data.var,".m"), drop = FALSE]
    inputs[[k]]$obs <- dat[,colnames(dat) %in% paste0(data.var,".o"), drop = FALSE][[1]]
    inputs[[k]]$n   <- sum(!is.na(inputs[[k]]$obs))
    colnames(model.out[[k]]) <- data.var
  }
  
  return(list(model.out = model.out, inputs = inputs))
}
