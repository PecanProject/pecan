##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name calc.benchmark 
##' @title Calculate benchmarking statistics
##' @param settings object from start.benchmark.runs (for now - we many only need a few variables)
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 

calc.benchmark <- function(settings, con){ #settings file is output from start.benchmark.runs
  
  require(RPostgreSQL)
  require(XML)
  require(data.table)
  
  
  # Query BRR for benchmark ids
  
  bm.id <- settings.bm$run$id #maybe something like that?
  
  # For each bm.id
  #  Query database for: input path, variables, site, start/end dates, metrics, formats
  
  input.path <- db.query.file.path(input.id,host_name,con)
  f_v <- db.query.format.vars(input.id,con)
  format_table <- f_v[[1]]
  vars_names_units <- f_v[[2]]
  
  
  # Need to do something here to narrow down the variables to be used
  # rows <- which(vars_names_units$bety_name %in% calc.vars)
  # vars_names_units <- vars_names_units[rows,]
  
  site  <- db.query.site(input.id, con)
  
  metrics <- db.query(paste("SELECT m.id, m.name from metrics as m JOIN benchmarks_metrics as b 
                            ON m.id = b.metric_id WHERE b.benchmark_id = ", bm.id),con)
  
  # Local or remote
  results <- calc.metrics(input_path, format_table, vars_names_units, model_run, metrics, 
                          start_year=NA, end_year=NA, site=NA)
  
  #  Update benchmark ensemble scores table
  
}

# Return results
return(results)

}

#--------------------------------------------------------------------------------------------------#
##' @name db.query.file.path
##' @title Get file path given id and machine
##' @param input_id
##' @param host_name
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
db.query.file.path <- function(input.id, host_name, con){
  machine.host <- ifelse(host$name == "localhost",fqdn(),host_name)
  machine = db.query(paste0("SELECT * from machines where hostname = '",machine.host,"'"),con)
  dbfile = db.query(paste("SELECT file_name,file_path from dbfiles where container_id =",input.id," and container_type = 'Input' and machine_id =",machine$id),con)
  path <- file.path(dbfile$file_path,dbfile$file_name)
  if(file.exists(path)){
    return(path)
  }else{
    logger.error("Invalid file path")
  }
}


#--------------------------------------------------------------------------------------------------#
##' @name db.query.format.vars
##' @title Given input_id, return formats table and table of variables and units
##' @param input_id
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' 
db.query.format.vars <- function(input.id,con){
  
  f <- db.query(paste("SELECT * from formats as f join inputs as i on f.id = i.format_id where i.id = ", input.id),con)
  
  fv <- db.query(paste("SELECT variable_id,name,unit from formats_variables where format_id = ", f$id),con)
  colnames(fv) <- c("variable_id", "name", "units")
  
  n <- dim(fv)[1]
  vars <- lapply(1:n, function(i) db.query(paste("SELECT id,units,name,standard_name,standard_units from variables where id = ", fv$variable_id[i]),con))
  vars <- do.call(rbind, vars)
  colnames(vars) <- c("variable_id", "bety_units", "bety_name", "CF_name", "CF_units")
  
  vars_names_units <- merge(form_var, vars, by="variable_id")
  vars_names_units <- var_names_units[c("variable_id", "name", "bety_name", "CF_name", "units", "bety_units", "CF_units")]
  
  return(list(f, vars_names_units))
}

#--------------------------------------------------------------------------------------------------#
##' @name db.query.format.vars
##' @title Given site_id, return site table
##' @param site_id
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' 
db.query.site <- function(site.id,con){
  site <- db.query(paste("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry))
                         AS lat FROM sites WHERE id =",site.id),con)
  if(nrow(site)==0){logger.error("Site not found"); return(NULL)}
  if(!(is.na(site$lat)) && !(is.na(site$lat))){
    return(site)
  }
}