##' @title met.files.tree
##' 
##' @description Function will check database for matching met files for a given run
##' dependant on site, machine, and source. It will return a list containing the 
##' start/end dates and id associated with each input record for each stage of met.process.
##' If none exists, it will return an empty object. 
##' 
##' @param site.id object wih site id
##' @param machine.id object with machine id
##' @param met object created in met.process with name of the met source (Ameriflux, NARR,CRUNCEP, etc.)
##' @param met.reg object created in met.process; used for format id
##' @param con connection object
##' 
##' @export
##' @author Tony Gardella

met.files.tree <- function  (site.id,machine.id,met,met.reg, con){
  
  ### Query Database for inputs by site and machine id
  input.file.info <- db.query(paste("SELECT * from inputs as i join dbfiles as d on i.id = d.container_id where i.site_id = ", 
                                    site.id,
                                    "and d.machine_id = ",
                                    machine.id,
                                    "and d.container_type = ",
                                    "'Input'"), con)
  
  if ( !exists("input.file.info") | length(input.file.info) == 0){
    ## Nothing was returned or there was an error 
    logger.error("Unable to find existing Met file records")
    info.list <- NULL
    
  }else{
    
    raw.info <- NULL
    cf.info <- NULL
    gf.info <- NULL
    model.info <- NULL 
    
    ## Cut out files from other met sources
    file.table <- input.file.info[grepl(met,input.file.info$name),]
    
    # Look for Raw Files
    if((met.reg$format$id %in% file.table$format_id)){
      raw.file.info <- file.table[file.table$format_id == met.reg$format$id,]
      ### grab latest created input record start and end dates
      raw.clean.info <- raw.file.info[raw.file.info$updated_at == max(raw.file.info$updated_at),]
      ###Grab start and end date
      raw.info$start_date <- raw.clean.info$start_date
      raw.info$end_date <- raw.clean.info$start_date
      ### grab id of raw record 
      raw.info$raw.input.id <- raw.clean.info$id
    }
    
    # CF files that have raw.id as parent.id
    if(raw.input.id %in% file.table$parent_id){
      cf.file.info <- file.table[file.table$parent_id %in% raw.input.id,]
      # Grab file that was last updated
      cf.clean.info <- cf.file.info[cf.file.info$updated_at == max(cf.file.info$updated_at),]
      ### grab start and end_dates
      cf.info$start_date <- cf.clean.info$start_date
      cf.info$end_date <- cf.clean.info$end_date
      ## grab id of cf record 
      cf.info$cf.input.id <- cf.clean.info$id
    }
    
    # Gapfill files that have cf.id as parent.id
    if(( cf.input.id %in% file.table$parent_id  )){
      gf.file.info <- file.table[file.table$parent_id %in% cf.input.id,]
      # Grab file that was last updated
      gf.clean.info <- gf.file.info[gf.file.info$updated_at == max(gf.file.info$updated_at),]
      ### grab start and end_dates
      gf.info$gf.start_date <- gf.clean.info$start_date
      gf.info$gf.end_date <- gf.clean.info$end_date
      ## grab id of cf record 
      gf.info$gf.input.id <- gf.clean.info$id
    }
    
    # Model specific met files that have gapfill.id as parent.id
    if((gf.input.id %in% file.table$parent_id)){
      model.file.info <- file.table[file.table$parent_id %in% gf.input.id | file.table$parent_id %in% cf.input.id,]
      ### grab start and end_dates
      model.info$model.start_date <- model.info$start_date
      model.info$model.end_date <- model.info$end_date
      ## grab id of model record 
      model.info$model.input.id <- model.info$id
    }
    
    ##Put .info lists in one list
    
    info.list <- eapply(.GlobalEnv,I)[c('raw.info','cf.info','gf.info','model.info')]
    
  }
  
  return(info.list)
  
} ### END oF FUNCTION
