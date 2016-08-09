##' @name query.format.vars
##' @title Given input_id, return formats table and table of variables and units
##' @param input_id
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery , Ankur Desai
##' 
query.format.vars <- function(input.id,con,format.id){

  # get input info either form input.id or format.id, depending which is provided
  # defaults to format.id if both provided
  # also query site information (id/lat/lon) if an input.id
  
  site.id <- NULL
  site.lat <- NULL
  site.lon <- NULL
  
  if (missing(format.id)) {
    f <- db.query(paste("SELECT * from formats as f join inputs as i on f.id = i.format_id where i.id = ", input.id),con)
    site.id <- db.query(paste("SELECT site_id from inputs where id =",input.id),con)
    if (is.data.frame(site.id) && nrow(site.id)>0) {
      site.id <- site.id$site_id
      site.info <- db.query(paste("SELECT id, ST_X(ST_CENTROID(geometry)) AS lon, ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id =",site.id),con)
      site.lat <- site.info$lat
      site.lon <- site.info$lon
    } 
  } else {
    f <- db.query(paste("SELECT * from formats where id = ", format.id),con)
  }
  
  mimetype <- db.query(paste("SELECT * from  mimetypes where id = ", f$mimetype_id),con)[["type_string"]]
  f$mimetype <- tail(unlist(strsplit(mimetype, "/")),1)
  
  # get variable names and units of input data
  fv <- db.query(paste("SELECT variable_id,name,unit,storage_type,column_number from formats_variables where format_id = ", f$id),con)
  if (nrow(fv)>0) {
    colnames(fv) <- c("variable_id", "orig_name", "orig_units", "storage_type", "column_number")
    fv$variable_id <- as.numeric(fv$variable_id)
  
    n <- dim(fv)[1]
  
    vars <- as.data.frame(matrix(NA, ncol=3, nrow=n))
  
    # get bety names and units 

    for(i in 1:n){
      vars[i,] <- as.matrix(db.query(paste("SELECT id, name, units from variables where id = ",
                                 fv$variable_id[i]),con))
    }
    colnames(vars) <- c("variable_id", "bety_name", "bety_units")
    vars$variable_id <- as.numeric(fv$variable_id)
  
  
    # Fill in CF vars
    # This will ultimately be useful when looking at met variables where CF != Bety
    # met <- read.csv(system.file("/data/met.lookup.csv", package= "PEcAn.data.atmosphere"), header = T, stringsAsFactors=FALSE)
  
    #Fill in MstMIP vars 
    #All pecan output is in MstMIP variables 
  
    bety_mstmip <- read.csv(system.file("bety_mstmip_lookup.csv", package= "PEcAn.DB"), header = T, stringsAsFactors=FALSE)
  
    vars_full <- merge(fv, merge(vars, bety_mstmip, by = "bety_name", all.x = TRUE), by="variable_id", all=TRUE) # not sure if all=TRUE is appropriate here or not.
  
    vars_full$pecan_name <- vars_full$mstmip_name
    vars_full$pecan_units <- vars_full$mstmip_units
    ind <- is.na(vars_full$pecan_name)
    vars_full$pecan_name[ind] <- vars_full$bety_name[ind]
    vars_full$pecan_units[ind] <- vars_full$bety_units[ind]
  
    header <- as.numeric(f$header)
    skip <- ifelse(is.na(as.numeric(f$skip)),0,as.numeric(f$skip))
  
    # Final format list
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   vars = vars_full,
                   skip = skip, 
                   header = header,
                   na.strings=c("-9999","-6999","9999"), # This shouldn't be hardcoded in, but not specified in format table ?
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon
                   )
  } else {
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   na.strings=c("-9999","-6999","9999"), # This shouldn't be hardcoded in, but not specified in format table ?
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon
    )
  }
  return(format)
}