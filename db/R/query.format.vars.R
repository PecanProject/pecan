##' @name query.format.vars
##' @title Given input_id, return formats table and table of variables and units
##' @param input_id
##' @param con : database connection
##' @export query.format.vars
##' 
##' @author Betsy Cowdery , Ankur Desai
##' 
query.format.vars <- function(bety,input.id=NA,format.id=NA,var.ids=NA){

  if(is.na(input.id) & is.na(format.id)){PEcAn.utils::logger.error("Must specify input id or format id")}
  
  con <- bety$con
  
  # get input info either form input.id or format.id, depending which is provided
  # defaults to format.id if both provided
  # also query site information (id/lat/lon) if an input.id
  
  site.id <- NULL
  site.lat <- NULL
  site.lon <- NULL
  
  if (is.na(format.id)) {
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
  
  mimetype <- db.query(paste("SELECT * from  mimetypes where id = ",
                             f$mimetype_id),con)[["type_string"]]
  f$mimetype <- tail(unlist(strsplit(mimetype, "/")),1)
  
  # get variable names and units of input data
  fv <- db.query(paste("SELECT variable_id,name,unit,storage_type,column_number from formats_variables where format_id = ", f$id),con)
  
  if(all(!is.na(var.ids))){
    # Need to subset the formats table
    fv <- fv %>% dplyr::filter(variable_id %in% var.ids | storage_type != "") 
  }
  
  if (nrow(fv)>0) {
    colnames(fv) <- c("variable_id", "input_name", "input_units", "storage_type", "column_number")
    fv$variable_id <- as.numeric(fv$variable_id)
    n <- dim(fv)[1]
  
    # get bety names and units 
    vars <- as.data.frame(matrix(NA, ncol=3, nrow=n))
    for(i in 1:n){
      vars[i,] <- as.matrix(db.query(paste("SELECT id, name, units from variables where id = ",
                                 fv$variable_id[i]),con))
    }
    colnames(vars) <- c("variable_id", "bety_name", "bety_units")
    vars$variable_id <- as.numeric(fv$variable_id)
    
    # Fill in input names and units with bety names and units if they are missing
    
    ind1 <- fv$input_name == ""
    fv$input_name[ind1] <- vars$bety_name[ind1]
    ind2 <- fv$input_units == ""
    fv$input_units[ind2] <- vars$bety_units[ind2]
      
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
    
    # Right now I'm making the inappropriate assumption that storage type will be 
    # empty unless it's a time variable. 
    # This is because I haven't come up for a good way to test that a character string is a date format
    
    st <- vars_full$storage_type
    time.row <- which(nchar(st)>1 & substr(st, 1,1) == "%")
    if(length(time.row) == 0) time.row <- NULL
    
    # Final format list
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   vars = vars_full,
                   skip = skip, 
                   header = header,
                   na.strings=c("-9999","-6999","9999"), # This shouldn't be hardcoded in, but not specified in format table ?
                   time.row = time.row,
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon
                   )
  } else {
    format <- list(file_name = f$name,
                   mimetype = f$mimetype,
                   na.strings=c("-9999","-6999","9999"), # This shouldn't be hardcoded in, but not specified in format table ?
                   time.row = NULL,
                   site = site.id,
                   lat = site.lat,
                   lon = site.lon
    )
  }
  return(format)
}