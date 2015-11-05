
#--------------------------------------------------------------------------------------------------#
##' @name query.format.vars
##' @title Given input_id, return formats table and table of variables and units
##' @param input_id
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
##' 
query.format.vars <- function(input.id,con){
  
  # get input info
  f <- db.query(paste("SELECT * from formats as f join inputs as i on f.id = i.format_id where i.id = ", input.id),con)
  
  # get variable names and units of input data
  fv <- db.query(paste("SELECT variable_id,name,unit from formats_variables where format_id = ", f$id),con)
  colnames(fv) <- c("variable_id", "orig_name", "orig_units")
  
  n <- dim(fv)[1]
  
  # get bety and CF names and units 
  vars <- lapply(1:n, function(i) db.query(paste("SELECT id, name,standard_name,standard_units from variables where id = ", fv$variable_id[i]),con))
  vars <- do.call(rbind, vars)
  colnames(vars) <- c("variable_id", "bety_name", "CF_name", "CF_units")
  df <- merge(fv, vars, by="variable_id")
  
  header <- ifelse(is.na(as.numeric(f$header)),NA,TRUE)
  skip <- ifelse(is.na(as.numeric(f$skip)),0,as.numeric(f$skip))
  
  # merge tables 
  format <- list(orig_name = df$orig_name,
                 orig_units = df$orig_units,
                 bety_name = df$bety_name,
                 CF_name = df$CF_name, 
                 CF_units = df$CF_units,
                 skip = skip, 
                 header = header,
                 na.strings=c("-9999","-6999","9999") # This shouldn't be hardcoded in, but not specified in format table ?
                 )
  return(format)
}