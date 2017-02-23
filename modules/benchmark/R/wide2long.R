##' @name wide2long
##' @title wide2long
##' @export
##' @author Istem Fer
##' Function to convert wide format to long format
wide2long <- function(out, format, vars_used, time.row, vars.used.index){
  
  # which cols are "wide"
  dindx <- vars_used$bety_name == unique(vars_used$bety_name[duplicated(vars_used$bety_name)])
  wide.vars <- vars_used$input_name[dindx]
  long.vars <- colnames(out)[!(colnames(out) %in% wide.vars)]
  mout      <- reshape2::melt(out, id = long.vars) 
  
  # GapMacro example:
  # suppose that the wide variable is DBH, mout would look something like this:
  ####################
  # variable  value  #  
  #  diam12     12   #
  #  diam14     10   #
  #  diam16     23   #
  ####################
  
  # diam12/14/16 are the column headers in the input file
  # you probably want to rename "value" column as "DBH" here
  colnames(mout)[colnames(mout) == "value"] <- unique(vars_used$bety_name[duplicated(vars_used$bety_name)])
  
  # then you need to change "variable" to some pecan variable name, "year" in this case
  # and the diam12/14/16 to 2012, 2014, 2016 respectively
  # if storage types are "year 2012" / "year 2014" / "year 2016" in the format for those columns
  #you can replace diam12/14/16 with values after the space
  mout$variable <- as.character(mout$variable)
  for(v in seq_along(vars_used$storage_type[dindx])){
    mout$variable[mout$variable == wide.vars[v]] <- gsub(".* ", "", vars_used$storage_type[dindx][v])
  }
  
  # change the column name with the pecan name before the space
  colnames(mout)[colnames(mout) == "variable"] <- unique(gsub(" .*", "", vars_used$storage_type[dindx]))
  # are we creating a new time column or columns?
  time.check <- unique(gsub(" .*", "", vars_used$storage_type[dindx]))
  
  # now it changed to this
  ####################
  #  year    DBH     #  
  #  2012     12     #
  #  2014     10     #
  #  2016     23     #
  ####################
  # finally, you need to inform "format" about the new structure, vars_used index and update time.row if necessary

    # remove wide rows from format$vars altogether
  format$vars <- format$vars[!(format$vars$input_name %in% wide.vars), ]
  # just use one of the wide var rows
  wide_row <- vars_used[dindx,][1,]
  # just for the sake of unit conversion printf in load_data change the input name, not sure if this is necessary
  wide_row$input_name <- paste(vars_used$input_name[dindx], collapse = ",")
  # empty the storage type and column_number so that it won't break anything downstream, probably it won't anyway
  wide_row$storage_type  <- ""
  wide_row$column_number <- ""
  format$vars <- rbind(format$vars, wide_row)
  
  # finally if you add a new time column now, add it to format$vars
  # probably could have been handled in a more sophisticated way
  # this is to remind us that time.row needs to be handled
  if(time.check %in% c("year")){
    
    # just declaring
    time_row <- vars_used[dindx,][1,]
    
    if(time.check == "year"){
      time_row[1, ] <- ""
      time_row$bety_name <- time_row$input_name <- time_row$pecan_name <- "year"
      time_row$variable_id <- 382
      time_row$storage_type <- "%Y"
    }
    
    format$vars <- rbind(format$vars, time_row)
  }
   
  
  # update time.row
  st <- format$vars$storage_type
  time.row <- which(nchar(st)>1 & substr(st, 1,1) == "%")
  if(length(time.row) == 0){
    format$time.row <- NULL
    time.row <- NULL
  }else{
    format$time.row <- time.row
  }
  
  # update vars_used
  vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
  vars_used <- format$vars[vars.used.index, ]
  
  
  return(list(mout, format, vars_used, time.row))
} # wide2long