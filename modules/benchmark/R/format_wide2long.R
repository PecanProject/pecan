##' Function to convert wide format to long format
##' 
##' @param out wide format data
##' @param format as returned by query.format.vars
##' @param vars_used data frame mapping `input_name` to `bety_name`
##' @param time.row ignored; value in output is set from `format$vars$storage_type`
##' @return list of updated values
##' @export
##' @author Istem Fer
format_wide2long <- function(out, format, vars_used, time.row){
  
  # GapMacro example:
  # suppose that the wide variable is DBH, DMG (damage status), MORT (mortality status) 
  #
  # out looks like this:
  #
  #########################################################################
  #                                                                       #
  #  diam12  diam14  diam16  dmg12  dmg14  dmg16  mort12  mort14  mort16  #
  #    10      12      13      NA    TL      TL     A       A        D    #
  #                                                                       #
  #########################################################################
  #
  # we want it to look like this:
  #
  ##########################
  #                        #
  #  year  DBH  DMG  MORT  #
  #  2012  10   NA    A    #
  #  2014  12   TL    A    #
  #  2016  13   TL    D    #
  #                        #  
  ##########################
  
  
  # which cols are "wide"
  wide_bety  <- unique(vars_used$bety_name[duplicated(vars_used$bety_name)])
  long_bety  <- vars_used$bety_name[!(vars_used$bety_name %in% wide_bety)]
  dindx      <- vars_used$bety_name %in% wide_bety
  wide_input <- vars_used$input_name[dindx]
  long_input <- colnames(out)[!(colnames(out) %in% wide_input)]
  
  melt_list <- list()
  long_var  <- data.frame(varname = rep(NA, length(wide_bety)),
                          storage_type = rep(NA, length(wide_bety)))
  for(i in seq_along(wide_bety)){
     wide_var  <- wide_bety[i]
     wide_cols <- out[ ,vars_used$bety_name %in% c(wide_var, long_bety)]
     melt_cols <- reshape2::melt(wide_cols, id = long_input, value.name = wide_var) 
     #
     # a sample melt_cols for a wide variable, in this case for DBH, looks like this:
     #
     ##########################
     #                        #
     #  code  variable  value #
     #  FAGR   diam12    10   #
     #  FAGR   diam14    12   #
     #  FAGR   diam16    13   #
     #                        #
     ##########################
     #
     # now we need to replace diam12/14/16 values with 2012, 2014, 2016 respectively
     # and "variable" to some pecan variable name, "year" in this case
     # this information will come from the storage type which is the same for all wide variables of same kind
     # e.g. for this wide var storage type will be "diam:20 year:%Y" *I chose ":" as it is unlikely to be in var names*
     # expression before the space tells what to swap in the values
     # expression after the space tells what the newly created variable is 
     #
     storage_wide <- unique(vars_used$storage_type[vars_used$bety_name == wide_var])
     swap_val     <- gsub(" .*", "", storage_wide)
     new_var      <- gsub(".* ", "", storage_wide)
     # replace "diam" with "20"
     melt_cols$variable <- gsub(gsub(":.*", "", swap_val), gsub(".*:", "", swap_val), melt_cols$variable)
     # replace "variable" with "year"
     colnames(melt_cols)[colnames(melt_cols) == "variable"] <- gsub(":.*", "", new_var)
     # collect info about newly created var
     long_var[i, ] <- c(gsub(":.*", "", new_var), gsub(".*:", "", new_var))
     
     melt_list[[i]] <- melt_cols
  }
  # join sublists
  temp_data <- do.call(cbind, melt_list)
  long_data <- temp_data[, !duplicated(colnames(temp_data))]


  #time.check <- unique(gsub(" .*", "", vars_used$storage_type[dindx]))
  

  # finally, you need to inform "format" about the new structure, vars_used index and update time.row if necessary

  # remove wide rows from format$vars altogether
  format$vars <- format$vars[!(format$vars$input_name %in% wide_input), ]
  # just use one of the wide var row(s)
  wide_rows <- vars_used[dindx,]
  wide_rows <- wide_rows[!duplicated(wide_rows$bety_name),]
  # just for the sake of unit conversion printf in load_data change the input name, not sure if this is necessary
  wide_rows$input_name <- tapply(wide_input, rep(1:nrow(wide_rows), each=nrow(wide_rows)), paste, collapse = ",")
  # empty the storage type and column_number so that it won't break anything downstream, probably it won't anyway
  wide_rows$storage_type  <- ""
  wide_rows$column_number <- ""
  format$vars <- rbind(format$vars, wide_rows)
  
  # finally add the newly created variable(s), only "year" in this case
  new_var <- data.frame(bety_name = "", variable_id = "", input_name = "", input_units = "",
                        storage_type = "", column_number = "", bety_units = "",
                        mstmip_name = "", mstmip_units = "", pecan_name = "", pecan_units = "")
  for(i in 1:nrow(long_var)){
     if(!long_var[i, 1] %in% format$vars$bety_name){ # avoid duplicating
       new_var$bety_name <- new_var$input_name <- new_var$pecan_name <- long_var[i, 1]
       new_var$storage_type <- long_var[i, 2]
       # do we also nee variable_id downstream?
       format$vars <- rbind(format$vars, new_var)
    }
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
  
  
  return(list(long_data = long_data, format = format, vars_used = vars_used, time.row = time.row))
  
} # wide2long

