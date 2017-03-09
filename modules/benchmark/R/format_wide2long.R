##' @name format_wide2long
##' @title format_wide2long
##' 
##' @param out wide format data
##' @param format as returned by query.format.vars
##' @param vars_used 
##' @param time.row
##' @return list of updated values
##' @export
##' @author Istem Fer
##' Function to convert wide format to long format
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
  for(i in seq_along(wide_bety)){
     wide_var  <- wide_bety[i]
     wide_cols <- out[ ,vars_used$bety_name %in% c(wide_var, long_bety)]
     melt_cols <- reshape2::melt(wide_cols, id = long_input) 
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
     # you probably want to rename "value" column as "DBH" here
     colnames(melt_cols)[colnames(melt_cols) == "value"] <- wide_var
     #
     # now we need to replace diam12/14/16 values with 2012, 2014, 2016 respectively
     #
     # ...
     # TO BE FILLED
     # ...
     #
     melt_list[[i]] <- melt_cols
  }

  # ...
  # TO BE FILLED
  # ...
  
  
  return(list(mout = mout, format = format, vars_used = vars_used, time.row = time.row))
} # wide2long