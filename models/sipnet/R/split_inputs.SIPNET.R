#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## split clim file into smaller time units to use in KF
##' @title split_inputs.SIPNET
##' @name  split_inputs.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param multi.settings
##' @param start.time
##' @param stop.time
##' @param ens                ensemble number. default = 1
##' @param outpath  if specified, write output to a new directory. Default NULL writes back to the directory being read
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##' @export
split_inputs.SIPNET <- function(settings, start.time, stop.time, inputs, overwrite = FALSE, outpath = NULL) {
  #browser()
  #### Get met paths
  met <- inputs
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath)
  

  file <- NA
  names(file) <- paste(start.time, "-", stop.time)
  
  #Changing the name of the files, so it would contain the name of the hour as well.
  file <- paste0(outpath, "/", prefix, ".",
                 paste0(start.time%>% as.character() %>% gsub(' ',"_",.),
                        "-",
                        stop.time%>% as.character() %>% gsub(' ',"_",.)), ".clim")
  
  if(file.exists(file) & !overwrite){
    return(file)
  }

  dat <- read.table(met, header = FALSE)
  #strptime(paste("1976", 1), format="%Y %j")

  #@Hamze, I added the Date variable by using year, doy, and hour and filtered the clim based that and then removed it afterwards.
  dat<-dat %>% 
    mutate(Date = strptime(paste(V2, V3), format = "%Y %j",   tz = "EST")%>% as.POSIXct()) %>%
    mutate(Date = as.POSIXct(paste0(Date,  V4, ":00"), format = "%Y-%m-%d %H:%M", tz = "EST")) %>% 
    filter(Date >= start.time, Date < stop.time) %>% 
    dplyr::select(-Date)
  
  
  ###### Write Met to file
  write.table(dat, file, row.names = FALSE, col.names = FALSE)

  ###### Output input path to inputs
  #settings$run$inputs$met$path <- file
  return(file)
} # split_inputs.SIPNET
