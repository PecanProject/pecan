## split clim file into smaller time units to use in KF
##' @title split_inputs.SIPNET
##' @name  split_inputs.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param settings PEcAn settings object
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param inputs list of model inputs to use in write.configs.SIPNET
##' @param overwrite Default FALSE
##' @param outpath if specified, write output to a new directory. Default NULL writes back to the directory being read
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##'
##' @importFrom dplyr %>%
##' @export
split_inputs.SIPNET <- function(settings, start.time, stop.time, inputs, overwrite = FALSE, outpath = NULL) {
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

  input.dat <- utils::read.table(met, header = FALSE)


  #@Hamze, I added the Date variable by using year, doy, and hour and filtered the clim based that and then removed it afterwards.
  dat<-input.dat %>% 
    dplyr::mutate(Date = strptime(paste(V2, V3), format = "%Y %j",   tz = "UTC")%>% as.POSIXct()) %>%
    dplyr::mutate(Date = as.POSIXct(paste0(Date,  ceiling(V4), ":00"), format = "%Y-%m-%d %H:%M", tz = "UTC")) %>% 
    dplyr::filter(Date >= start.time, Date < stop.time) %>% 
    dplyr::select(-Date)
  
  
  ###### Write Met to file
  utils::write.table(dat, file, row.names = FALSE, col.names = FALSE)

  ###### Output input path to inputs
  #settings$run$inputs$met$path <- file
  return(file)
} # split_inputs.SIPNET
