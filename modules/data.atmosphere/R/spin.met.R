#' Spin-up meteorology
#'
#' @param in.path    met input folder path
#' @param in.prefix  met input file prefix (shared by all annual files, can be "") 
#' @param start_date start of met
#' @param end_date   end of met
#' @param nyear      number of years of spin-up, default 1000
#' @param nsample    sample the first nsample years of met, default 50
#' @param resample   resample (TRUE, default) or cycle (FALSE) meteorology
#' @param run_start_date date the run itself starts, which can be different than the start of met
#' @param overwrite whether to replace previous resampling
#' 
#' @details 
#' spin.met works by creating symbolic links to the sampled met file, 
#' rather than copying the whole file. Be aware that the internal dates in 
#' those files are not modified. Right now this is designed to be called within
#' met2model.[MODEL] before the met is processed (it's designed to work with annual CF
#' files not model-specific files) for example with models that process met
#' into one large file
#'
#' @return updated start date
#' @export
#'
#' @examples
#' start_date <- "0850-01-01 00:00:00"
#' end_date   <- "2010-12-31 23:59:59"
#' nyear      <- 10
#' nsample    <- 50
#' resample   <- TRUE
#' 
#' \dontrun{
#' if(!is.null(spin)){
#'    ## if spinning up, extend processed met by resampling or cycling met
#'    start_date <- PEcAn.data.atmosphere::spin.met(in.path,in.prefix,start_date,end_date,nyear,nsample,resample)
#' }
#' }
spin.met <- function(in.path, in.prefix, start_date, end_date, nyear = 1000, nsample = 50, resample = TRUE, run_start_date = start_date, overwrite = TRUE){
  
  ### input checking
  
  # paths
  if(missing(in.path) | is.null(in.path)){
    in.path <- "./"  ## if path is missing, assume current working directory 
  }
  if(missing(in.prefix) | is.null(in.prefix)){
    in.prefix <- ""  ## if prefix is missing, assume blank (files just YYYY.nc)
  }

  # dates
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  avail.years <- start_year:end_year
  
  # spin settings
  if(missing(nyear)|is.null(nyear) | is.na(nyear)) nyear <- 1000
  nyear <- as.numeric(nyear)
  if(missing(nsample)|is.null(nsample) | is.na(nsample)) nsample <- 50
  nsample <- as.numeric(nsample)
  nsample <- min(nsample,length(avail.years))
  avail.years <- avail.years[seq_len(nsample)]
  if(missing(resample) | is.null(resample)|is.na(resample)) resample <- TRUE
  resample <- as.logical(resample)
  spin_start_date <- as.POSIXct(run_start_date,"UTC") - lubridate::years(nyear)
  
  ### define the met years to sample
  new_year <- seq(lubridate::year(spin_start_date),by=1,length.out=nyear)
  is.leap <- lubridate::leap_year(avail.years)
  spin_year <- NA
  if(resample){
    for(t in seq_along(new_year)){
      if(lubridate::leap_year(new_year[t])){
        spin_year[t] <- sample(avail.years[is.leap],size = 1)
      } else {
        spin_year[t] <- sample(avail.years[!is.leap],size = 1)
      }
    }
  } else {
    spin_year <- rep(avail.years,length.out=nyear)
  }
  
  ## loop over spin-up years
  for(t in seq_along(new_year)){
    
    spin_year_txt <- formatC(spin_year[t], width = 4, format = "d", flag = "0")
    source.file <- file.path(in.path,paste0(in.prefix,spin_year_txt,".nc"))
    
    new_year_txt <- formatC(new_year[t], width = 4, format = "d", flag = "0")
    target.file <- file.path(in.path,paste0(in.prefix,new_year_txt,".nc"))
    
    if(overwrite){
      system2("rm",target.file)
    }
    
    ## check if a met file already exists
    if(!file.exists(target.file)){
      ## if not, create a symbolic link to the year to be sampled
      file.symlink(from = source.file,target.file)
    }
    
  }
  
  ## return new start_date
  return(spin_start_date)

}
