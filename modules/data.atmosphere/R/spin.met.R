#' Spin-up meteorology
#'
#' @param in.path    met input folder path
#' @param in.prefix  met input file prefix (shared by all annual files, can be "") 
#' @param start_date start of real met & run
#' @param end_date   end of run
#' @param nyear      number of years of spin-up, default 1000
#' @param nsample    sample the first nsample years of met, default 50
#' @param resample   resample (TRUE, default) or cycle (FALSE) meteorology
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
spin.met <- function(in.path,in.prefix,start_date,end_date,nyear,nsample,resample=TRUE){
  
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
  if(missing(nsample)|is.null(nsample) | is.na(nsample)) nsample <- 50
  nsample <- min(nsample,length(avail.years))
  if(missing(resample) | is.null(resample)|is.na(resample)) resample <- TRUE
  spin_start_date <- as.POSIXct(start_date,"UTC") - lubridate::years(nyear)
  
  ### define the met years to sample
  if(resample){
    spin_year <- sample(avail.years[1:nsample],
                        size = nyear,replace = TRUE)
  } else {
    spin_year <- rep(avail.years[1:nsample],length.out=nyear)
  }
  new_year <- seq(lubridate::year(spin_start_date),by=1,length.out=nyear)
  
  ## loop over spin-up years
  for(t in seq_along(new_year)){
    
    source.file <- file.path(in.path,paste0(in.prefix,spin_year[t],".nc"))
    target.file <- file.path(in.path,paste0(in.prefix,new_year[t],".nc"))
    
    ## check if a met file already exists
    if(!file.exists(target.file)){
      ## if not, create a symbolic link to the year to be sampled
      file.symlink(from = source.file,target.file)
    }
    
  }
  
  ## return new start_date
  return(spin_start_date)

}
