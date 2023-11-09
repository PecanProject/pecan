runModule.PEcAn2EFI <- function(settings) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    return(PEcAn.settings::papply(settings, runModule.PEcAn2EFI))
  } else if (PEcAn.settings::is.Settings(settings)) {
    return(PEcAn2EFI(settings))
  } else {
    stop("runModule.get.results only works with Settings or MultiSettings")
  }
}

PEcAn2EFI <- function(outdir = NULL, ne = NULL, site_id = NULL, start_date=NULL,end_date = NULL,s=settings){
  #s = settings[[1]] ## testing
  if(is.null(outdir))     outdir <- s$modeloutdir
  if(is.null(ne))         ne = s$ensemble$size
  if(is.null(site_id))    site_id = s$run$site$id
  if(is.null(start_date)) start_date = s$run$start.date
  if(is.null(end_date))   end_date = start_date + lubridate::days(35)
  start.year = lubridate::year(start_date)
  end.year   = lubridate::year(end_date)
  
  # ## load ensemble objects, used code from get.results
  # if (!is.null(ens.ensemble.id)) {
  #   fname <- ensemble.filename(s, "ensemble.samples", "Rdata", 
  #                              ensemble.id = ens.ensemble.id, 
  #                              all.var.yr = TRUE)
  # } else if (!is.null(s$ensemble$ensemble.id)) {
  #   ens.ensemble.id <- s$ensemble$ensemble.id
  #   fname <- ensemble.filename(s, "ensemble.samples", "Rdata",
  #                              ensemble.id = ens.ensemble.id, 
  #                              all.var.yr = TRUE)
  # } else {
  #   fname <- file.path(outdir, "samples.Rdata")
  # }     
  # if (!file.exists(fname)) {
  #   PEcAn.logger::logger.severe("No ensemble samples file found!")
  # }
  # load(fname)
  # if (!exists("ens.run.ids")) {
  #   ens.run.ids <- runs.samples$ens
  # }
  ens.run.ids = paste("ENS",
                      formatC(1:ne, width = 5, format = "d", flag = "0"),
                      site_id,
                      sep="-"
  )
  
  ensemble.output <- list()
  for (row in seq_along(ens.run.ids)) { #rownames(ens.run.ids)
    ## read output
    #run.id <- ens.run.ids[row, "id"]
    run.id <- ens.run.ids[row]
    PEcAn.logger::logger.info("reading ensemble output from run id: ", format(run.id, scientific = FALSE))
    ensemble.output[[row]] <- PEcAn.utils::read.output(run.id, file.path(outdir, run.id), start.year, end.year, variables=NULL)
    
    ## reprocess time_bounds
    doy = as.vector(ensemble.output[[row]]$time_bounds[1,])  ## day of year
    years = c(which(doy < 0.00001),length(doy)+1) ## get breaks between years
    if(years[1] != 1) years = c(1,years) ## add current year if not start of year
    years = rep(lubridate::year(start_date):lubridate::year(end_date),times=diff(years)) ## convert to years
    tod = lubridate::as_datetime(lubridate::seconds_to_period(floor((doy - floor(doy)) * 60 *60))) ## time of day
    lubridate::year(tod) <- years   
    lubridate::yday(tod) <- floor(doy)    
    ensemble.output[[row]]$time_bounds = tod
    
    ## convert to data frame
    ensemble.output[[row]]  = as.data.frame(ensemble.output[[row]]) %>% 
      dplyr::mutate(
        parameter = row,
        reference_datetime = lubridate::as_datetime(start_date),
        site_id = site_id
      )
  }
  names(ensemble.output) = ens.run.ids
  
  return(dplyr::bind_rows(ensemble.output))
}

PEcAn2EFI.ens <- function(outdir,run.id,start_date,end_date = NULL){
  if(is.null(end_date))   end_date = start_date + lubridate::days(35)
  start.year = lubridate::year(start_date)
  end.year   = lubridate::year(end_date)
  ens.id     = as.numeric(strsplit(run.id,"-")[[1]][2])
  site_id    = as.numeric(strsplit(run.id,"-")[[1]][3])
    
  PEcAn.logger::logger.info("reading ensemble output from run id: ", format(run.id, scientific = FALSE))
  ensemble.output <- PEcAn.utils::read.output(run.id, file.path(outdir, run.id), start.year, end.year, variables=NULL)
  
  if(!is.numeric(nrow(ensemble.output))) return(NULL)
  
  ## reprocess time_bounds
  doy = as.vector(ensemble.output$time_bounds[1,])  ## day of year
  years = c(which(doy < 0.00001),length(doy)+1) ## get breaks between years
  if(years[1] != 1) years = c(1,years) ## add current year if not start of year
  years = rep(lubridate::year(start_date):lubridate::year(end_date),times=diff(years)) ## convert to years
  tod = lubridate::as_datetime(lubridate::seconds_to_period(floor((doy - floor(doy)) * 60 *60*24))) ## time of day
  lubridate::year(tod) <- years   
  lubridate::yday(tod) <- floor(doy)    
  ensemble.output$time_bounds = tod
  
  ## convert to data frame
  ensemble.output  = as.data.frame(ensemble.output) %>% 
    dplyr::mutate(
      parameter = ens.id,
      reference_datetime = lubridate::as_datetime(start_date),
      site_id = site_id
    )

  return(ensemble.output)
}
