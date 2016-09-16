.download.raw.met.module <- function(dir, met, register, machine, start_date, end_date, con, new.site=NULL, host, overwrite=FALSE) {
  outfolder  <- file.path(dir, met)
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  if(register$scale=="regional") { 
    # Right now this only means NARR but will need to be generalized once we have more 
    # regional met products
     raw.data.site.id <- ifelse(met %in% c("CRUNCEP", "GFDL"), new.site$id, register$siteid)

    raw.id <- convert.input(
      input.id=NA, outfolder=outfolder, formatname=register$format$name, mimetype=register$format$mimetype, 
      site.id=raw.data.site.id, start_date=start_date, end_date=end_date, 
      pkg=pkg, fcn=fcn, con=con, host=host, browndog=NULL, write=TRUE, overwrite=overwrite, 
      site_id=new.site$id, lat.in=new.site$lat, lon.in=new.site$lon,
      model=input_met$model, scenario=input_met$scenario, ensemble_member=input_met$ensemble_member)

  } else if(register$scale=="site") { # Site-level met
    outfolder = paste0(outfolder,"_site_",str_ns)

    raw.id <- convert.input(
      input.id=NA, outfolder=outfolder, formatname=register$format$name, mimetype=register$format$mimetype, 
      site.id=raw.data.site.id, start_date=start_date, end_date=end_date, 
      pkg=pkg, fcn=fcn, con=con, host=host, browndog=NULL, write=TRUE, overwrite=overwrite, 
      sitename=site$name, username=username)
  }
  return(raw.id)
}
