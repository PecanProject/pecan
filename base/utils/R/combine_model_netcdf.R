#' Combine many netCDFs into one file per year
#'
#' Merges model outputted netCDF files by the time steps specified in a pecan settings file.
#'
#' The function is only tested for SIPNET model runs that were run with state data assimilation enabled.
#' Please make sure you have the same netCDF formats if you want to proceed with different models.
#' We could also have more functions that deal with different dimensions (e.g., by site instead of by year).
#' 
#' @param model.outdir character: path to the folder that contains model outputs.
#' @param nc.outdir  character: physical path to the folder that contains the merged netCDF files.
#' @param ens.num numeric: number of ensembles for the model run.
#' @param site.ids numeric or character: vector of site ids across locations.
#' @param start.date date or character in YYYY-MM-DD format: start date of the model run.
#' @param end.date date or character in YYYY-MM-DD format: end date of the model run.
#' @param time.step character: time step of the model run. Default is 1 year.
#' @param cores numeric: the number of CPUs for the parallel computation. Default is 1.
#'
#' @return character: file paths to the merged netCDF files.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
nc_merge_all_sites_by_year <- function (model.outdir, 
                                        nc.outdir, 
                                        ens.num, 
                                        site.ids, 
                                        start.date, 
                                        end.date, 
                                        time.step = "1 year", 
                                        cores = 1) {
  # check shell environments.
  if (suppressWarnings(system2("which", "cdo", stdout = FALSE)) != 0) {
    PEcAn.logger::logger.info("The cdo function is not detected in shell command.")
    return(NA)
  }
  # create the nc output folder if it doesn't exist.
  if (!file.exists(nc.outdir)) {
    dir.create(nc.outdir)
  }
  # calculate time points.
  time.points <- lubridate::year(seq(lubridate::date(start.date), 
                                     lubridate::date(end.date), 
                                     time.step))
  
  # loop over time.
  # initialize parallel.
  cl <- parallel::makeCluster(as.numeric(cores))
  doSNOW::registerDoSNOW(cl)
  #progress bar
  pb <- utils::txtProgressBar(min = 1, max = length(site.ids), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  # record nc paths.
  nc.paths <- c()
  for (t in seq_along(time.points)) {
    time <- time.points[t] # grab the current time point.
    # record previous file.
    if (file.exists(file.path(nc.outdir, paste0(time, ".nc")))) {
      nc.paths <- c(nc.paths, file.path(nc.outdir, paste0(time, ".nc")))
      next
    }
    # loop over sites.
    s <- NULL # For passing the GitHub actions.
    nc.files <- 
      foreach::foreach(s = seq_along(site.ids), 
                       .packages = c("purrr", "ncdf4"), 
                       .options.snow=opts) %dopar% {
                         nc_merge_single_site(model.outdir = model.outdir, 
                                              nc.outdir = nc.outdir, 
                                              ens.num = ens.num, 
                                              # cdo collgrid only works for numeric data type.
                                              site.id = site.ids[s], 
                                              time)
                       } %>% unlist
    # merge across sites using CDO command.
    cmd <- "cdo -P @CORES@ collgrid @NC.OUTDIR@/*@TIME@.nc @OUTFILE@"
    cmd <- gsub("@CORES@", cores, cmd)
    cmd <- gsub("@NC.OUTDIR@", nc.outdir, cmd)
    cmd <- gsub("@TIME@", time, cmd)
    cmd <- gsub("@OUTFILE@", file.path(nc.outdir, paste0(time, ".nc")), cmd)
    out <- system(cmd, intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    # if we have site ids in character format.
    if (all(is.character(site.ids))) {
      nc <- ncdf4::nc_open(file.path(nc.outdir, paste0(time, ".nc")))
      site_dim <- ncdf4::ncdim_def("site", units = "", vals = seq_along(site.ids))
      site_id_var <- ncdf4::ncvar_def("site_id", units = "", dim = site_dim, prec = "char")
      ncdf4::ncvar_put(nc, varid = "site_id", vals = site.ids)
      ncdf4::nc_close(nc) # close nc connection.
    }
    # record the current nc path.
    nc.paths <- c(nc.paths, file.path(nc.outdir, paste0(time, ".nc")))
    # remove nc files for each site.
    unlink(nc.files)
  }
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # return nc paths.
  return(nc.paths)
}

#' Merge model outputted netCDF files across ensembles for a single site.
#' @details
#' The function is only tested for SIPNET model runs.
#' Please make sure you have the same netCDF formats if you want to proceed with different models.
#'
#' This function requires `site.id` to be an integer. 
#' If your sites have non-numeric IDs, one possible workaround is to 
#' pass a dummy value and then edit the output file afterward to replace its `site_id` variable 
#' with character data. If you do this, do be aware many legacy netCDF tools have poor support 
#' for netCDFs containing character data.
#' 
#' @param model.outdir character: physical path to the model output folder.
#' @param nc.outdir  character: physical path to the folder that contains the merged netCDF files.
#' @param ens.num numeric: ensemble size.
#' @param site.id numeric: identification number of the site.
#' @param time numeric or character: the current time of netCDF files to be extracted.
#'   See details for use with non-numeric siteIDs
#' @return character: file path to the merged netCDF file.
#' 
#' @author Dongchen Zhang
nc_merge_single_site <- function (model.outdir, nc.outdir, ens.num, site.id, time) {
  # grab basic formats from the first nc file of the site.
  # create the folder name associated with first ensemble and first site.
  prefix <- "ENS-"
  folder.name <- paste0(prefix, sprintf("%05d", 1), "-", site.id)
  # read nc file.
  nc <- ncdf4::nc_open(file.path(model.outdir, folder.name, paste0(time, ".nc")))
  nc.vars <- nc$var # grab variable definitions.
  time.values <- nc$dim$time # grab time dimensions.
  lat <- nc$dim$lat$vals
  lon <- nc$dim$lon$vals
  ncdf4::nc_close(nc) # close nc connection.
  # dimension and variable definitions.
  # site dimension.
  site_dim <- ncdf4::ncdim_def("site", units = "", vals = site.id)
  # time dimension.
  time_dim <- ncdf4::ncdim_def("time", longname = "time", units = time.values$units, vals = time.values$vals)
  # ensemble dimension.
  ens_dim <- ncdf4::ncdim_def("ensemble", longname = "ensemble member", unit = "", vals = 1:ens.num)
  # define site-specific variables.
  lat_var <- ncdf4::ncvar_def("latitude", units = "degrees_north", dim = site_dim, prec = "double")
  lon_var <- ncdf4::ncvar_def("longitude", units = "degrees_east", dim = site_dim, prec = "double")
  site_id_var <- ncdf4::ncvar_def("site_id", units = "", dim = site_dim, prec = "integer")
  # loop over variables.
  first.creation <- TRUE
  for (i in seq_along(nc.vars)) {
    # grab the variable name.
    var <- nc.vars[[i]]$name
    # skip if it's time related variable.
    if (grepl("time", var, fixed = T)) next
    # loop over ensembles.
    var.mat <- matrix(NA, time.values$len, ens.num)
    for (ens in 1:ens.num) {
      # TODO: add checks to make sure every thing in files are in the same shape and format.
      folder.name <- paste0(prefix, sprintf("%05d", ens), "-", site.id)
      nc <- ncdf4::nc_open(file.path(model.outdir, folder.name, paste0(time, ".nc")))
      var.mat[,ens] <- ncdf4::ncvar_get(nc, var = var)
      ncdf4::nc_close(nc)
    }
    # define the current model variable.
    temp_var <- ncdf4::ncvar_def(nc.vars[[i]]$name, 
                                 units = nc.vars[[i]]$units, 
                                 dim = list(site_dim, ens_dim, time_dim), 
                                 prec = nc.vars[[i]]$prec)
    # if it's the first variable, we will need to create the NC file along with the site-specific variables.
    if (first.creation) {
      # turn the flag off.
      first.creation <- !first.creation
      # create nc file.
      nc_file <- ncdf4::nc_create(file.path(nc.outdir, paste0(site.id, "_", time, ".nc")), list(site_id_var, lon_var, lat_var, temp_var))
      # add the site-specific variables.
      ncdf4::ncvar_put(nc_file, varid = "site_id", vals = site.id)
      ncdf4::ncvar_put(nc_file, varid = "latitude", vals = lat)
      ncdf4::ncvar_put(nc_file, varid = "longitude", vals = lon)
      # add the current variable.
      ncdf4::ncvar_put(nc_file, varid = nc.vars[[i]]$name, vals = var.mat)
    } else {
      # add additional variable.
      nc_file <- ncdf4::ncvar_add(nc_file, temp_var)
      # update data.
      ncdf4::ncvar_put(nc_file, varid = nc.vars[[i]]$name, vals = var.mat)
    }
  }
  # close nc connection.
  ncdf4::nc_close(nc_file)
  # return all nc paths.
  return(file.path(nc.outdir, paste0(site.id, "_", time, ".nc")))
}

#' Extract netCDF file by site.id, time window, and variable name.
#' @details
#' The function is only tested for netCDF files generated by the `nc_merge_all_sites_by_year` function.
#' 
#' @param site.id numeric or character: identification of the site.
#' @param start.date date in YYYY-MM-DD format: start date of the requested time window.
#' @param end.date date in YYYY-MM-DD format: end date of the requested time window.
#' @param var.name character: variable name.
#' @param nc.path character: physical path to the target netCDF file.
#'
#' @return list: a list contains requested array, time steps, site id, variable name, and ensemble size.
#' 
#' @author Dongchen Zhang
#' @export
extract_nc_sda <- function (site.id, start.date, end.date, var.name, nc.path) {
  # open NC file.
  nc <- ncdf4::nc_open(nc.path)
  # grab the index for the requested site.id.
  site.ind <- which(nc$dim$site$vals == site.id)
  # calculate real time.
  time.val <- nc$dim$time$vals
  time.unit <- nc$dim$time$units
  origin <- strsplit(x = time.unit, split = "since ", fixed = TRUE)[[1]][2]
  real_time <- as.POSIXct(time.val*3600*24, origin = origin, tz = "UTC")
  time.steps <- length(real_time)
  # grab ensemble size.
  ensemble.size <- nc$dim$ensemble$len
  # if we have the time window.
  if (start.date >= real_time[1] & end.date <= real_time[time.steps]) {
    time.inds <- which(real_time >= start.date & real_time <= end.date)
  } else {
    PEcAn.logger::logger.info("The netCDF file doesn't include the date range you asking for.")
    return(0)
  }
  # grab outputs.
  res <- ncdf4::ncvar_get(nc, var.name, start = c(site.ind, 1, time.inds[1]), count = c(1, ensemble.size, length(time.inds)))
  # close NC connection.
  ncdf4::nc_close(nc)
  # prepare outputs.
  return(list(mat = res, time.points = real_time[time.inds], site.ids = site.id, var.name = var.name, ensemble.size = ensemble.size))
}