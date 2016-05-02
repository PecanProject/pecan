##' Permute netCDF files
##'
##' @name permute.nc
##' @title permute.nc
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be permuted (will only use the year part of the date)
##' @param end_date the end date of the data to be permuted (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should ouput of function be extra verbose
##'
##' @author Elizabeth Cowdery, Rob Kooper
permute.nc <- function(in.path, in.prefix, out.path, start_date, end_date, overwrite=FALSE, verbose=FALSE,...){
  # get start/end year code works on whole years only
  start_year <- year(start_date)
  end_year <- year(end_date)

  dir.create(out.path, showWarnings=FALSE, recursive=TRUE)

  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)

  for(year in start_year:end_year){
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    tmp.file <- file.path(out.path, paste(in.prefix, "temp", year, "nc", sep="."))
    new.file <- file.path(out.path, paste(in.prefix, year, "nc", sep="."))

    # create array with results
    row <- year - start_year + 1
    results$file[row] <- new.file
    results$host[row] <- fqdn()
    results$startdate[row] <- paste0(year,"-01-01 00:00:00")
    results$enddate[row] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[row] <- 'application/x-netcdf'
    results$formatname[row] <- 'CF (permuted)'

    if (file.exists(new.file) && !overwrite) {
      logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }

    if (verbose)
      print(paste(c("nccopy", list("-k", "3", "-u", "-c", "time/8,y/277,x/349", old.file, tmp.file)), collapse=" "))
    system2("nccopy", list("-k", "3", "-u", "-c", "time/8,y/277,x/349", old.file, tmp.file))
    if (verbose)
      print(paste(c("ncpdq", list("--no_tmp_fl", "-h", "-O", "-a", "y,x,time", tmp.file, new.file)), collapse=" "))
    system2("ncpdq", list("--no_tmp_fl", "-h", "-O", "-a", "y,x,time", tmp.file, new.file))
    unlink(tmp.file)
  }
}
