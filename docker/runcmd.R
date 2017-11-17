#!/usr/bin/env Rscript

require(PEcAn.logger)

run_model <- function(model, revision, binary, outdir, lat, lon, start_date, end_date, delete_raw, overwrite=FALSE) {
  # check if model already ran successfullly
  if (file.exists("DONE")) {
    if (overwrite) {
      file.remove("DONE")
    } else {
      PEcAn.logger::logger.info("Model already ran successfullly")
      return(TRUE)
    }
  }

    # check if model already ran unsuccessfully
  if (file.exists("ERROR")) {
    if (overwrite) {
      file.remove("ERROR")
    } else {
      PEcAn.logger::logger.info("Model already ran unsuccessfully")
      return(FALSE)
    }
  }

  # run model
  result_binary <- system2(binary, stdout="stdout.log", stderr="stderr.log")
  if (result_binary != 0) {
    file.create("ERROR")
    PEcAn.logger::logger.error("Program exited with exit code of", result_binary, "See logfiles for more details.")
    return(FALSE)
  }

  # some model specific checks to see if there was a failure
  # TODO this check should become part of the model package
  if (model == "ED2") {
    # TODO check the log file to see if everything was OK.
  }

  # convert model output to netcdf
  args <- list(outdir, lat, lon, start_date, end_date, delete_raw, revision, overwrite)
  func <- get(paste0("model2netcdf.", model), asNamespace(paste0("PEcAn.", model)))
  do.call(func, args)

  # success
  file.create("DONE")
  return(TRUE)
}

model      <- toupper(Sys.getenv("MODEL"))
revision   <- Sys.getenv("REVISION")
binary     <- Sys.getenv("BINARY")
outdir     <- Sys.getenv("OUTDIR")
lat        <- Sys.getenv("SITE_LAT")
lon        <- Sys.getenv("SITE_LON")
start_date <- Sys.getenv("START_DATE")
end_date   <- Sys.getenv("END_DATE")
delete_raw <- Sys.getenv("DELETE_RAW")
overwrite  <- Sys.getenv("OVERWRITE")

run_model(model, revision, binary, outdir, lat, lon, start_date, end_date, delete_raw, overwrite)
