##' Download PalEON files
##'
##' @name download.PalEON
##' @title download.PalEON
##' @export
##'
##' @param outfolder desired output location
##' @param start_date desired start date YYYY-MM-DD
##' @param end_date desired end date YYYY-MM-DD
##' @param sitename sitename
##' @param overwrite overwrite existing files? Default is FALSE
##' @param ... Other inputs
##' 
##' @author Betsy Cowdery
download.PalEON <- function(sitename, outfolder, start_date, end_date, overwrite = FALSE, ...) {
  
  if (sitename == "Harvard Forest - Lyford Plots (PalEON PHA)") {
    site <- "PHA"
  }  # 1-650 done
  else if (sitename == "Howland Forest- main tower (US-Ho1) (PalEON PHO)") {
    site <- "PHO"
  }  # 0-759
  else if (sitename == "Billy\U2019s Lake (PalEON PBL)") {
    #\U2019 = curly right single-quote, escaped to keep R from complaining about non-ASCII in code files
    # (yes, the curly quote is present in the DB sitename)
    site <- "PBL"
  }  # 1-672 done
  else if (sitename == "Deming Lake (PalEON PDL)") {
    site <- "PDL"
  }  # 1-673 done
  else if (sitename == "Minden Bog (PalEON PMB)") {
    site <- "PMB"
  }  # 1-674 done
  else if (sitename == "University of Notre Dame Environmental Research Center (PalEON UNDERC)") {
    site <- "PUN"
  }  # 1-675 done
  else {
   PEcAn.logger::logger.severe("Unknown site name")
  }
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  ylist      <- start_year:end_year
  mlist      <- 1:12
  vlist      <- c("lwdown", "precipf", "psurf", "qair", "swdown", "tair", "wind")
  
  system(paste0("mkdir -p ", outfolder))
  
  V <- length(vlist)
  Y <- length(ylist)
  M <- length(mlist)
  rows <- V * Y * M
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows), 
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = "PalEON", 
                        stringsAsFactors = FALSE)
  
  files <- dir(outfolder)
  if (sum(!(vlist %in% files)) > 0) {
   PEcAn.logger::logger.error("Don't have all variables downloaded")
  } else {
    for (v in vlist) {
      print(sprintf("Checking %s", v))
      for (y in ylist) {
        for (m in mlist) {
          file <- file.path(outfolder, v, sprintf("%s_%s_%04d_%02d.nc", site, v, y, m))
          if (!(file.exists(file))) {
           PEcAn.logger::logger.error("Missing met file")
          }
          row <- (which(vlist == v) - 1) * Y * M + (which(ylist == y) - 1) * M + m
          # print(row)
          results$file[row] <- dirname(file)
          results$host[row] <- PEcAn.remote::fqdn()
          results$startdate[row] <- paste0(y, "-01-01 00:00:00")
          results$enddate[row] <- paste0(y, "-12-31 23:59:59")
          results$mimetype[row] <- "application/x-netcdf"
          results$formatname[row] <- "ALMA"
        }
      }
      print(sprintf("Finished %s", v))
    }
  }
  
  return(invisible(results))
} # download.PalEON
