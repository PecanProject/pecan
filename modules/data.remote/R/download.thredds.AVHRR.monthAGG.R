##' download.thredds
##'
##' @param outdir file location to place output
##' @param site_info list containing site_id, site_name, lat, lon, time_zone.
##'  Derived from BETY using a PEcAn .xml settings file with site information.
##'  Can use the get_site_info function to generate this list.
##' @param dates vector of start and end date for dataset as YYYYmmdd,
##'  YYYY-mm-dd, YYYYjjj, or date object.
##' @param varid character vector of shorthand variable name. i.e. LAI
##' @param dir_url catalog url of data from ncei.noaa.gov/thredds website
##' @param data_url opendap url of data from ncei.noaa.gov/thredds website
##' @param run_parallel Logical. Download and extract files in parallel?
##'
##' @return data.frame summarize the results of the function call
##'
##' @examples
##' \dontrun{
##' results <- download_thredds(
##'   site_info = site_info,
##'   years = c("2000", "2003"),
##'   months = c(6,7,8),
##'   varid = "LAI",
##'   dir_url = "https://www.ncei.noaa.gov/thredds/catalog/cdr/lai/files",
##'   data_url = "https://www.ncei.noaa.gov/thredds/dodsC/cdr/lai/files",
##'   run_parallel = FALSE,
##'   outdir = NULL)
##' }
##' @importFrom foreach %do% %dopar%
##' @noRd
##' @author Bailey Morrison
##'
download_thredds_AVHRR_monthly <- function(site_info,
                                           years,
                                           months,
                                           varid,
                                           dir_url,
                                           data_url,
                                           run_parallel = FALSE,
                                           outdir = NULL) {
  # until the issues with parallel runs are fixed.
  run_parallel <- FALSE


  # assumes there is a max of 31 possible days in a month.
  # This covers leap years!
  years_range <- sort(rep(seq(years[1], years[2]), 31))

  if (!(is.null(dir_url))) {
    output <- data.frame()

    for (i in seq_along(unique(years_range))) {
      result <- readLines(
        paste(dir_url, unique(years_range)[i], "/catalog.html", sep = "/")
      )
      files <- XML::getHTMLLinks(result)

      index_dates <- regexpr(
        pattern = paste0(
          "_[0-9]{4}0[", months[1], "-", months[length(months)], "]{1}[0-9]{2}_"
        ),
        files
      )
      files <- files[-(which(index_dates < 0))]
      index_dates <- index_dates[which(index_dates > 0)]

      dates_avail <- as.Date(substr(files, index_dates + 1, index_dates + 8),
                             "%Y%m%d")

      if (!(is.null(data_url))) {
        urls <- sort(
          paste(data_url, substr(dates_avail, 1, 4), "/", basename(files),
                sep = "")
        )

        if (run_parallel) {
          # ncores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
          # This is a failsafe for computers with low numbers of CPUS to reduce
          #  risk of blowing RAM.
          # if (ncores >= 3)
          # {
          #   # failsafe in case someone has a computer with 2-4 nodes.
          #   ncores <- ncores-2
          # }
          # # THREDDS has a 10 job limit. Will fail if you try to download more
          #  than 10 values at a time
          # if (ncores >= 10)
          # {
          #   ncores <- 9 # went 1 less becasue it still fails sometimes
          # }
          # cl <- parallel::makeCluster(ncores, outfile="")
          # doParallel::registerDoParallel(cl)
          # out <- foreach(i = urls, .combine = rbind) %dopar%
          #   extract_thredds_nc_AVHRR(site_info = site_info, url = i,
          #                            varid = varid)
          # parallel::stopCluster(cl)
        } else {
          i <- NULL # avoids R pkg checks "no visible binding" complaint below
          out <- foreach::foreach(i = urls, .combine = rbind) %do%
            extract_thredds_nc_AVHRR(site_info, url = i, varid = varid)

          # get max LAI for each site instead of all days with missing NA filler
          test <- foreach::foreach(i = unique(out$site_id),
                                   .combine = rbind) %do%
            max_lai(x = out, site = i)
          test$date <- lubridate::year(test$date)

          output <- rbind(output, test)
        }
      }
    }

    # if (!(is.null(outdir))) {
    #   # this will need to be changed in the future if users want to be able to
    #   # save data they haven't already extracted at different sites/dates.
    #   utils::write.csv(
    #     output,
    #     file = paste(outdir, "/THREDDS_", varid, "_",
    #                  years[1], "-", years[2], "_",months[1], "-",
    #                  months[length(months)], ".csv",
    #                  sep = "")
    #   )
    # }
    return(output)
  }
}




max_lai <- function(x, site) {
  site_info_max <- as.data.frame(
    x[x$site_id == site, ][1, 1:4],
    stringsAsFactors = FALSE
  )
  site_info_max$max <- as.numeric(
    max(x[x$site_id == site, ]$value, na.rm = TRUE)
  )
  return(site_info_max)
}
