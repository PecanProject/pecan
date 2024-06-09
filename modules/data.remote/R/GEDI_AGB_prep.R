#' Prepare L4A GEDI above ground biomass (AGB) data for the state data assimilation (SDA) workflow.
#' This function is built upon the modified `l4_download` function within the `GEDI4R` package in need for a better parallel computation.
#' @details During the first use, users will be ask to enter their Earth Explore
#'  login Information for downloading the data. If you don't have already an
#'  account, register at https://urs.earthdata.nasa.gov/users/new.
#'  These information will be saved in outdir as a netrc
#'  file. This function uses the foreach package for
#'  downloading files in parallel, with the
#'  doParallel configuration. If a file with the same
#'  name is already presented in outdir it will be overwrite.
#'
#' @param site_info A list including site_id, longitude, and latitude.
#' @param time_points A vector of date contains target dates (in YYYY-MM-DD).
#' @param outdir Directory where the final CSV file will be stored.
#' @param buffer buffer distance (in degrees) for locate GEDI AGB searching box (default is 0.01 [~ 1 km]).
#' @param search_window search window (any length of time. e.g., 3 month) for locate available GEDI AGB values.
#'
#' @return A data frame containing AGB and sd for each site and each time step.
#' 
#' @examples
#' \dontrun{
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' site_info <- settings %>% 
#'   purrr::map(~.x[['run']] ) %>% 
#'   purrr::map('site')%>% 
#'   purrr::map(function(site.list){
#'     #conversion from string to number
#'     site.list$lat <- as.numeric(site.list$lat)
#'     site.list$lon <- as.numeric(site.list$lon)
#'     list(site_id=site.list$id, lat=site.list$lat, lon=site.list$lon, site_name=site.list$name)
#'   })%>% 
#'   dplyr::bind_rows() %>% 
#'   as.list()
#' time_points <- seq(start.date, end.date, by = time.step)
#' buffer <- 0.01
#' outdir <- getwd()
#' GEDI_AGB <- GEDI_AGB_prep(site_info, time_points, outdir, buffer)
#' }
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
GEDI_AGB_prep <- function(site_info, time_points, outdir = file.path(getwd(), "GEDI_AGB"), buffer = 0.01, search_window = "3 month") {
  # if we don't have outdir, we will use the temp dir as outdir.
  if (!dir.exists(outdir)) {
    dir.create(outdir)
  }
  # Create credential file into outdir.
  getnetrc(outdir)
  # check dates.
  time_points <- time_points[which(time_points >= as.Date("2019-04-18"))]
  # if we don't have any observation for those dates.
  if (length(time_points) == 0) {
    return(NULL)
  }
  # if we have dates with observations.
  # summarize data lists into data frame.
  AGB_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
    `colnames<-`(c("site_id", paste0(time_points, "_AGB"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, AGB, std, target time point.
  AGB_Output$site_id <- site_info$site_id
  # loop over each time point
  for (i in seq_along(time_points)) {
    # create start and end dates.
    start_date <- seq(time_points[i], length.out = 2, by = paste0("-", search_window))[2]
    end_date <- seq(time_points[i], length.out = 2, by = search_window)[2]
    # extract GEDI AGB.
    AGB <- GEDI_AGB_extract(site_info, start_date, end_date, outdir, nfile.min = 0, nrow.min = 1, buffer = as.numeric(buffer))
    for (j in seq_along(AGB)) {
      # skip NA observations.
      if (all(is.na(AGB[[j]]))) {
        next
      }
      # otherwise calculate the mean and standard error.
      AGB_Output[j, paste0(time_points[i], "_AGB")] <- mean(AGB[[j]]$agbd, na.rm = T) # mean
      # Note: the following sd calculation is only for testing.
      # TODO: making sure to upgrade this sd calculation.
      if (nrow(AGB[[i]]) == 1) {
        AGB_Output[j, paste0(time_points[i], "_SD")] <- AGB[[j]]$agbd_se # sd
      } else {
        AGB_Output[j, paste0(time_points[i], "_SD")] <- stats::sd(AGB[[j]]$agbd, na.rm = T) # sd
      }
    }
  }
  return(list(AGB_Output = AGB_Output, time_points = time_points, var = "AGB"))
}
#' Plot GEDI AGB observations around the target site.
#'
#' @param outdir Where the plot PNG file will be stored.
#' @param site.id Unique ID for the target site.
#' @param start_date start date (date with YYYY-MM-DD format) for filtering out the existing CSV file.
#' @param end_date end date (date with YYYY-MM-DD format) for filtering out the existing CSV file.
#'
#' @return 
#' @export 
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#' @importFrom rlang .data
GEDI_AGB_plot <- function(outdir, site.id, start_date, end_date) {
  # redirect to the site folder.
  site_folder <- file.path(outdir, site.id)
  # 
  if (file.exists(file.path(site_folder, "Error.txt"))) {
    PEcAn.logger::logger.info("The current point is outside of GEDI domain!")
    return(FALSE)
  } else {
    if (!file.exists(file.path(site_folder, "extent.txt")) | !file.exists(file.path(site_folder, "GEDI_AGB.csv"))) {
      PEcAn.logger::logger.info("Please rerun the GEDI_AGB_prep function for this site!")
      return(FALSE)
    } else {
      extent <- utils::read.table(file.path(site_folder, "extent.txt"), skip = 1) %>%
        as.numeric %>%
        purrr::set_names(c("ymax", "ymin", "xmin", "xmax"))
      point.lat.lon <- matrix(c(mean(extent[c("ymin", "ymax")]), mean(extent[c("xmin", "xmax")])), nrow = 1) %>% 
        `colnames<-`(c("lat", "lon")) %>%
        as.data.frame
      extent.x.y <- data.frame(matrix(c(extent["xmin"], extent["ymin"],
                                         extent["xmax"], extent["ymin"],
                                         extent["xmax"], extent["ymax"],
                                         extent["xmin"], extent["ymax"]), nrow = 4, byrow = T)) %>% `colnames<-`(c("lon", "lat"))
      res <- utils::read.csv(file.path(site_folder, "GEDI_AGB.csv"))
      ggplot2::ggplot() +
        ggplot2::geom_polygon(data = extent.x.y, ggplot2::aes(x = .data$lon, y = .data$lat), color="blue", fill = "white") +
        ggplot2::geom_point(data = res, ggplot2::aes(x = .data$lon_lowestmode, y = .data$lat_lowestmode, color = .data$agbd)) +
        ggplot2::geom_point(shape = 24, data = point.lat.lon, ggplot2::aes(x = .data$lon, y = .data$lat), size = 3) +
        ggplot2::geom_text(data = point.lat.lon, ggplot2::aes(x = .data$lon, y = .data$lat, label=site.id, hjust=-0.1, vjust=0)) +
        ggplot2::scale_color_distiller(palette = 'Greens', direction = 1) +
        ggplot2::labs(color = "AGB")
    }
  }
}
#' Extract L4A GEDI above ground biomass data for the GEDI AGB prep function.
#'
#' @param site_info A list including site_id, longitude, and latitude.
#' @param start_date target start date (date with YYYY-MM-DD format) for preparing GEDI AGB from remote or local database.
#' @param end_date end date (date with YYYY-MM-DD format) for preparing GEDI AGB from remote or local database.
#' @param outdir Directory where the final CSV file will be stored.
#' @param nfile.min the minimum required file number to be downloaded and extracted, default is 0.
#' @param nrow.min the minimum required observation number to be extracted, default is 0.
#' @param buffer buffer distance (in degrees) for locate GEDI AGB searching box (default is 0.01 [~ 1 km]).
#' @param gradient the gradient for iteratively enlarge the extent if the nfile.min or nrow.min are not reached, default is 0. If nfile.min or nrow.min is 0 this will be skipped.
#'
#' @return A list of AGB data frames for each site.
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
#' @importFrom rlang .data
GEDI_AGB_extract <- function(site_info, start_date, end_date, outdir, nfile.min = 0, nrow.min = 0, buffer = 0.01, gradient = 0) {
  #Initialize the multicore computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }
  # grab site.info and convert from lat/lon to sf objects of points and buffer areas.
  GEDI_AGB <- split(as.data.frame(site_info), seq(nrow(as.data.frame(site_info)))) %>%
    furrr::future_map(function(point){
      # flag determine if we have satisfied res.filter object.
      csv.valid <- FALSE
      # extent for filter.
      extent <- data.frame(ymax = point$lat + buffer,
                            ymin = point$lat - buffer,
                            xmin = point$lon - buffer,
                            xmax = point$lon + buffer)
      # redirect to the current folder.
      # if we already create the folder.
      site_folder <- file.path(outdir, point$site_id)
      if (file.exists(site_folder)) {
        # if csv file has been generated.
        csv.path <- list.files(site_folder, "GEDI_AGB.csv", full.names = T)
        if (length(csv.path) > 0) {
          # read csv file.
          res <- utils::read.csv(csv.path)
          if (file.exists(file.path(site_folder, "extent.txt")) & nfile.min != 0) {
            extent <- utils::read.table(file.path(site_folder, "extent.txt"), skip = 1, col.names = c("ymax", "ymin", "xmin", "xmax"))
            extent <- extent[nrow(extent),]
          }
          # filter previous records based on space and time.
          res.filter <- res %>% dplyr::filter(.data$lat_lowestmode <= extent["ymax"],
                                              .data$lat_lowestmode >= extent["ymin"],
                                              .data$lon_lowestmode >= extent["xmin"], 
                                              .data$lon_lowestmode <= extent["xmax"],
                                              lubridate::as_date(.data$date) >= lubridate::as_date(start_date),
                                              lubridate::as_date(.data$date) <= lubridate::as_date(end_date))
          # determine if res.filter is not empty.
          # In the future, we will need to document 
          # file name of each pre-downloaded `GEDI L4A` files 
          # such that any new files within the range will be downloaded and processed.
          if (nrow(res.filter) > 0) {
            csv.valid <- TRUE
          }
        }
      } else {
        # create folder for the current site.
        base::dir.create(site_folder)
        # copy and paste credentials to the current folder.
        base::file.copy(file.path(outdir, "netrc"), file.path(site_folder, "netrc"))
      }
      # filter out point outside the GEDI spatial domain.
      if (point$lat > 52 | point$lat < -52) {
        utils::write.table("Point is outside the GEDI domain.", file = file.path(site_folder, "Error.txt"))
        return(NA)
      }
      # if we have previous GEDI records covering space and time.
      if (csv.valid) {
        return(res.filter)
      } else {
        # download GEDI AGB for current site.
        res.current <- GEDI_AGB_download(start_date = start_date,
                                         end_date = end_date, 
                                         outdir = site_folder, 
                                         extent = extent,
                                         nfile.min = nfile.min,
                                         nrow.min = nrow.min,
                                         gradient = gradient)
        # if we have previous downloaded GEDI records.
        if (exists("res", mode = "environment") & !all(is.na(res.current))) {
          res <- rbind(res, res.current)
        } else if (!all(is.na(res.current))) {
          res <- res.current
        }
        # write into new csv file.
        if (exists("res") & !all(is.na(res.current))) {
          utils::write.csv(res, file = file.path(site_folder, "GEDI_AGB.csv"), row.names = F)
          # save plot.
          grDevices::png(file.path(site_folder, "plot.png"))
          print(GEDI_AGB_plot(outdir = outdir, site.id = point$site_id, start_date = start_date, end_date = end_date))
          grDevices::dev.off()
          return(res.current)
        } else {
          return(NA)
        }
      }
    }, .progress = T) %>% purrr::set_names(site_info$site_id)
  GEDI_AGB
}
#' Download GEDI AGB data for the GEDI AGB extract function.
#'
#' @param start_date start date (date with YYYY-MM-DD format) for downloading GEDI AGB from remote database.
#' @param end_date end date (date with YYYY-MM-DD format) for downloading GEDI AGB from remote database.
#' @param outdir Directory where the final CSV file will be stored.
#' @param extent the XY box (in degrees) for downloading GEDI AGB file.
#' @param nfile.min the minimum required file number to be downloaded and extracted, default is 0.
#' @param nrow.min the minimum required observation number to be extracted, default is 0.
#' @param gradient the gradient for iteratively enlarge the extent if the nfile.min or nrow.min are not reached, default is 0. If nfile.min or nrow.min is 0 this will be skipped.
#'
#' @return A data frame containing AGB and sd for the target spatial and temporal extent.
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
GEDI_AGB_download <- function(start_date, end_date, outdir, extent, nfile.min = 0, nrow.min = 0, gradient = 0) {
  # download GEDI AGB files.
  # if there is no data within current buffer distance.
  files <- try(l4_download(ncore = 1,
                           ul_lat = extent["ymax"], 
                           lr_lat = extent["ymin"], 
                           ul_lon = extent["xmin"], 
                           lr_lon = extent["xmax"], 
                           from = start_date, 
                           to = end_date,
                           outdir = outdir,
                           just_path = T), silent = T)
  # if we just need the data within fixed extent and hit error.
  if ("try-error" %in% class(files) & nfile.min == 0) {
    return(NA)
  }
  while ("try-error" %in% class(files) | length(files) < nfile.min) {
    # we iteratively add 0.1 degree to the buffer distance.
    extent[c(1, 4)] <- extent[c(1, 4)] + gradient
    extent[c(2, 3)] <- extent[c(2, 3)] - gradient
    files <- try(l4_download(ncore = 1,
                             ul_lat = extent["ymax"], 
                             lr_lat = extent["ymin"], 
                             ul_lon = extent["xmin"], 
                             lr_lon = extent["xmax"], 
                             from = start_date, 
                             to = end_date,
                             outdir = outdir,
                             just_path = T), silent = T)
  }
  try(files <- l4_download(ncore = 1,
                           ul_lat = extent["ymax"], 
                           lr_lat = extent["ymin"], 
                           ul_lon = extent["xmin"], 
                           lr_lon = extent["xmax"], 
                           from = start_date, 
                           to = end_date,
                           outdir = outdir), silent = T)
  # load files.
  res <- GEDI4R::l4_getmulti(files, ncore = 1)
  # filter observations based on filter buffer distance.
  keep.ind <- which(res$lat_lowestmode <= extent["ymax"] & 
                      res$lat_lowestmode >= extent["ymin"] & 
                      res$lon_lowestmode >= extent["xmin"] & 
                      res$lon_lowestmode <= extent["xmax"])
  while (length(keep.ind) < nrow.min & length(files) > 0) {
    # we iteratively add 0.1 degree to the buffer distance.
    # because sometimes even the the extent ensure at least 1 tile nearby the location.
    # they may not be accessible by the extent when we try to extract the file.
    # possible due to accuracy issue.
    extent[c(1, 4)] <- extent[c(1, 4)] + gradient
    extent[c(2, 3)] <- extent[c(2, 3)] - gradient
    # filter observations based on filter buffer distance.
    keep.ind <- which(res$lat_lowestmode <= extent["ymax"] &
                        res$lat_lowestmode >= extent["ymin"] &
                        res$lon_lowestmode >= extent["xmin"] &
                        res$lon_lowestmode <= extent["xmax"])
  }
  # record extent for download and extraction.
  extent <- data.frame(matrix(extent, nrow = 1)) %>% purrr::set_names(c("ymax", "ymin", "xmin", "xmax"))
  utils::write.table(extent, file = file.path(outdir, "extent.txt"), row.names = F)
  # if (file.exists(file.path(outdir, "extent.txt"))) {
  #   pre.extent <- utils::read.table(file.path(outdir, "extent.txt"), skip = 1)
  #   extent <- rbind(pre.extent, extent) %>% `colnames<-`(c("ymax", "ymin", "xmin", "xmax"))
  #   extent <- extent[!base::duplicated(extent),]
  #   utils::write.table(extent, file = file.path(outdir, "extent.txt"), row.names = F)
  # } else {
  #   extent <- data.frame(matrix(extent, nrow = 1)) %>% purrr::set_names(c("ymax", "ymin", "xmin", "xmax"))
  #   utils::write.table(extent, file = file.path(outdir, "extent.txt"), row.names = F)
  # }
  # delete downloaded H5 files.
  unlink(list.files(outdir, "*.h5", full.names = T), recursive = T)
  if (length(keep.ind) == 0) {
    return(NA)
  } else {
    return(res[keep.ind,])
  }
}
#' DOWNLOAD GEDI level 4A data from DAACL.ORNL
#'
#' Download all GEDI footprints from
#' \href{https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=2056}{the official repository} that intersect a study area, defined as an extent in lon/lat
#' coordinates. The footprints are located within the global latitude band
#' observed by the International Space Station (ISS), nominally 51.6 degrees N
#' and S and reported for the period 2019-04-18 to 2020-09-02
#'
#' @param ul_lat Numeric: upper left latitude.
#' @param lr_lat Numeric: lower right latitude.
#' @param ul_lon Numeric: upper left longitude.
#' @param lr_lon Numeric: lower right longitude.
#' @param ncore Numeric: numbers of core to be used if the maximum core
#'   available is less than the number of files to be download. Default to the
#'   number of cores available minus one.
#' @param from Character: date from which the data search starts. In the form
#'   "yyyy-mm-dd".
#' @param to Character: date on which the data search end. In the form
#'   "yyyy-mm-dd".
#' @param outdir Character: path of the directory in which to save the
#'   downloaded files.Default to the working directory. If it doesn't exist it
#'   will be created. Ignored if just_path=TRUE.
#' @param just_path Logical: if TRUE return a character vector of available
#'   files without downloading them. Default to FALSE.
#' @param subset Numeric vector of indices for downloading a subset of files
#'   instead of all. If is not numeric it will be ignored silently.
#' @details During the first use, users will be ask to enter their Earth Explore
#'  login Information for downloading the data. If you don't have already an
#'  account, register at https://urs.earthdata.nasa.gov/users/new.
#'  These information will be saved in outdir as a netrc
#'  file. This function uses the foreach package for
#'  downloading files in parallel, with the
#'  doParallel configuration. If a file with the same
#'  name is already presented in outdir it will be overwrite.
#' @return List of file path in outdir.
#' @examples
#' \dontrun{
#' #retrive Italy bound
#' bound <- sf::st_as_sf(raster::getData('GADM', country='ITA', level=1))
#' ex <- raster::extent(bound)
#' ul_lat <- ex[4]
#' lr_lat <- ex[3]
#' ul_lon <- ex[2]
#' lr_lon <- ex[1]
#' from <- "2020-07-01"
#' to <- "2020-07-02"
#' #get just files path available for the searched parameters
#' l4_download(ul_lat=ul_lat,
#'             lr_lat=lr_lat,
#'             ul_lon=ul_lon,
#'             lr_lon=lr_lon,
#'             from=from,
#'             to=to,
#'             just_path=T
#' )
#' 
#' #download the first 4 files
#' 
#' l4_download(ul_lat=ul_lat,
#'             lr_lat=lr_lat,
#'             ul_lon=ul_lon,
#'             lr_lon=lr_lon,
#'             from=from,
#'             to=to,
#'             just_path=F,
#'             outdir = tempdir(),
#'             subset=1:4)
#' }
#' @author Elia Vangi
l4_download <-
  function(ul_lat,
           lr_lat,
           ul_lon,
           lr_lon,
           ncore = parallel::detectCores() - 1,
           from = NULL,
           to = NULL,
           outdir=getwd(),
           just_path = F,
           subset = NULL) {
    
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    #check if outdir exist and if there is a netrc file in
    if (!just_path) {
      # stopifnot("outdir is not character" = check_char(outdir))
      if (!dir.exists(outdir)) {
        dir.create(outdir)
        message(outdir, " does not exist. It will be created")
        # netrc_file <- getnetrc(outdir)
      } else if (length(list.files(outdir, pattern = "netrc")) == 0) {
        # netrc_file <- getnetrc(outdir)
      } else{
        netrc_file <- list.files(outdir, pattern = "netrc", full.names = T)
      }
    }
    #time period
    daterange <- c(from, to)
    
    # Get path to GEDI2A data
    gLevel4 <-
      GEDI4R::gedifinder(
        ul_lat,
        ul_lon,
        lr_lat,
        lr_lon,
        daterange = daterange
      )
    
    lg <- length(gLevel4)
    
    if(lg==0){stop("there are no GEDI files for this date or coordinates")}
    
    if (just_path) {
      return(gLevel4)
      stop(invisible())
    }
    
    
    #check for existing GEDI file in outdir
    pre <- list.files(outdir,pattern = "h5")
    if(length(pre)!=0) {
      gLevel4 <-
        gLevel4[!basename(tools::file_path_sans_ext(gLevel4)) %in% basename(tools::file_path_sans_ext(pre))]
      nlg <- length(gLevel4)
      message(lg, " files found, of wich ",lg-nlg, " already downloaded in ", outdir)
      
    }else{ message(lg, " files found.")}
    
    
    #subset GEDI files
    if (!is.null(subset) && is.numeric(subset)) {
      if(length(subset)>length(gLevel4)){
        warning("the length of subset vector is greater than the number of files to be download. Subsetting will not be done.")
      }else{ gLevel4 <- gLevel4[subset]}
    }
    
    #set ncore equal to the number of files found or to the user defined value
    if (ncore > 1) {
      ncore <- ifelse(length(gLevel4) <= parallel::detectCores()-1, length(gLevel4), ncore)
      message("using ", ncore, " cores")
      #download
      cl <- parallel::makeCluster(ncore)
      doParallel::registerDoParallel(cl)
      message("start download")
      
      foreach::foreach(
        i = 1:length(gLevel4),
        .packages = "httr"
      ) %dopar% {
        response <-
          httr::GET(
            gLevel4[i],
            httr::write_disk(file.path(outdir, basename(gLevel4)[i]), overwrite = T),
            httr::config(netrc = TRUE, netrc_file = netrc_file),
            httr::set_cookies("LC" = "cookies")
          )
      }
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    } else {
      message("using ", ncore, " core")
      for (i in seq_along(gLevel4)) {
        response <-
          httr::GET(
            gLevel4[i],
            httr::write_disk(file.path(outdir, basename(gLevel4)[i]), overwrite = T),
            httr::config(netrc = TRUE, netrc_file = netrc_file),
            httr::set_cookies("LC" = "cookies")
          )
      }
    }
    
    message("Done")
    files <- list.files(outdir, pattern = "h5", full.names = T)
    return(files)
  }
#' Function for building netrc file with access credentials
#'
#' @param dl_dir Directory where the netrc file will be stored.
#' @return file path of the netrc file.
getnetrc <- function (dl_dir) {
  netrc <- file.path(dl_dir, "netrc")
  if (file.exists(netrc) == FALSE ||
      any(grepl("urs.earthdata.nasa.gov",
                readLines(netrc))) == FALSE) {
    netrc_conn <- file(netrc)
    writeLines(c(
      "machine urs.earthdata.nasa.gov",
      sprintf(
        "login %s",
        getPass::getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")
      ),
      sprintf(
        "password %s",
        getPass::getPass(msg = "Enter NASA Earthdata Login Password:")
      )
    ),
    netrc_conn)
    close(netrc_conn)
    message(
      "A netrc file with your Earthdata Login credentials was stored in the output directory "
    )
  }
  return(netrc)
}