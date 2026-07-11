#' Merge image tiles to a single image (currently support hdf and tif image format).
#' @details
#' Please make sure all image tiles are stored in the `folder.path`.
#' Please refer to the gdalwarp manual for more details
#' https://gdal.org/en/stable/programs/gdalwarp.html
#' 
#' @param in.path character: physical path to the folder that contains all the original image tiles.
#' @param out.path  character: physical path to the folder that contains converted and merged images.
#' @param band.name character: band name of the image. Default is NULL.
#' @param just.band.name logical: if we just want the band names of the image file. Default is TRUE.
#' @param keep.files logical: if we want to keep the image tiles at the end.
#' @param skip.conversion logical: if we want to ignore the image conversion.
#' Note that this is a experimental feature, which only works when images are all in the GeoTIFF format.
#' @param image.settings list: settings used during exporting merged image.
#' Such as image coordinate system (crs), dimension, extents (ext), and average function (fun).
#' @param computation list: settings used for configuring computation.
#' Such as maximum memory per CPU (GDAL_CACHEMAX), percentage of total memory (wm),
#' number of CPUs (NUM_THREADS), compress method (COMPRESS).
#'
#' @return character: file path to the merged GeoTIFF file.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom purrr %>%
merge_image_tiles <- function(in.path, 
                              out.path = NULL, 
                              band.name = NULL,
                              just.band.name = TRUE,
                              keep.files = FALSE, 
                              skip.conversion = FALSE,
                              image.settings = list(crs = "EPSG:4326",
                                                    dimension = NULL,
                                                    ext = NULL,
                                                    fun = NULL),
                              computation = list(GDAL_CACHEMAX = 1000,                                                        
                                                 wm = "80%",                                                        
                                                 NUM_THREADS = parallel::detectCores() - 1,                                                        
                                                 COMPRESS = "DEFLATE")) {
  # print out computation allocation.
  PEcAn.logger::logger.info(paste0("Using ", computation$wm, " memory."))
  PEcAn.logger::logger.info(paste0("Using ", computation$NUM_THREADS, " CPUs."))
  PEcAn.logger::logger.info(paste0("Using ", computation$COMPRESS, " compression mode."))
  # Detect if we have the gdalwarp module installed.
  # check shell environments.
  if (suppressWarnings(system2("which", "gdalwarp", stdout = FALSE)) != 0) {
    PEcAn.logger::logger.info("The gdalwarp function is not detected in shell command.")
    return(NA)
  }
  # grab file paths.
  file.paths <- list.files(in.path, full.names = T)
  # if we only want to know the exact band names from the image files.
  if (just.band.name) {
    # here we are assuming all image tiles share the same band names.
    band.names <- gdal_conversion(file.paths[1], just_band_name = just.band.name)
    return(band.names)
  }
  # Image conversion.
  if (is.null(out.path)) {
    PEcAn.logger::logger.info("Please provide the output directory to store the converted/mosaic image tiles.")
    return(0)
  }
  # if we want to ignore the image conversion.
  if (skip.conversion) {
    # if we have any file that has format other than .tif or .tiff.
    if (!all(grepl("tif", unique(tools::file_ext(file.paths)), fixed = TRUE))) {
      PEcAn.logger::logger.info("Can't ignore the image conversion. Please make sure all images are in the .tif or .tiff format and try again!")
      return(0)
    } else {
      # the input files will become converted files.
      converted.file.paths <- file.paths
      # band name should be replaced too.
      band.name <- "all_bands"
    }
  } else {
    converted.file.paths <- file.paths %>% 
      purrr::map2(seq_along(file.paths), function(f, tile.id) {
        gdal_conversion(in_path = f, 
                        outfolder = out.path, 
                        band_name = band.name, 
                        tile_id = tile.id, 
                        just_band_name = just.band.name)
      }) %>% unlist
  }
  # write job.sh script.
  # insert image settings.
  gdal.cmd <- "gdalwarp"
  # output coordinate system.
  if (!is.null(image.settings$crs)) {
    gdal.cmd <- paste(gdal.cmd, "-t_srs", image.settings$crs)
  }
  # output image dimension (=resolution).
  if (!is.null(image.settings$dimension)) {
    gdal.cmd <- paste(gdal.cmd, "-ts", paste(image.settings$dimension, collapse = " "))
  }
  # output image extents (in xmin, ymin, xmax, ymax order).
  if (!is.null(image.settings$ext)) {
    gdal.cmd <- paste(gdal.cmd, "-te", paste(image.settings$ext[c(1, 3, 2, 4)], collapse = " "))
  }
  # average function used to upscale image.
  if (!is.null(image.settings$fun)) {
    gdal.cmd <- paste(gdal.cmd, "-r", image.settings$fun)
  }
  # insert computation settings.
  if (any(!is.null(unlist(computation)))) {
    gdal.cmd <- paste(gdal.cmd, "--config")
  }
  # memory usage per CPU.
  if (!is.null(computation$GDAL_CACHEMAX)) {
    gdal.cmd <- paste(gdal.cmd, "GDAL_CACHEMAX", computation$GDAL_CACHEMAX)
  }
  # total memory usage.
  if (!is.null(computation$wm)) {
    gdal.cmd <- paste(gdal.cmd, "-wm", computation$wm)
  }
  # how many CPUs will be used.
  if (!is.null(computation$NUM_THREADS)) {
    gdal.cmd <- paste(gdal.cmd, paste0("-multi -wo NUM_THREADS=", computation$NUM_THREADS))
  }
  # image compress method.
  if (!is.null(computation$COMPRESS)) {
    gdal.cmd <- paste(gdal.cmd, paste0("-co COMPRESS=", computation$COMPRESS))
  }
  gdal.cmd <- paste(gdal.cmd, "-co BIGTIFF=YES -co TILED=TRUE @VRT@ @FINALTIFF@")
  cmd <- c("#!/bin/bash -l", 
           "module load gdal", 
           "gdalbuildvrt @VRT@ @TIF@",
           gdal.cmd)
  cmd <- gsub("@VRT@", file.path(out.path, "index.vrt"), cmd)
  # if we ignore the conversion, the file should be in the original path.
  if (skip.conversion) {
    cmd <- gsub("@TIF@", file.path(in.path, "*.tif"), cmd)
  } else {
    cmd <- gsub("@TIF@", file.path(out.path, "*.tif"), cmd)
  }
  cmd <- gsub("@FINALTIFF@", file.path(out.path, paste0(band.name, ".tif")), cmd)
  writeLines(cmd, con = file.path(out.path, "job.sh"))
  # grand permissions to the job file.
  cmd <- "chmod 744 @JOBFILE@"
  cmd <- gsub("@JOBFILE@", file.path(out.path, "job.sh"), cmd)
  out <- system(cmd, intern = TRUE)
  # enter the folder and run the job file.
  cmd <- 'cd \"@JOBPATH@\";./job.sh'
  cmd <- gsub(pattern = "@JOBPATH@", replacement = out.path, x = cmd)
  out <- system(cmd, intern = TRUE)
  # remove files.
  if (!keep.files) {
    unlink(list.files(out.path, full.names = T)[which(!grepl(paste0(band.name, ".tif"), list.files(out.path)))], recursive = T)
  }
  return(file.path(out.path, paste0(band.name, ".tif")))
}

#' @description This function provides tool for remote sensing image conversion using GDAL utility.
#' @details
#' Please note that, this function only supports conversions for one band of one image.
#' If you want to convert multiple images or bands, make sure to loop over these targets.
#' Currently tested H5, NetCDF, HDF4, and GeoTIFF formats. 
#' This function should be ready to any GDAL supported image format.
#' 
#' @title gdal_conversion
#' @param in_path character: physical path to the image file.
#' @param outfolder character: physical path to the folder where you want to export the converted image. Default is NULL.
#' @param band_name character: band name of the image. Default is NULL.
#' @param tile_id character/numeric: id for differentiate different converted image tiles.
#' @param just_band_name logical: if we just want the band names of the image file. Default is TRUE.
#' @param target_format character: target image format. Default is .tif.
#' @export
#' 
#' @author Dongchen Zhang
#' @examples
#' \dontrun{
#' in_path <- "/projectnb/dietzelab/malmborg/CARB/HLS_data/MSLSP_10SDH_2016.nc"
#' outfolder <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/MODIS_Phenology"
#' band_name <- "NumCycles"
#' # try grab all available bands from the target file.
#' band_names <- 
#'   gdal_conversion(in_path = in_path, 
#'     outfolder = outfolder, 
#'     band_name = NULL, 
#'     just_band_name = TRUE)
#' # try convert the first band of the available band names to GeoTIFF file.
#' f <- 
#'   gdal_conversion(in_path = in_path, 
#'     outfolder = outfolder, 
#'     band_name = band_names[1], 
#'     just_band_name = FALSE, 
#'     target_format = ".tif")
#' }
gdal_conversion <- function(in_path, outfolder = NULL, band_name = NULL, tile_id = NULL, just_band_name = TRUE, target_format = ".tif") {
  # grab subdataset paths.
  sds <- get_subdatasets(in_path)
  # grab band names.
  band_names <- sds %>% purrr::map(function(s){
    str <- strsplit(s, split = ":", fixed = T)[[1]]
    return(str[length(str)])
  }) %>% unlist
  # return band names.
  if (just_band_name) {
    return(band_names)
  }
  # conversion.
  # checks.
  if (!just_band_name) {
    if (is.null(band_name)) {
      PEcAn.logger::logger.info("Please provide band name if you want to do the conversion!")
      return(0)
    }
    if (is.null(outfolder)) {
      PEcAn.logger::logger.info("Please provide out directory path if you want to do the conversion!")
      return(0)
    }
  }
  # create target output file name.
  origin_file_name <- basename(in_path)
  if (!is.null(tile_id)) {
    target_file_name <- paste0(strsplit(origin_file_name, split = ".", fixed = T)[[1]][1], "_", band_name, "_", tile_id, target_format)
  } else {
    target_file_name <- paste0(strsplit(origin_file_name, split = ".", fixed = T)[[1]][1], "_", band_name, target_format)
  }
  # conversion.
  band.ind <- which(band_names == band_name)
  out <- gdal_translate(sds[band.ind], file.path(outfolder, target_file_name))
  return(file.path(outfolder, target_file_name))
}

#' @description This function provides tool for reading band names of remote sensing image.
#' 
#' @title get_subdatasets.
#' @param in_path character: physical path to the image file.
#' 
#' @author Dongchen Zhang
get_subdatasets <- function(in_path) {
  image.info <- terra::describe(in_path)
  image_rawnames <- image.info[grep(utils::glob2rx("*SUBDATASET*NAME*"), image.info)]
  sds <- sapply(X = seq(length(image_rawnames)), 
                FUN = function(X) {
                  split1 <- strsplit(image_rawnames[X], "=")
                  return(gsub("\"", "", split1[[1]][2]))
                })
  return(sds)
}

#' @description This function provides tool the gdal_translate execution.
#' 
#' @title gdal_translate
#' @param from character: subdataset name. 
#' Generated from the `get_subdatasets` function.
#' @param to character: physical path to the output file.
#' @author Dongchen Zhang
gdal_translate <- function (from, to) {
  # grab gdal installation path.
  if ("try-error" %in% class(try(gdal_path <- system("which gdal_translate", intern = TRUE)))) {
    PEcAn.logger::logger.info("Please make sure the gdal_translate module is installed correctly!")
    return(0)
  }
  # create cmd.
  cmd <- paste(paste('"',gdal_path,'"',sep=""), paste('"',from,'"',sep=""), paste('"',to,'"',sep=""))
  out <- system(cmd, intern = T)
}