##' Given latitude and longitude coordinates, find NARR x and y indices
##'
##' @param slat,slon site location, in decimal degrees
##' @param infolder path to folder containing infile
##' @param infile pattern to match for filename inside infile.
##'   Only the first file matching this pattern AND ending with '.nc'
##'   will be used
##'
##' @export
##' @author Betsy Cowdery, Ankur Desai
closest_xy <- function(slat, slon, infolder, infile) {
  
  test.file <- dir(infolder, infile, full.names = TRUE)
  test.file <- test.file[grep("*.nc", test.file)]
  if (length(test.file) == 0) {
    return(NULL)
  }
  test.file <- test.file[1]
  
  nc <- ncdf4::nc_open(test.file)
  lat <- ncdf4::ncvar_get(nc, "latitude")
  lon <- ncdf4::ncvar_get(nc, "longitude")
  ncdf4::nc_close(nc)
  
  if (all(dim(lat) == dim(lon))) {
    if (dim(lat)==1&&dim(lon)==1) {
      ##Case of a single grid cell in file
      use_xy <- FALSE
      D <- matrix(-1, 1, 1)
      D[1,1] <- sqrt((lat - slat) ^ 2 + (lon - slon) ^ 2)
    } else {
    ## this case appears to involve hard-coded values for NARR
    ## needs to be generalized
      use_xy <- TRUE
      rows <- nrow(lat)
      cols <- ncol(lat)
      D <- matrix(-1, rows, cols)
    
      for (i in seq_len(rows)) {
        for (j in seq_len(cols)) {
          tlat <- lat[i, j]
          tlon <- lon[i, j]
          c1 <- tlat >= 20
          c2 <- tlat <= 50
          c3 <- tlon >= -125
          c4 <- tlon <= -65
          if (c1 & c2 & c3 & c4) {
            D[i, j] <- sqrt((tlat - slat) ^ 2 + (tlon - slon) ^ 2)
          }
        }
      }
    }
  } else {
    ## this case may make assumptions specific to PalEON Regional
    use_xy <- FALSE
    rows <- length(lat)
    cols <- length(lon)
    D <- matrix(-1, rows, cols)
    
    dlat <- mean(diff(lat))
    dlon <- mean(diff(lon))
    for (i in seq_len(rows)) {
      for (j in seq_len(cols)) {
        tlat <- lat[i]+dlat ## assume lat/lon is LL corner, move to center
        tlon <- lon[j]+dlon 
        c1 <- tlat >= min(lat)
        c2 <- tlat <= max(lat)
        c3 <- tlon >= min(lon)
        c4 <- tlon <= max(lon)
        if (c1 & c2 & c3 & c4) {
          D[i, j] <- sqrt((tlat - slat) ^ 2 + (tlon - slon) ^ 2)
        }
      }
    }
  }
  
  dmin <- min(D[which(D >= 0)])
  xy <- which(D == dmin, arr.ind = TRUE)
  
  if (nrow(xy) > 1) {
    print("More than one possible coordinate, choosing first one")
  }
  return(list(x = as.numeric(xy[1, 1]), y = as.numeric(xy[1, 2]),use_xy=use_xy))
} # closest_xy
