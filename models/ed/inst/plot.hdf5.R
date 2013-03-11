#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
library(XML)
library(hdf5)

# ----------------------------------------------------------------------
# COMMAND LINE ARGUMENTS
# ----------------------------------------------------------------------
# arguments are --args year variable
args   <- commandArgs(trailingOnly = TRUE)
year   <- args[1]
xvar   <- 'time'
yvar   <- args[2]
width  <- as.numeric(args[3])
height <- as.numeric(args[4])
png    <- args[5]

# ----------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------
settings.file <- Sys.getenv('PECANSETTINGS')
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

plot.hdf5(year, yvar, xvar, width, height, filename, settings) ;

# ----------------------------------------------------------------------
# CONSTANTS
# ----------------------------------------------------------------------
# This depends on NL%FRQFAST in template and should reflect the number of
# data points written per day.
values_day <- 24

umol2gc <- 1.0368

# ----------------------------------------------------------------------
# PRIVATE FUNCTIONS
# ----------------------------------------------------------------------
data.fetch2 <- function(var, start=start_day, end=end_day, values=values_day, fun=mean) {
  # get a specific set of values from the HDF data
  #
  # Args:
  #   var:    the variable to extract from the hdf data
  #   start:  the start time in the array
  #   end:    the end time in the array
  #   values: number of values per day
  #   fun:    the function to apply to the data at the same time
  #
  # Returns:
  #   values extracted from the hdf data
  
  # find the variable in the data
  if (is.null(data[[var]])) {
    useme <- sprintf("AVG_%s", var)
    if (is.null(data[[useme]])) {
      stop(sprintf("Could not find the variable '%s' in the data.", var))
    }
  } else {
    useme <- var
  }
  
  # some precomputations
  lastval  <- (values_day*(1+end-start))
  aggrlist <- list(rep(start:(end), each=values_day))
  
  # aggregate the data
  val <- aggregate(data[[useme]][1:lastval], by=aggrlist, FUN=fun)$x		
  if (length(grep("TE?MP$", useme)) != 0) {
    val[val<200] <- NA
  }
  
  # get the label
  metadata <- attr(data[[useme]], "Metadata")
  if (is.null(metadata)) {
    attr(val, "lbl") <- "Unknown"
  } else {
    title <- gsub(" *$", "", gsub("Long Name: (.*)", "\\1", metadata[pmatch("Long Name:", metadata)]))
    units <- gsub("Units: \\[(.*)\\] *", "\\1", metadata[pmatch("Units:", metadata)])
    if ((title == "") && (units == "")) {
      attr(val, "lbl") <- "Unknown"
    } else if (title == "") {
      attr(val, "lbl") <- paste("Unknown in ", units)
    } else if (units == "") {
      attr(val, "lbl") <- title
    } else {
      attr(val, "lbl") <- paste(title, "in", units)
    }
  }
  
  # done
  return(val)
}

# 
data.fetch <- function(var, start=start_day, end=end_day, values=values_day, fun=mean) {
  # get specific dataset either by computation or from dataset
  #
  # Args:
  #   var:    the variable to extract from the hdf data
  #   start:  the start time in the array
  #   end:    the end time in the array
  #   values: number of values per day
  #   fun:    the function to apply to the data at the same time
  #
  # Returns:
  #   values extracted from the hdf data
  if (var == "time") {
    val <- start:end
    attr(val, "lbl") <- "Day of the year"
    return(val)
  } else if (var == "Reco") {
    PLANT_RESP  <- data.fetch2("AVG_PLANT_RESP", start, end, values, fun);
    HTROPH_RESP <- data.fetch2("AVG_HTROPH_RESP", start, end, values, fun);
    val         <- (PLANT_RESP + HTROPH_RESP) * umol2gc
    attr(val, "lbl") <- "unknown"
    return(val)
  } else if (var == "NPP") {
    GPP         <- data.fetch2("AVG_GPP", start, end, values, fun);
    PLANT_RESP  <- data.fetch2("AVG_PLANT_RESP", start, end, values, fun);
    val         <- (GPP - PLANT_RESP)  * umol2gc
    attr(val, "lbl") <- "unknown"
    return(val)
  } else if (var == "NEE") {
    GPP         <- data.fetch2("AVG_GPP", start, end, values, fun);
    PLANT_RESP  <- data.fetch2("AVG_PLANT_RESP", start, end, values, fun);
    HTROPH_RESP <- data.fetch2("AVG_HTROPH_RESP", start, end, values, fun);
    val         <- (GPP - (PLANT_RESP + HTROPH_RESP))  * umol2gc
    attr(val, "lbl") <- "unknown"
    return(val)
  } else {
    return(data.fetch2(var, start, end, values, fun));
  }
}

# ----------------------------------------------------------------------
# MAIN FUNCTIONS
# ----------------------------------------------------------------------
########################## CREATE PLOT #################################
##' Load the tower dataset and create a plot.
##' @name plot.hdf5
##'
##' \code{plot.hdf5} loads the tower data from an HDF5 file generated by
##' ED and will plot the values against one another. The default is for
##' the given variable to be plotted against time.
##'
##' @param year the specific year to plot.
##' @param yvar the variable to plot along the y-axis.
##' @param xvar the variable to plot along the x-axis, by default time is
##' used.
##' @param the width of the image generated, default is 800 pixels.
##' @param the height of the image generated, default is 600 pixels.
##' @param filename is the name of the file name that is geneated.
##' @param settings the pecan.xml file loaded.
plot.hdf5 <- function(year, yvar, xvar='time', width=800, height=600, filename, settings) { 
	# find out the first/last day of the plot
	start_date <- as.Date(settings$run$start.date)
	start_year <- format(start_date, "%Y")
	end_date <- as.Date(settings$run$end.date)
	end_year <- format(end_date, "%Y")
	if (year == start_year) {
	  start_day <- as.numeric(format(start_date, "%j")) - 1
	} else {
	  start_day <- 0
	}
	if (year == end_year) {
	  end_day <- as.numeric(format(end_date, "%j")) - 1
	} else {
	  end_day <- as.numeric(format(as.Date(sprintf("%s-12-31", year)), "%j")) - 1
	}

	# find the Tower file
	filename <- list.files(settings$run$host$outdir, full.names=TRUE,pattern=paste('.*-T-', year, '-.*.h5', sep=''))[1]
	data <- hdf5load(filename, load = FALSE)

	# compute variables
	xval_mean <- data.fetch(xvar, fun=mean)
	xval_max  <- data.fetch(xvar, fun=max)
	xval_min  <- data.fetch(xvar, fun=min)
	yval_mean <- data.fetch(yvar, fun=mean)
	yval_max  <- data.fetch(yvar, fun=max)
	yval_min  <- data.fetch(yvar, fun=min)

	# setup plot (needs to be done before removing of NA since that removes attr as well).
	png(filename=filename, width=width, height=height)
	plot.new()
	title(xlab=attr(xval_mean, "lbl"))
	title(ylab=attr(yval_mean, "lbl"))
	if (xvar == "time") {
	  title(main=paste(yvar))
	} else {
	  title(main=paste(xvar, "VS", yvar))
	}

	# remove all NA's
	removeme <- unique(c(which(is.na(xval_min)), which(is.na(yval_min)), which(is.na(xval_mean)), which(is.na(yval_mean)), which(is.na(xval_max)), which(is.na(yval_max))))
	if (length(removeme) > 0) {
	  xval_mean <- xval_mean[-removeme]
	  xval_max  <- xval_max[-removeme]
	  xval_min  <- xval_min[-removeme]
	  yval_mean <- yval_mean[-removeme]
	  yval_max  <- yval_max[-removeme]
	  yval_min  <- yval_min[-removeme]
	}

	# combine
	xvals <- c(xval_max, rev(xval_min))
	yvals <- c(yval_max, rev(yval_min))

	# plot actual data
	plot.window(xlim=c(min(xvals), max(xvals)), ylim=c(min(yvals), max(yvals)))
	polygon(c(xval_max, rev(xval_min)), c(yval_max, rev(yval_min)), col="gray", border="black")
	points(xval_mean, yval_mean, col="black", pch=20)

	# draw axis and box
	axis(1)
	axis(2)
	box()
	dev.off()
}

