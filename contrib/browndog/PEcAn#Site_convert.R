#!/usr/bin/Rscript
#PEcAn
#data
#xml
#pecan.zip

# input files is a xml file specifying what to get
#<input>
#  <type>Ameriflux</type>
#  <site>US-Dk3</site>
#  <lat>35.9782</lat>
#  <lon>-79.0942</lon>
#  <start_date>2001-01-01 00:00:00</start_date>
#  <end_date>2001-12-31 23:59:59</end_date>
#</input>

#send all output to stdout (incl stderr)
sink(stdout(), type="message")

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
    allargs <- commandArgs(trailingOnly = FALSE)
    myCommand <- sub('--file=', '', allargs[grep('--file=', allargs)])
    print(paste0("Usage:    ", myCommand, " xml_Input_File  cf-nc_Output_File [tempDirectory] [cacheDirectory]"))
    print(paste0("Example1: ", myCommand, " US-Dk3.xml US-Dk3.pecan.nc [/tmp/watever] [/tmp/cache]"))
    print(paste0("Example2: ", myCommand, " US-Dk3.xml US-Dk3.pecan.zip [/tmp/watever] [/tmp/cache]"))
    q()
}

# load required libraries
require(XML)
require(PEcAn.data.atmosphere)

# 1st argument is the input xml file
input <- xmlToList(xmlParse(args[1]))

# 2nd argument is the output file
outputfile <- args[2]

# 3rd argument is the temp folder
ifelse(length(args) > 2, tempDir <- args[3], tempDir <- ".")

# 4th argument is the cachefolder
ifelse(length(args) > 3, cacheDir <- args[4], cacheDir <- tempDir)
cacheDir <- "/home/polyglot/cache/PEcAn"

# variables definition
sitename   <- input$site
start_date <- input$start_date
end_date   <- input$end_date
model      <- ifelse(is.null(input$model), 'SIPNET', input$model)
mettype    <- ifelse(is.null(input$type), 'Ameriflux', input$type)
site_lat   <- ifelse(is.null(input$lat), NA, input$lat)
site_lon   <- ifelse(is.null(input$lon), NA, input$lon)
overwrite  <- FALSE
verbose    <- FALSE

# download
rawfolder <- file.path(cacheDir, mettype, "raw")
dir.create(rawfolder, showWarnings=FALSE, recursive=TRUE)
if (mettype == "Ameriflux") {
  do.call(paste0("download.", mettype), list(sitename, rawfolder, start_date, end_date, overwrite, verbose))
} else if (mettype == "NARR") {
  do.call(paste0("download.", mettype), list(rawfolder, start_date, end_date, overwrite))
}

# convert to CF
cffolder <- file.path(cacheDir, mettype, "cf")
dir.create(cffolder, showWarnings=FALSE, recursive=TRUE)
do.call(paste0("met2CF.", mettype), list(rawfolder, sitename, cffolder, start_date, end_date, overwrite, verbose))

if (mettype == "Ameriflux") {
  # gapfill
  gapfolder <- file.path(cacheDir, mettype, "gap")
  dir.create(gapfolder, showWarnings=FALSE, recursive=TRUE)
  metgapfill(cffolder, sitename, gapfolder, start_date, end_date, 0, overwrite, verbose)

  folder <- gapfolder
} else if (mettype == "NARR") {
  permutefolder <- file.path(cacheDir, mettype, "permute")
  dir.create(permutefolder, showWarnings=FALSE, recursive=TRUE)
  permute.nc(cffolder, mettype, permutefolder, start_date, end_date, overwrite, verbose)

  sitefolder <- file.path(cacheDir, mettype, "site")
  dir.create(sitefolder, showWarnings=FALSE, recursive=TRUE)
  extract.nc(permutefolder, mettype, sitefolder, sitename, start_date, end_date, site_lat, site_lon, overwrite, verbose)

  folder <- sitefolder
}

# get start/end year code works on whole years only
start_year <- year(start_date)
end_year <- year(end_date)

# if more than 1 year, or zip specified, zip result
if (grepl("\\.zip$", outputfile) || (end_year - start_year > 1)) {
    # get list of files we need to zip
    files <- c()
    for(year in start_year:end_year) {
      files <- c(files, file.path(folder, paste(sitename, year, "nc", sep=".")))
    }

    # use intermediate file so it does not get marked as done until really done
    dir.create(tempDir, showWarnings=FALSE, recursive=TRUE)
    zipfile <- file.path(tempDir, "temp.zip")
    zip(zipfile, files, extras="-j")
    # move file should be fast
    file.rename(zipfile, outputfile)
} else {
    start_year <- year(start_date)
    outfile <- file.path(folder, paste(sitename, start_year, "nc", sep="."))
    file.link(outfile, outputfile)
}
